package diskanalyzer

import java.io.IOException
import java.nio.charset.StandardCharsets
import java.nio.file.{AccessDeniedException, Files, InvalidPathException, NoSuchFileException, Path, Paths}
import java.time.LocalDateTime
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class App(private var config: Config, configPath: Path, logger: Logger) {
  private var rootPath: Option[Path] = None
  private var root: Option[DirNode] = None
  private var current: Option[DirNode] = None
  private val candidates = ArrayBuffer[Candidate]()
  private var nextCandidateId = 1
  private val issues = ArrayBuffer[ScanIssue]()
  private var interrupted = false
  private var staleSettings = false
  private var running = true

  def run(): Unit = {
    Term.println("Анализатор занятого места на диске.")
    Term.println("Программа ничего не удаляет — она только помогает найти крупные каталоги.")
    Term.println("Введите help, чтобы увидеть список команд.")
    while (running) {
      Term.println()
      Term.readLine("> ") match {
        case None => running = false
        case Some(line) =>
          try handle(line.trim)
          catch {
            case e: Exception =>
              Term.println(s"Внутренняя ошибка: $e")
              Term.println("Команда не выполнена, но программа продолжает работу.")
              logger.error(s"Внутренняя ошибка: $e")
          }
      }
    }
  }
  // Разбирает строку на команду и аргумент и вызывает нужный метод
  private def handle(line: String): Unit = {
    val space = line.indexOf(' ')
    val command = (if (space < 0) line else line.substring(0, space)).toLowerCase
    val arg = if (space < 0) "" else line.substring(space + 1).trim

    command match {
      case "" =>
      case "help"=> showHelp()
      case "select"=> cmdSelect(arg)
      case "scan" => cmdScan()
      case "ls" => showCurrent()
      case "open" => cmdOpen(arg)
      case "back" => cmdBack()
      case "rescan" => cmdRescan()
      case "mark"=> cmdMark(arg)
      case "unmark" => cmdUnmark(arg)
      case "candidates" => showCandidates()
      case "errors" => showErrors()
      case "report" => cmdReport()
      case "reload" => cmdReload()
      case "exit" => running = false
      case other =>
        Term.println(s"Неизвестная команда: $other. Введите help, чтобы увидеть список команд.")
        logger.warn(s"Неизвестная команда: $other")
    }
  }
  private def showHelp(): Unit = {
    Term.println(
      """Команды:
        |  select <путь>        выбрать диск или начальный каталог (например: select C:\)
        |  scan                 просканировать выбранный каталог
        |  ls                   показать содержимое текущего каталога
        |  open <имя или номер> перейти во вложенный каталог
        |  back                 вернуться в родительский каталог
        |  rescan               повторно просканировать текущий каталог
        |  mark [имя или номер] отметить каталог как кандидата (без аргумента — текущий)
        |  unmark [номер]       снять отметку (номер из списка candidates; без аргумента — текущий)
        |  candidates           список кандидатов на удаление
        |  errors               объекты, которые не удалось прочитать
        |  report               сохранить отчёт со списком кандидатов
        |  reload               перечитать файл конфигурации
        |  exit                 выход
        |Во время сканирования нажмите Enter, чтобы прервать его.""".stripMargin)
  }
  // Выбор каталога и сканирование
  private def cmdSelect(arg: String): Unit = {
    if (arg.isEmpty) {
      Term.println("Укажите путь, например: select C:\\")
      logger.warn("Некорректные параметры: select без пути")
    } else {
      parsePath(arg) match {
        case Left(error) =>
          Term.println(error)
          logger.warn(s"Некорректные параметры: select $arg")
        case Right(path) if !Files.exists(path) =>
          Term.println(s"Путь не существует: $path")
          Term.println("Проверьте, правильно ли он введён и подключено ли устройство.")
          logger.warn(s"Выбран несуществующий путь: $path")
        case Right(path) if !Files.isDirectory(path) =>
          Term.println(s"Путь не является каталогом: $path")
          Term.println("Укажите каталог или диск, а не файл.")
          logger.warn(s"Выбранный путь не является каталогом: $path")
        case Right(path) if !Files.isReadable(path) =>
          Term.println(s"Каталог недоступен для чтения: $path")
          logger.warn(s"Каталог недоступен для чтения: $path")
        case Right(path) =>
          rootPath = Some(path)
          root = None
          current = None
          issues.clear()
          interrupted = false
          staleSettings = false
          logger.info(s"Выбран начальный каталог: root=$path")
          showDisk(path)
          Term.println()
          Term.println(s"Выбран каталог: $path")
          Term.println("Введите scan, чтобы начать сканирование.")
      }
    }
  }
  //Превращает введённый текст в абсолютный путь
  private def parsePath(text: String): Either[String, Path] = {
    var t = text.stripPrefix("\"").stripSuffix("\"")
    if (t.matches("[A-Za-z]:")) t = t + "\\"
    try Right(Paths.get(t).toAbsolutePath.normalize)
    catch {
      case e: InvalidPathException => Left(s"Некорректный путь: ${e.getReason}")
    }
  }
  private def cmdScan(): Unit = {
    rootPath match {
      case None => needSelect()
      case Some(path) if !Files.isDirectory(path) =>
        Term.println(s"Каталог $path больше недоступен (возможно, устройство отключено).")
        Term.println("Выберите другой каталог командой select.")
        logger.warn(s"Начальный каталог недоступен: $path")
      case Some(path) => logger.info(s"Начато сканирование: root=$path")
        val (node, progress) = runScan(path, None)
        root = Some(node)
        current = Some(node)
        issues.clear()
        issues ++= progress.issues
        interrupted = progress.cancelled.get()
        staleSettings = false
        afterScan(node, progress)
        showCurrent()
    }
  }
  private def cmdRescan(): Unit = {
    requireScan().foreach { cur =>
      if (!Files.isDirectory(cur.path)) {
        Term.println(s"Каталог больше не существует: ${cur.path}")
        Term.println("Вернитесь назад командой back и выполните rescan там.")
        logger.warn(s"Повторное сканирование невозможно, каталог не существует: ${cur.path}")
      } else {
        logger.info(s"Повторное сканирование: ${cur.path}")
        current = Some(rescanNode(cur))
        showCurrent()
      }
    }
  }
  // Сканирует каталог заново и заменяет старый узел в дереве новым
  private def rescanNode(old: DirNode): DirNode = {
    val (node, progress) = runScan(old.path, old.parent)
    old.parent match {
      case Some(parent) =>
        val index = parent.children.indexOf(old)
        if (index >= 0) parent.children(index) = node else parent.children += node
        node.recalcParents()
      case None =>
        root = Some(node) // пересканировали начальный каталог целиком
    }
    // старые ошибки из этого каталога больше не актуальны
    issues.filterInPlace(i => !i.path.startsWith(old.path))
    issues ++= progress.issues

    if (progress.cancelled.get()) interrupted = true
    else if (node.parent.isEmpty) {
      interrupted = false
      staleSettings = false
    }
    afterScan(node, progress)
    node
  }
  //Запускает сканирование в отдельном потоке
  private def runScan(path: Path, parent: Option[DirNode]): (DirNode, ScanProgress) = {
    val progress = new ScanProgress
    val scanner = new Scanner(config, progress)
    var result: DirNode = null
    var failure: Throwable = null
    val thread = new Thread(() => {
      try result = scanner.scan(path, parent)
      catch { case e: Throwable => failure = e }
    })
    thread.setDaemon(true)
    Term.println(s"Сканирование $path")
    Term.println("(нажмите Enter, чтобы прервать)")
    thread.start()
    var tick = 0
    while (thread.isAlive) {
      thread.join(200)
      if (thread.isAlive) {
        Term.showProgress(progressLine(progress, tick))
        tick += 1
        if (Term.inputReady) {
          val input = Term.readLine("").getOrElse("").trim.toLowerCase
          if (input.isEmpty || input == "q" || input == "stop") progress.cancelled.set(true)
        }
      }
    }
    Term.clearProgress()
    if (failure != null) throw new RuntimeException(s"ошибка при сканировании: $failure", failure)
    (result, progress)
  }
  private def progressLine(p: ScanProgress, tick: Int): String = {
    val head =
      if (p.topTotal > 0) {
        val filled = showable(config.progressFilledCharacter, "#")
        val empty = showable(config.progressEmptyCharacter, ".")
        val bar = Format.progressBar(p.topDone, p.topTotal, 20, filled, empty)
        s"$bar ${p.topDone * 100 / p.topTotal} %"
      } else {
        val frames = List("[|]", "[/]", "[-]", "[\\]")
        frames(tick % frames.size) + " Сканирование"
      }
    val counts = s"каталогов: ${Format.number(p.dirs)}, файлов: ${Format.number(p.files)}"
    s"$head | $counts | ${Format.shortenStart(p.currentDir, 45)}"
  }
  //Общие действия после любого сканирования: журнал, сводка, кэш, кандидаты
  private def afterScan(node: DirNode, p: ScanProgress): Unit = {
    val seconds = (System.currentTimeMillis() - p.startedAt) / 1000.0
    val counts = s"files=${p.files}, directories=${p.dirs}"
    if (p.cancelled.get()) logger.warn(s"Сканирование прервано пользователем: $counts")
    else logger.info(s"Сканирование завершено: $counts")
    if (p.failedFiles + p.failedDirs > 0)
      logger.warn(s"Недоступные объекты: files=${p.failedFiles}, directories=${p.failedDirs}")
    p.issues.take(100).foreach(i => logger.warn(s"Ошибка чтения (${i.kind}): ${i.path} — ${i.reason}"))
    if (p.issues.size > 100) logger.warn(s"... и ещё ${p.issues.size - 100} ошибок чтения")
    //сводка для пользователя
    Term.println()
    if (p.cancelled.get()) {
      Term.println("Сканирование ПРЕРВАНО пользователем.")
      Term.println("Результаты НЕПОЛНЫЕ: часть каталогов не просканирована, реальные размеры могут быть больше.")
    } else if (p.issues.nonEmpty) Term.println("Сканирование завершено с предупреждениями.")
    else Term.println("Сканирование завершено.")
    Term.println()
    Term.println(s"Обработано файлов: ${Format.number(p.files)}")
    Term.println(s"Обработано каталогов: ${Format.number(p.dirs)}")
    Term.println(s"Не удалось прочитать файлов: ${Format.number(p.failedFiles)}")
    Term.println(s"Не удалось открыть каталогов: ${Format.number(p.failedDirs)}")
    Term.println(s"Каталогов с неполным размером: ${Format.number(node.countIncomplete)}")
    Term.println(s"Время: ${Format.decimal(seconds)} с")
    if (p.issues.nonEmpty) {
      Term.println()
      Term.println("Не удалось полностью просканировать:")
      p.issues.take(5).foreach(i => Term.println(s"  ${i.path}\n    Причина: ${i.reason}."))
      if (p.issues.size > 5) Term.println(s"  ... и ещё ${p.issues.size - 5}. Полный список — команда errors.")
      Term.println("Размеры родительских каталогов являются неполными. Сканирование остальных каталогов продолжено.")
    }
    applyCacheLimit()
    refreshCandidates()
  }
  // Просмотр и навигация
  private def showDisk(path: Path): Unit = {
    try {
      val store = Files.getFileStore(path)
      val total = store.getTotalSpace
      val free = store.getUsableSpace
      Term.println(s"Диск: ${path.getRoot}")
      Term.println(s"Всего: ${sizeText(total)}")
      Term.println(s"Занято: ${sizeText(total - free)}")
      Term.println(s"Свободно: ${sizeText(free)}")
    } catch {
      case e: IOException => Term.println(s"Не удалось получить сведения о диске: ${Scanner.reason(e)}")
    }
  }
  //Показывает текущий каталог
  private def showCurrent(): Unit = {
    requireScan().foreach { cur =>
      rootPath.foreach(showDisk)
      Term.println()
      if (interrupted)
        Term.println("ВНИМАНИЕ: сканирование было прервано, результаты неполные.")
      if (staleSettings)
        Term.println("ВНИМАНИЕ: результаты получены по старым настройкам обхода. Выполните scan заново.")
      Term.println(s"Текущий каталог: ${cur.path}")
      Term.println(s"Размер каталога: ${nodeSizeText(cur)}" + (if (cur.incomplete) "   [неполные данные]" else ""))
      Term.println(s"Внутри: файлов ${Format.number(cur.totalFiles)}, каталогов ${Format.number(cur.totalDirs)}")
      if (isMarked(cur.path)) Term.println("Текущий каталог отмечен как кандидат на удаление.")
      Term.println()
      val visible = visibleChildren(cur)
      if (visible.isEmpty) Term.println("Вложенных каталогов для отображения нет.")
      else {
        Term.println("Каталоги:")
        Term.println()
        printDirTable(cur, visible)
      }
      val notShown = cur.children.filterNot(visible.contains)
      if (notShown.nonEmpty) {
        Term.println()
        Term.println(s"Не показано каталогов: ${notShown.size} (скрытые или меньше " +
          s"${sizeText(config.minimumDisplayedSizeBytes)}), их размер ${sizeText(notShown.map(_.totalSize).sum)} " +
          "учтён в размере текущего каталога.")
      }
      Term.println()
      Term.println(s"Файлы текущего каталога: ${sizeText(cur.filesSize)} (${Format.number(cur.filesCount)} шт.)")
    }
  }
  //Таблица
  private def printDirTable(cur: DirNode, list: Seq[DirNode]): Unit = {
    val barSymbol = showable(config.barCharacter, "#")
    val maxSize = list.map(_.totalSize).max
    val names = list.map(n => Format.shortenEnd(n.name, 40))
    val sizes = list.map(nodeSizeText)
    val percents = list.map(n => Format.percent(n.totalSize, cur.totalSize))
    val nameWidth = names.map(_.length).max
    val sizeWidth = sizes.map(_.length).max
    val percentWidth = percents.map(_.length).max
    val numberWidth = list.size.toString.length
    for (i <- list.indices) {
      val node = list(i)
      val bar = Format.bar(node.totalSize, maxSize, config.barWidth, barSymbol)
      var line = Format.padLeft((i + 1).toString, numberWidth) + ". " +
        names(i).padTo(nameWidth, ' ') + "   " +
        Format.padLeft(sizes(i), sizeWidth) + "   " +
        bar + "  " +
        Format.padLeft(percents(i), percentWidth)
      if (isMarked(node.path)) line += "  [кандидат]"
      if (node.incomplete) line += "  [неполные данные]"
      Term.println(line)
    }
  }
  //Вложенные каталоги, которые нужно показать в нужном порядке
  private def visibleChildren(node: DirNode): Seq[DirNode] = {
    val shown = node.children.toSeq.filter { c =>
      (config.showHiddenDirectories || !c.hidden) && c.totalSize >= config.minimumDisplayedSizeBytes
    }
    config.defaultSort match {
      case "sizeAscending"  => shown.sortBy(_.totalSize)
      case "nameAscending"  => shown.sortBy(_.name.toLowerCase)
      case "nameDescending" => shown.sortBy(_.name.toLowerCase).reverse
      case _ => shown.sortBy(c => -c.totalSize) // sizeDescending
    }
  }
  // Ищет вложенный каталог по номеру из списка или по имени
  private def findChild(cur: DirNode, arg: String): Option[DirNode] = {
    val byNumber = arg.toIntOption.flatMap(n => visibleChildren(cur).lift(n - 1))
    val found = byNumber
      .orElse(cur.children.find(_.name == arg))
      .orElse(cur.children.find(_.name.equalsIgnoreCase(arg)))
    if (found.isEmpty) {
      Term.println(s"Вложенный каталог \"$arg\" не найден в ${cur.path}.")
      Term.println("Введите ls, чтобы увидеть список, и укажите имя или номер каталога.")
      logger.warn(s"Некорректные параметры: вложенный каталог не найден: $arg")
    }
    found
  }
  private def cmdOpen(arg: String): Unit = {
    requireScan().foreach { cur =>
      if (arg.isEmpty) {
        Term.println("Укажите имя или номер каталога, например: open 1")
        logger.warn("Некорректные параметры: open без аргумента")
      } else if (arg == "..") cmdBack()
      else {
        findChild(cur, arg).foreach { child =>
          logger.info(s"Переход в каталог: ${child.path}")
          current = Some(child)
          if (!child.childrenInMemory) {
            Term.println("Сведения о вложенных каталогах не хранятся в памяти " +
              "(ограничение maximumCachedDirectories). Каталог будет просканирован заново.")
            current = Some(rescanNode(child))
          }
          showCurrent()
        }
      }
    }
  }
  private def cmdBack(): Unit = {
    requireScan().foreach { cur =>
      cur.parent match {
        case None =>
          Term.println(s"Вы находитесь в начальном каталоге ${cur.path}. Подняться выше нельзя.")
          Term.println("Чтобы выбрать другой каталог, используйте select.")
          logger.warn("Попытка перейти выше начального каталога")
        case Some(parent) =>
          logger.info(s"Переход в родительский каталог: ${parent.path}")
          current = Some(parent)
          showCurrent()
      }
    }
  }

  //Ограничение maximumCachedDirectories: храним в памяти не больше N каталогов.
  private def applyCacheLimit(): Unit = {
    root.foreach { r =>
      val limit = config.maximumCachedDirectories
      val before = r.countNodes
      if (before > limit) {
        var kept = 1
        val queue = mutable.Queue(r)
        while (queue.nonEmpty) {
          val node = queue.dequeue()
          val onCurrentPath = current.exists(c => c.path.startsWith(node.path))
          if (node.children.nonEmpty) {
            if (kept + node.children.size <= limit || onCurrentPath) {
              kept += node.children.size
              queue ++= node.children
            } else {
              node.children.clear()
              node.childrenInMemory = false
            }
          }
        }
        Term.println(s"В памяти сохранено каталогов: ${Format.number(kept)} из ${Format.number(before)} " +
          s"(лимит maximumCachedDirectories = $limit). Остальные будут просканированы при переходе в них.")
        logger.info(s"Применён лимит кэша: kept=$kept, total=$before")
      }
    }
  }
  // Кандидаты на удаление
  private def cmdMark(arg: String): Unit = {
    requireScan().foreach { cur =>
      val target = if (arg.isEmpty) Some(cur) else findChild(cur, arg)
      target.foreach(addCandidate)
    }
  }
  private def addCandidate(node: DirNode): Unit = {
    if (isMarked(node.path)) {
      Term.println(s"Каталог уже отмечен как кандидат: ${node.path}")
      logger.warn("Попытка повторно отметить кандидата")
    } else {
      val overlaps = Candidates.overlapping(node.path, candidates.toSeq)
      var confirmed = true
      if (overlaps.nonEmpty) {
        for (other <- overlaps) {
          val relation =
            if (node.path.startsWith(other.path)) "уже входит в отмеченный каталог"
            else "содержит уже отмеченный каталог"
          Term.println(s"\nКаталог\n\n${node.path}\n\n$relation\n\n${other.path}\n")
        }
        logger.warn(s"Обнаружено пересечение кандидатов: пересечений=${overlaps.size}")
        val question = if (overlaps.size == 1) "Оставить обе отметки?" else "Оставить все отметки?"
        confirmed = askYesNo(s"$question [да/нет]: ")
      }

      if (!confirmed) Term.println("Отметка не добавлена.")
      else {
        val comment = Term.readLine("Комментарий (Enter — без комментария): ").getOrElse("").trim
        val c = new Candidate(nextCandidateId, node.path, node.totalSize, node.scannedAt, !node.incomplete, comment)
        nextCandidateId += 1
        candidates += c
        Term.println(s"Отмечен кандидат: ${node.path} (${nodeSizeText(node)})")
        logger.info(s"Добавлен кандидат: candidateId=${c.id}, sizeBytes=${c.sizeBytes}")
      }
    }
  }

  private def cmdUnmark(arg: String): Unit = {
    if (candidates.isEmpty) Term.println("Список кандидатов пуст.")
    else {
      val target: Option[Candidate] =
        if (arg.isEmpty) current.flatMap(cur => candidates.find(_.path == cur.path))
        else
          arg.toIntOption match {
            case Some(n) => candidates.lift(n - 1)
            case None => // имя вложенного каталога
              current
                .flatMap(cur => cur.children.find(_.name.equalsIgnoreCase(arg)))
                .flatMap(child => candidates.find(_.path == child.path))
          }

      target match {
        case None =>
          Term.println("Кандидат не найден. Укажите номер из списка candidates, например: unmark 1")
          logger.warn(s"Некорректные параметры: unmark $arg")
        case Some(c) =>
          candidates -= c
          Term.println(s"Отметка снята: ${c.path}")
          logger.info(s"Удалён кандидат: candidateId=${c.id}")
      }
    }
  }

  private def showCandidates(): Unit = {
    if (candidates.isEmpty) {
      Term.println("Список кандидатов пуст. Отметить каталог можно командой mark.")
    } else {
      Term.println("Кандидаты на удаление:")
      val list = candidates.toSeq
      for ((c, i) <- list.zipWithIndex) {
        Term.println()
        Term.println(s"${i + 1}. ${c.path}")
        Term.println(s"   Размер: ${candidateSizeText(c)}")
        Term.println(s"   Состояние: ${candidateState(c)}")
        Term.println(s"   Просканирован: ${Format.dateTime(c.scannedAt)}")
        if (c.comment.nonEmpty) Term.println(s"   Комментарий: ${c.comment}")
        Candidates.parentOf(c, list).foreach { p =>
          Term.println(s"   Входит в отмеченный каталог №${list.indexOf(p) + 1} — место не учитывается дважды")
        }
      }
      val s = Candidates.summary(list)
      val atLeast = if (s.freeableIncomplete) "не менее " else ""
      Term.println()
      Term.println(s"Сумма размеров всех отмеченных каталогов: ${sizeText(s.totalSize)}")
      Term.println(s"Потенциально освобождаемое место (без двойного учёта): $atLeast${sizeText(s.freeable)}")
      Term.println(s"Просканированы полностью: ${s.completeCount}, неполностью: ${s.incompleteCount}, " +
        s"больше не существуют: ${s.missingCount}")
    }
  }

  //После сканирования обновляет размеры кандидатов и проверяет существуют ли они
  private def refreshCandidates(): Unit = {
    for (c <- candidates) {
      root.flatMap(_.find(c.path)).foreach { node =>
        c.sizeBytes = node.totalSize
        c.scannedAt = node.scannedAt
        c.complete = !node.incomplete
      }
      if (!c.exists) {
        Term.println(s"Внимание: отмеченный каталог больше не существует: ${c.path}")
        logger.warn(s"Кандидат больше не существует: candidateId=${c.id}")
      }
    }
  }

  private def candidateSizeText(c: Candidate): String =
    if (c.complete) sizeText(c.sizeBytes) else s"не менее ${sizeText(c.sizeBytes)}"
  private def candidateState(c: Candidate): String =
    if (!c.exists) "каталог больше не существует"
    else if (c.complete) "просканирован полностью"
    else "часть файлов недоступна"
  private def isMarked(path: Path): Boolean = candidates.exists(_.path == path)
  //Ошибки, отчёт, конфигурация
  private def showErrors(): Unit = {
    if (issues.isEmpty) Term.println("Ошибок при сканировании не было.")
    else {
      Term.println(s"Не удалось обработать объектов: ${issues.size}")
      Term.println()
      issues.foreach(i => Term.println(s"[${i.kind}] ${i.path}\n    Причина: ${i.reason}"))
    }
  }

  private def cmdReport(): Unit = {
    val text = Candidates.reportText(candidates.toSeq, config.sizeUnits, LocalDateTime.now())
    try {
      val file = Paths.get(config.reportFile).toAbsolutePath
      Files.write(file, text.getBytes(StandardCharsets.UTF_8))
      Term.println(s"Отчёт сохранён: $file")
      Term.println(s"Кандидатов в отчёте: ${candidates.size}")
      val missing = candidates.count(!_.exists)
      if (missing > 0) Term.println(s"Из них больше не существуют: $missing")
      logger.info(s"Сформирован отчёт: candidates=${candidates.size}")
    } catch {
      case e: Exception =>
        val reason = e match {
          case e: InvalidPathException  => s"некорректный путь (${e.getReason})"
          case _: NoSuchFileException => "папка, в которой должен лежать отчёт, не существует"
          case _: AccessDeniedException => "нет прав на запись в этот файл"
          case e: IOException  => Scanner.reason(e)
          case e => e.toString
        }
        Term.println(s"Не удалось записать отчёт в файл \"${config.reportFile}\": $reason.")
        Term.println("Проверьте параметр reportFile в конфигурации и права на запись, затем выполните reload и report.")
        logger.error(s"Не удалось сформировать отчёт: $reason")
    }
  }

  private def cmdReload(): Unit = {
    logger.info("Перезагрузка конфигурации")
    Config.load(configPath) match {
      case Left(errors) =>
        Term.println("Не удалось перезагрузить конфигурацию:")
        errors.foreach(e => Term.println(s"  - $e"))
        Term.println("Продолжает использоваться прежняя конфигурация.")
        errors.foreach(e => logger.error(s"Ошибка конфигурации: $e"))

      case Right(newConfig) =>
        val old = config
        config = newConfig
        if (old.logFile != newConfig.logFile) logger.open(newConfig.logFile)
        Term.println("Конфигурация перезагружена. Новые настройки отображения и отчёта уже действуют.")
        logger.info("Конфигурация успешно перезагружена")

        if (root.isDefined && old.followSymbolicLinks != newConfig.followSymbolicLinks) {
          staleSettings = true
          Term.println("ВНИМАНИЕ: изменён параметр followSymbolicLinks. Текущие результаты получены по старым " +
            "настройкам — выполните scan, чтобы применить новые правила обхода.")
          logger.warn("Результаты сканирования получены по старым настройкам обхода")
        }
        if (root.isDefined && (old.showHiddenDirectories != newConfig.showHiddenDirectories ||
            old.minimumDisplayedSizeBytes != newConfig.minimumDisplayedSizeBytes)) {
          Term.println("Изменены showHiddenDirectories / minimumDisplayedSizeBytes. Размеры скрытых и мелких " +
            "каталогов всегда учитываются при сканировании, поэтому изменения применены сразу.")
        }
        if (old.maximumCachedDirectories != newConfig.maximumCachedDirectories) applyCacheLimit()
    }
  }
  //Проверяет, что каталог выбран и просканирован. Возвращает текущий каталог
  private def requireScan(): Option[DirNode] = {
    if (rootPath.isEmpty) {
      needSelect()
      None
    } else if (current.isEmpty) {
      Term.println("Каталог ещё не просканирован. Выполните команду scan.")
      logger.warn("Команда недоступна до сканирования")
      None
    } else current
  }

  private def needSelect(): Unit = {
    Term.println("Сначала выберите диск или каталог командой select, например: select C:\\")
    logger.warn("Команда недоступна до выбора начального каталога")
  }

  private def askYesNo(question: String): Boolean = {
    var answer: Option[Boolean] = None
    while (answer.isEmpty) {
      Term.readLine(question).map(_.trim.toLowerCase) match {
        case None => answer = Some(false)
        case Some("да" | "д" | "yes" | "y") => answer = Some(true)
        case Some("нет" | "н" | "no" | "n") => answer = Some(false)
        case Some(_) => Term.println("Ответьте «да» или «нет».")
      }
    }
    answer.get
  }
  private def sizeText(bytes: Long): String = Format.size(bytes, config.sizeUnits)
  // Размер каталога, если данные неполные
  private def nodeSizeText(node: DirNode): String =
    (if (node.incomplete) Term.geq + " " else "") + sizeText(node.totalSize)
  //Символ из конфигурации или замена, если консоль не может его показать
  private def showable(symbol: String, replacement: String): String =
    if (Term.canShow(symbol)) symbol else replacement
}
