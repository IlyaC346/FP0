package diskanalyzer

import java.io.IOException
import java.nio.file._
import java.nio.file.attribute.BasicFileAttributes
import java.util.concurrent.atomic.AtomicBoolean
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters._
import scala.util.{Failure, Success, Try}


case class ScanIssue(path: Path, kind: String, reason: String)
//Сканер обновляет эти поля в фоновом потоке, а основной поток читает их, чтобы рисовать индикатор.
class ScanProgress {
  @volatile var dirs = 0L
  @volatile var files = 0L
  @volatile var failedDirs = 0L
  @volatile var failedFiles = 0L
  @volatile var currentDir = ""
  @volatile var topTotal = 0 
  @volatile var topDone = 0 
  val cancelled = new AtomicBoolean(false)
  val issues: ArrayBuffer[ScanIssue] = ArrayBuffer()
  val startedAt: Long = System.currentTimeMillis()
}
object Scanner {
  def reason(e: Throwable): String = e match {
    case _: AccessDeniedException => "недостаточно прав доступа"
    case _: NoSuchFileException => "объект исчез во время сканирования (или устройство отключено)"
    case _: FileSystemLoopException => "циклическая ссылка"
    case _: NotDirectoryException => "объект больше не является каталогом"
    case e: FileSystemException if e.getReason != null => s"ошибка файловой системы: ${e.getReason}"
    case _: SecurityException => "недостаточно прав доступа"
    case e => s"ошибка чтения: ${e.getMessage}"
  }
}
// Рекурсивно обходит каталоги и строит дерево DirNode
class Scanner(config: Config, progress: ScanProgress) {
  private val chain = mutable.HashSet[String]()
  def scan(dir: Path, parent: Option[DirNode]): DirNode = {
    if (config.followSymbolicLinks) {
      var p = parent
      while (p.isDefined) {
        realPath(p.get.path).foreach(chain.add)
        p = p.get.parent
      }
    }
    scanDir(dir, parent, isTop = true)
  }
  private def scanDir(dir: Path, parent: Option[DirNode], isTop: Boolean): DirNode = {
    val node = new DirNode(dir, parent)
    node.hidden = Try(Files.isHidden(dir)).getOrElse(false)
    progress.dirs += 1
    progress.currentDir = dir.toString
    val real = if (config.followSymbolicLinks) realPath(dir) else None
    if (real.exists(chain.contains)) {
      progress.failedDirs += 1
      addIssue(dir, "каталог", "циклическая ссылка: ведёт в один из родительских каталогов")
      node.ownProblems = true
    } else {
      real.foreach(chain.add)
      readDirectory(dir) match {
        case Left(reason) =>
          progress.failedDirs += 1
          addIssue(dir, "каталог", reason)
          node.ownProblems = true
        case Right(entries) =>
          // сначала обрабатываем файлы и собираем список подкаталогов
          val subdirs = ArrayBuffer[Path]()
          entries.foreach(entry => processEntry(entry, node, subdirs))
          if (isTop) progress.topTotal = subdirs.size
          for (sub <- subdirs) {
            if (progress.cancelled.get()) {
              val skipped = new DirNode(sub, Some(node))
              skipped.ownProblems = true
              skipped.recalc()
              node.children += skipped
            } else {
              node.children += scanDir(sub, Some(node), isTop = false)
              if (isTop) progress.topDone += 1
            }
          }
      }
      real.foreach(chain.remove) 
    }
    node.recalc()
    node
  }

  //Читает список содержимого каталога
  private def readDirectory(dir: Path): Either[String, List[Path]] = {
    var stream: DirectoryStream[Path] = null
    try {
      stream = Files.newDirectoryStream(dir)
      Right(stream.asScala.toList)
    } catch {
      case e: DirectoryIteratorException => Left(Scanner.reason(e.getCause))
      case e: IOException => Left(Scanner.reason(e))
      case e: SecurityException => Left(Scanner.reason(e))
    } finally {
      if (stream != null) Try(stream.close())
    }
  }
  // Обрабатывает один элемент каталога
  private def processEntry(entry: Path, node: DirNode, subdirs: ArrayBuffer[Path]): Unit = {
    try {
      val attrs = Files.readAttributes(entry, classOf[BasicFileAttributes], LinkOption.NOFOLLOW_LINKS)
      if (isLink(entry, attrs)) {
        if (config.followSymbolicLinks) {
          // смотрим, на что указывает ссылка
          Try(Files.readAttributes(entry, classOf[BasicFileAttributes])) match {
            case Success(target) if target.isDirectory => subdirs += entry
            case Success(target) => addFile(node, target.size)
            case Failure(_) =>
              progress.failedFiles += 1
              node.ownProblems = true
              addIssue(entry, "ссылка", "ссылка указывает на несуществующий или недоступный объект")
          }
        }
      } else if (attrs.isDirectory) {
        subdirs += entry
      } else {
        addFile(node, attrs.size)
      }
    } catch {
      case e: IOException =>
        progress.failedFiles += 1
        node.ownProblems = true
        addIssue(entry, "файл", Scanner.reason(e))
    }
  }
  private def addFile(node: DirNode, size: Long): Unit = {
    node.filesSize += size
    node.filesCount += 1
    progress.files += 1
  }
  private def isLink(entry: Path, attrs: BasicFileAttributes): Boolean = {
    if (attrs.isSymbolicLink) true
    else if (attrs.isDirectory && attrs.isOther) {
      Try(entry.toRealPath()) match {
        case Success(real) => !real.toString.equalsIgnoreCase(entry.toAbsolutePath.toString)
        case Failure(_)    => true
      }
    } else false
  }
  private def realPath(dir: Path): Option[String] =
    Try(dir.toRealPath().toString.toLowerCase).toOption
  private def addIssue(path: Path, kind: String, reason: String): Unit =
    progress.issues += ScanIssue(path, kind, reason)
}
