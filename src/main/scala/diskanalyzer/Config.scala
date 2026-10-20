package diskanalyzer

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success, Try}
// Настройки приложения
case class Config(
    logFile: String,
    reportFile: String,
    barWidth: Int,
    barCharacter: String,
    progressFilledCharacter: String,
    progressEmptyCharacter: String,
    sizeUnits: String,
    defaultSort: String,
    followSymbolicLinks: Boolean,
    showHiddenDirectories: Boolean,
    maximumCachedDirectories: Int,
    minimumDisplayedSizeBytes: Long
)
object Config {
  val SizeUnits: List[String] = List("automatic", "bytes", "kilobytes", "megabytes", "gigabytes")
  val SortOrders: List[String] = List("sizeDescending", "sizeAscending", "nameAscending", "nameDescending")
  val DefaultLogFile = "disk-analyzer.log"
  val DefaultJson: String =
    """{
      |  "logFile": "disk-analyzer.log",
      |  "reportFile": "cleanup-candidates.txt",
      |  "barWidth": 20,
      |  "barCharacter": "#",
      |  "progressFilledCharacter": "#",
      |  "progressEmptyCharacter": "-",
      |  "sizeUnits": "automatic",
      |  "defaultSort": "sizeDescending",
      |  "followSymbolicLinks": false,
      |  "showHiddenDirectories": true,
      |  "maximumCachedDirectories": 10000,
      |  "minimumDisplayedSizeBytes": 0
      |}
      |""".stripMargin
  // Читает и проверяет файл конфигурации. Left — список найденных ошибок
  def load(path: Path): Either[List[String], Config] = {
    if (!Files.exists(path)) Left(List(s"Файл конфигурации не найден: ${path.toAbsolutePath}"))
    else
      Try(new String(Files.readAllBytes(path), StandardCharsets.UTF_8)) match {
        case Failure(e) => Left(List(s"Не удалось прочитать файл конфигурации: ${e.getMessage}"))
        case Success(text) => parse(text)
      }
  }
  def parse(text: String): Either[List[String], Config] = {
    Try(ujson.read(text.stripPrefix("\uFEFF"))) match {
      case Failure(e) =>
        Left(List(s"Конфигурационный файл имеет неправильный формат (ожидается JSON): ${e.getMessage}"))
      case Success(obj: ujson.Obj) =>
        fromFields(obj.value)
      case Success(_) =>
        Left(List("Конфигурационный файл должен содержать JSON-объект в фигурных скобках { ... }"))
    }
  }
  private def fromFields(fields: collection.Map[String, ujson.Value]): Either[List[String], Config] = {
    val errors = ListBuffer[String]()
    // Вспомогательные функции: достают значение нужного типа или запоминают ошибку
    def getString(key: String, default: String): String = fields.get(key) match {
      case Some(ujson.Str(s)) => s
      case None | Some(ujson.Null) => default
      case Some(_) =>
        errors += s"Параметр \"$key\" должен быть строкой (в кавычках)"
        default
    }
    def getLong(key: String, default: Long): Long = fields.get(key) match {
      case Some(ujson.Num(n)) if n.isWhole => n.toLong
      case None | Some(ujson.Null) => default
      case Some(_) =>
        errors += s"Параметр \"$key\" должен быть целым числом"
        default
    }
    def getBoolean(key: String, default: Boolean): Boolean = fields.get(key) match {
      case Some(ujson.Bool(b)) => b
      case None | Some(ujson.Null) => default
      case Some(_) =>
        errors += s"Параметр \"$key\" должен быть true или false"
        default
    }
    val config = Config(
      logFile = getString("logFile", ""),
      reportFile = getString("reportFile", ""),
      barWidth = getLong("barWidth", 20).toInt,
      barCharacter = getString("barCharacter", "#"),
      progressFilledCharacter = getString("progressFilledCharacter", "#"),
      progressEmptyCharacter = getString("progressEmptyCharacter", "-"),
      sizeUnits = getString("sizeUnits", "automatic"),
      defaultSort = getString("defaultSort", "sizeDescending"),
      followSymbolicLinks = getBoolean("followSymbolicLinks", false),
      showHiddenDirectories = getBoolean("showHiddenDirectories", true),
      maximumCachedDirectories = getLong("maximumCachedDirectories", 10000).toInt,
      minimumDisplayedSizeBytes = getLong("minimumDisplayedSizeBytes", 0)
    )
    if (config.logFile.trim.isEmpty)
      errors += "Не указан путь к файлу журнала (logFile)"
    if (config.reportFile.trim.isEmpty)
      errors += "Не указан путь к файлу отчёта (reportFile)"
    if (config.barWidth < 1)
      errors += s"Длина полосы (barWidth) должна быть не меньше 1, указано: ${config.barWidth}"
    if (config.barWidth > 200)
      errors += s"Длина полосы (barWidth) слишком большая (максимум 200), указано: ${config.barWidth}"
    if (config.barCharacter.isEmpty)
      errors += "Символ полосы (barCharacter) не может быть пустой строкой"
    if (config.progressFilledCharacter.isEmpty)
      errors += "Символ заполненной части индикатора (progressFilledCharacter) не может быть пустым"
    if (config.progressEmptyCharacter.isEmpty)
      errors += "Символ незаполненной части индикатора (progressEmptyCharacter) не может быть пустым"
    if (!SizeUnits.contains(config.sizeUnits))
      errors += s"Неизвестный способ отображения единиц (sizeUnits): \"${config.sizeUnits}\". Допустимо: ${SizeUnits.mkString(", ")}"
    if (!SortOrders.contains(config.defaultSort))
      errors += s"Неизвестный порядок сортировки (defaultSort): \"${config.defaultSort}\". Допустимо: ${SortOrders.mkString(", ")}"
    if (config.maximumCachedDirectories < 1)
      errors += s"maximumCachedDirectories должно быть не меньше 1, указано: ${config.maximumCachedDirectories}"
    if (config.minimumDisplayedSizeBytes < 0)
      errors += s"minimumDisplayedSizeBytes не может быть отрицательным, указано: ${config.minimumDisplayedSizeBytes}"
    if (errors.isEmpty) Right(config) else Left(errors.toList)
  }
}
