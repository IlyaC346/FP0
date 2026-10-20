package diskanalyzer

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.util.Locale

// Форматирование чисел, размеров и полос
object Format {
  private val ru = Locale.forLanguageTag("ru-RU")
  private val dateFormat = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm")
  private val KB  = 1024.0
  private val MB = KB * 1024
  private val GB = MB * 1024
  def size(bytes: Long, units: String): String = units match {
    case "bytes" => s"${number(bytes)} Б"
    case "kilobytes" => s"${decimal(bytes / KB)} КБ"
    case "megabytes" => s"${decimal(bytes / MB)} МБ"
    case "gigabytes" => s"${decimal(bytes / GB)} ГБ"
    case _ => automatic(bytes)
  }
  //Автоматический выбор единицы
  private def automatic(bytes: Long): String = {
    val names = List("Б", "КБ", "МБ", "ГБ", "ТБ")
    var value = bytes.toDouble
    var i = 0
    while (value >= 1024 && i < names.size - 1) {
      value = value / 1024
      i += 1
    }
    if (i == 0) s"$bytes Б" else s"${decimal(value)} ${names(i)}"
  }
  def number(n: Long): String = cleanSpaces("%,d".formatLocal(ru, n))
  def decimal(d: Double): String = cleanSpaces("%,.1f".formatLocal(ru, d))
  def percent(part: Long, whole: Long): String =
    if (whole <= 0) "0,0 %" else decimal(part * 100.0 / whole) + " %"
  private def cleanSpaces(s: String): String = s.replace('\u00A0', ' ').replace('\u202F', ' ')
  def bar(size: Long, maxSize: Long, width: Int, symbol: String): String = {
    val length =
      if (size <= 0 || maxSize <= 0) 0
      else math.max(1, math.round(size.toDouble * width / maxSize).toInt)
    symbol * length + " " * (width - length)
  }
  def progressBar(done: Int, total: Int, width: Int, filled: String, empty: String): String = {
    val length = if (total <= 0) 0 else math.min(width, done * width / total)
    "[" + filled * length + empty * (width - length) + "]"
  }
  def dateTime(t: LocalDateTime): String = t.format(dateFormat)
  // Дополняет строку пробелами слева до нужной ширины 
  def padLeft(s: String, width: Int): String = " " * (width - s.length) + s
  def shortenStart(s: String, max: Int): String =
    if (s.length <= max) s else "..." + s.takeRight(max - 3)
  def shortenEnd(s: String, max: Int): String =
    if (s.length <= max) s else s.take(max - 3) + "..."
}
