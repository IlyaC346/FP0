package diskanalyzer

import java.io.{FileOutputStream, OutputStreamWriter, PrintWriter}
import java.nio.charset.StandardCharsets
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

// Журнал работы программы,записи дописываются в конец файла
class Logger(initialPath: String) {
  private val timeFormat = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")
  private var writer: Option[PrintWriter] = None
  open(initialPath)
  // Открывает файл журнала для дозаписи
  def open(path: String): Boolean = {
    close()
    try {
      val stream = new FileOutputStream(path, true) // true — дописывать, а не перезаписывать
      writer = Some(new PrintWriter(new OutputStreamWriter(stream, StandardCharsets.UTF_8), true))
      true
    } catch {
      case e: Exception =>
        Term.println(s"Предупреждение: не удалось открыть файл журнала \"$path\": ${e.getMessage}")
        Term.println("Программа продолжит работу без журнала. Проверьте параметр logFile в конфигурации.")
        writer = None
        false
    }
  }
  def info(message: String): Unit = write("INFO", message)
  def warn(message: String): Unit = write("WARN", message)
  def error(message: String): Unit = write("ERROR", message)
  private def write(level: String, message: String): Unit = {
    writer.foreach { w =>
      val time = LocalDateTime.now().format(timeFormat)
      w.println(f"$time $level%-5s $message")
      // PrintWriter не бросает исключений, ошибку записи нужно проверять вручную
      if (w.checkError()) {
        Term.println("Предупреждение: не удалось записать в файл журнала. Запись журнала отключена.")
        writer = None
      }
    }
  }
  def close(): Unit = {
    writer.foreach(_.close())
    writer = None
  }
}
