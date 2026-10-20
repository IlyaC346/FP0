package diskanalyzer

import java.io.{BufferedReader, FileDescriptor, FileOutputStream, InputStreamReader, PrintStream}
import java.nio.charset.{Charset, StandardCharsets}
import scala.util.Try
//Ввод и вывод в консоль.
object Term {
  val charset: Charset =
    sys.props.get("sun.stdout.encoding")
      .flatMap(name => Try(Charset.forName(name)).toOption)
      .getOrElse(StandardCharsets.UTF_8)
  private val out = new PrintStream(new FileOutputStream(FileDescriptor.out), true, charset.name)
  private val in = new BufferedReader(new InputStreamReader(System.in, charset))
  // Можно ли вывести строку в текущей кодировке консоли
  def canShow(s: String): Boolean = charset.newEncoder().canEncode(s)
  val geq: String = if (canShow("≥")) "≥" else ">="
  def println(s: String = ""): Unit = out.println(s)
  def print(s: String): Unit = {
    out.print(s)
    out.flush()
  }
  // Читает строку None — ввод закончился 
  def readLine(prompt: String): Option[String] = {
    print(prompt)
    Option(in.readLine())
  }
  // Есть ли уже введённая строка
  def inputReady: Boolean = Try(in.ready()).getOrElse(false)
  private var progressLength = 0
  def showProgress(line: String): Unit = {
    val text = line.take(110)
    print("\r" + text + " " * (progressLength - text.length))
    progressLength = text.length
  }
  def clearProgress(): Unit = {
    print("\r" + " " * progressLength + "\r")
    progressLength = 0
  }
}
