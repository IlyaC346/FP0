package diskanalyzer

import java.nio.file.{Files, Path}
import java.time.LocalDateTime
import scala.collection.mutable.ArrayBuffer

//Каталог отмеченный как кандидат на удаление
class Candidate(
    val id: Int,
    val path: Path,
    var sizeBytes: Long,
    var scannedAt: LocalDateTime,
    var complete: Boolean,
    val comment: String
) {
  def exists: Boolean = Files.isDirectory(path)
}
//Итоги по списку кандидатов
case class CandidatesSummary(
    totalSize: Long,
    freeable: Long,
    freeableIncomplete: Boolean,
    completeCount: Int,
    incompleteCount: Int,
    missingCount: Int
)
object Candidates {
  def overlapping(path: Path, list: Seq[Candidate]): Seq[Candidate] =
    list.filter(c => c.path != path && (path.startsWith(c.path) || c.path.startsWith(path)))
  def parentOf(candidate: Candidate, list: Seq[Candidate]): Option[Candidate] =
    list.find(other => other.path != candidate.path && candidate.path.startsWith(other.path))
  //Кандидаты верхнего уровня те что не лежат внутри другого кандидата
  def topLevel(list: Seq[Candidate]): Seq[Candidate] =
    list.filter(c => parentOf(c, list).isEmpty)
  def summary(list: Seq[Candidate]): CandidatesSummary = {
    val existing = list.filter(_.exists)
    val top = topLevel(existing)
    CandidatesSummary(
      totalSize = list.map(_.sizeBytes).sum,
      freeable = top.map(_.sizeBytes).sum,
      freeableIncomplete = top.exists(!_.complete),
      completeCount = existing.count(_.complete),
      incompleteCount = existing.count(!_.complete),
      missingCount = list.size - existing.size
    )
  }
  //служебная информация и пути
  def reportText(list: Seq[Candidate], units: String, now: LocalDateTime): String = {
    val s = summary(list)
    val atLeast = if (s.freeableIncomplete) "не менее " else ""
    val lines = ArrayBuffer[String]()
    lines += s"Дата формирования: ${Format.dateTime(now)}"
    lines += s"Кандидатов: ${list.size}"
    lines += s"Потенциально освобождаемое место: $atLeast${Format.size(s.freeable, units)}"
    lines += ""
    list.foreach(c => lines += c.path.toString)
    lines.mkString(System.lineSeparator()) + System.lineSeparator()
  }
}
