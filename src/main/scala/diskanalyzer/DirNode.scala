package diskanalyzer

import java.nio.file.Path
import java.time.LocalDateTime
import scala.collection.mutable.ArrayBuffer

// Один каталог в дереве результатов сканирования
class DirNode(val path: Path, var parent: Option[DirNode]) {
  val name: String = if (path.getFileName == null) path.toString else path.getFileName.toString
  val scannedAt: LocalDateTime = LocalDateTime.now()
  var hidden = false
  var filesSize = 0L
  var filesCount = 0L
  var ownProblems = false
  val children: ArrayBuffer[DirNode] = ArrayBuffer()
  var childrenInMemory = true
  // Итоги с учётом всех вложенных каталогов 
  var totalSize = 0L
  var totalFiles = 0L
  var totalDirs = 0L
  var incomplete = false
  def recalc(): Unit = {
    if (childrenInMemory) {
      totalSize = filesSize + children.map(_.totalSize).sum
      totalFiles = filesCount + children.map(_.totalFiles).sum
      totalDirs = children.size + children.map(_.totalDirs).sum
      incomplete = ownProblems || children.exists(_.incomplete)
    }
  }
  def recalcParents(): Unit = {
    parent.foreach { p =>
      p.recalc()
      p.recalcParents()
    }
  }
  def find(target: Path): Option[DirNode] = {
    if (path == target) Some(this)
    else children.find(c => target.startsWith(c.path)).flatMap(_.find(target))
  }
  def countNodes: Int = 1 + children.map(_.countNodes).sum
  //Количество каталогов с неполными данными
  def countIncomplete: Int = (if (incomplete) 1 else 0) + children.map(_.countIncomplete).sum
}
