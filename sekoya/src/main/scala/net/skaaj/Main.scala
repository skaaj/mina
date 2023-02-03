package net.skaaj

import net.skaaj.*
import net.skaaj.entity.{GroupRecord, NodeContent, Node, TaskRecord, TaskStatus}
import net.skaaj.core.Constants.*
import net.skaaj.core.Tree
import net.skaaj.core.Utils.*
import net.skaaj.entity.NodeRecord
import scala.util.Random
import scala.util.chaining.*

object Main extends App {
  // Simulate database
  private val randomRecords = {
    val taskCount = 100
    val groupCount = 20

    val groups = (0 until groupCount)
      .map(i => GroupRecord(i, s"Group $i", RootId))

    val movedGroups = groups.map { group =>
      if(Random.nextFloat() < 0.8f) {
        val parentId: Option[Long] = Some(Random.between(0, groupCount))
        group.copy(parentId = parentId.filterNot(_ == group.id).getOrElse(RootId))
      } else {
        group
      }
    }

    val tasks = (groupCount until (groupCount + taskCount))
      .map(i => TaskRecord(i, s"Task $i", None, TaskStatus.Open, Random.between(0, groupCount)))
    
    Random.shuffle(movedGroups ++ tasks)
  }

  private val records = {
    Seq[NodeRecord](
      GroupRecord(0, "Group 0", RootId),
      TaskRecord(1, "Task 1", None, TaskStatus.Open, 0),
      TaskRecord(2, "Task 2", None, TaskStatus.Open, 0),
      GroupRecord(3, "Group 3", RootId),
      GroupRecord(4, "Group 4", RootId),
      TaskRecord(5, "Task 5", None, TaskStatus.Open, 4),
      GroupRecord(6, "Group 6", 4),
    )
  }

  val tree = Tree.fromSeq(records)
  println(tree)
}
