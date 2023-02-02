package net.skaaj

import net.skaaj.*
import net.skaaj.entity.{GroupRecord, NodeContent, Node, TaskRecord, TaskStatus}
import net.skaaj.core.Constants.*
import net.skaaj.core.Tree
import net.skaaj.core.Utils.*
import net.skaaj.entity.NodeRecord
import scala.util.Random

object Main extends App {
  // Simulate database
  private val records = {
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

  // App logic
  private val tree = Tree(records)
  private val printTime: ((Long, _)) => _ = {
    case (elapsed, _) => println(s"$elapsed us")
  }
  println(timeTap(printTime)(tree.toString))
}

