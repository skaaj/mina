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
    val taskCount = 8000
    val groupCount = 2000

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
    
    movedGroups ++ tasks
  }

  // App logic
  private val tree = Tree(records)
  private val printTime: ((Long, _)) => _ = {
    case (elapsed, _) => println(s"$elapsed us")
  }
  private val getRepr: Node => String = {
    node => node.content match {
      case t: NodeContent.Task => t.title
      case g: NodeContent.Group => g.name
    }
  }
  timeTap(printTime){
    val collected = tree.walkLazy(RootId)(getRepr)
    collected.find(_ == "Task 3000")
  }
  timeTap(printTime){
    val collected = tree.walk(RootId)(getRepr)
    collected.find(_ == "Task 3000")
  }
}
