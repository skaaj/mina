package net.skaaj

import net.skaaj.*
import net.skaaj.entity.Record
import net.skaaj.entity.Record.{GroupRecord, TaskRecord}
import net.skaaj.entity.TaskStatus

object Main extends App {
  private val nodes = Seq(
    GroupRecord(0, None, "Project Alpha"),
    GroupRecord(3, None, "Project Sigma"),
    TaskRecord(1, 0, "Task One", TaskStatus.Open),
    TaskRecord(2, 0, "Task Two", TaskStatus.Open),
  ).groupMapReduce(_.id)(identity)((_, value) => value)

  private val edges: Map[Option[Int], Seq[Int]] = nodes.values.map {
    case group: GroupRecord => (group.parentId, Seq(group.id))
    case task: TaskRecord => (Some(task.parentId), Seq(task.id))
  }.groupMapReduce(_(0))(_(1))(_ ++ _)

  private val rootTitles =
    edges
      .get(None)
      .map { ids =>
        ids.flatMap(nodes.get).map(_.title)
      }

  println(rootTitles)
  println(nodes.get(2))
}
