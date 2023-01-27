package net.skaaj

import net.skaaj.*
import net.skaaj.entity.TaskStatus
import net.skaaj.entity.Record.{GroupRecord, TaskRecord}
import net.skaaj.entity.Node
import net.skaaj.entity.NodeContent

object Main extends App {
  // Record model
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

  println(nodes)
  println(edges)

  // Tree model
  private val tree =
    Node(1, "Root", NodeContent.Group(Seq(
      Node(2, "Node 01", NodeContent.Task(TaskStatus.Open)),
      Node(3, "Node 02", NodeContent.emptyGroup),
      Node(4, "Node 03", NodeContent.Task(TaskStatus.Frozen))
    )))

  println(tree)
}
