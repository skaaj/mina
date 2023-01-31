package net.skaaj

import net.skaaj.*
import net.skaaj.Main.GroupRecord
import net.skaaj.entity.TaskStatus
import net.skaaj.entity.Node
import net.skaaj.entity.NodeContent
import net.skaaj.Constants.*
import net.skaaj.codec.{GroupRecordCodec, NodeRecordCodec, TaskRecordCodec}

object Main extends App {
  // Database model
  sealed trait NodeRecord {
    def id: Long
    def parentId: Long
  }
  final case class GroupRecord(id: Long, name: String, parentId: Long) extends NodeRecord
  final case class TaskRecord(
    id: Long, title: String,
    description: Option[String],
    status: TaskStatus,
    parentId: Long
  ) extends NodeRecord
  // ...and stubs
  private val groupTable: Set[GroupRecord] =
    Set(
      GroupRecord(0, "A first group", RootId),
      GroupRecord(1, "Another group", RootId)
    )
  private val taskTable: Set[TaskRecord] =
    Set(
      TaskRecord(2, "A first task", None, TaskStatus.Open, RootId),
      TaskRecord(3, "Another task", None, TaskStatus.Open, 1),
    )

  // Memory model
  given NodeRecordCodec[TaskRecord] = new TaskRecordCodec
  given NodeRecordCodec[GroupRecord] = new GroupRecordCodec
  private val edges: Map[Long, Set[Long]] = {
    (groupTable ++ taskTable)
      .map(record => (record.parentId, Set(record.id)))
      .groupMapReduce(_(0))(_(1))(_ ++ _)
  }
  private val nodes: Set[Node] =
    groupTable.map(_.toNode) ++ taskTable.map(_.toNode)
  println(edges)
  println(nodes)

  // Walking
  // FIXME: root node being virtual makes things ugly here
  def walk[A](start: Long)(f: Node => A): Seq[A] = {
    def go(current: Long, visited: Seq[A]): Seq[A] = {
      nodes.find(_.id == current) match {
        case Some(node) =>
          node.content match {
            case _: NodeContent.Task =>
              f(node) +: visited
            case _: NodeContent.Group =>
              edges.getOrElse(current, Seq.empty)
                .foldLeft(f(node) +: visited)((acc, item) => go(item, acc))
          }
        case None =>
          Seq.empty
      }
    }
    if(start == RootId) edges(start).foldLeft(Seq.empty[A])(_ ++ go(_, Seq.empty[A]))
    else go(start, Seq.empty).reverse
  }
  println(walk(RootId)(_.id))
}

object Utils {
  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) + "ns")
    result
  }
}