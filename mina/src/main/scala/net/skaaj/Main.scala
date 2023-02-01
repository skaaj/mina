package net.skaaj

import net.skaaj.*
import net.skaaj.entity.{GroupRecord, NodeContent, TaskRecord, TaskStatus}
import net.skaaj.core.Constants.*
import net.skaaj.core.Tree
import net.skaaj.core.Utils.*

object Main extends App {
  // Simulate database
  private val groupTable: Seq[GroupRecord] =
    Seq(
      GroupRecord(0, "A first group", RootId),
      GroupRecord(1, "Another group", RootId)
    )
  private val taskTable: Seq[TaskRecord] =
    Seq(
      TaskRecord(2, "A first task", None, TaskStatus.Open, RootId),
      TaskRecord(3, "Another task", None, TaskStatus.Open, 1),
    )

  // App logic
  private val tree = Tree(groupTable ++ taskTable)
  println(tree)
  println("====")
  val collected = tree.walk(RootId) { node =>
    node.content match {
      case NodeContent.Task(title, _, _) => title
      case NodeContent.Group(name) => name
    }
  }
  collected.foreach(println)
}
