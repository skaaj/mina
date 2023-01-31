package net.skaaj.codec

import net.skaaj.Main.{GroupRecord, TaskRecord}
import net.skaaj.entity.{Node, NodeContent}

class TaskRecordCodec extends NodeRecordCodec[TaskRecord]:
  extension (value: TaskRecord) def toNode: Node =
    Node(value.id, value.parentId, NodeContent.Task(value.title, value.description, value.status))

  def fromNode(node: Node): Option[TaskRecord] = node.content match {
    case task: NodeContent.Task =>
      Some(TaskRecord(node.id, task.title, task.description, task.status, node.parentId))
    case _ =>
      None
  }
