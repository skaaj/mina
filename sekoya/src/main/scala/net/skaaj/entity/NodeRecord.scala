package net.skaaj.entity

sealed trait NodeRecord {
  def id: Long
  def parentId: Long

  def toNode: Node = this match
    case GroupRecord(id, name, parentId) =>
      Node(id, parentId, NodeContent.Group(name))
    case TaskRecord(id, title, description, status, parentId) =>
      Node(id, parentId, NodeContent.Task(title, description, status))
}

final case class GroupRecord(
  id: Long,
  name: String,
  parentId: Long
) extends NodeRecord

final case class TaskRecord(
  id: Long, title: String,
  description: Option[String],
  status: TaskStatus,
  parentId: Long
) extends NodeRecord

object NodeRecord {
  def fromNode(node: Node): NodeRecord = node.content match
    case NodeContent.Task(title, description, status) =>
      TaskRecord(node.id, title, description, status, node.parentId)
    case NodeContent.Group(name) =>
      GroupRecord(node.id, name, node.parentId)
}
