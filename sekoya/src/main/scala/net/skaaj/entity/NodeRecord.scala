package net.skaaj.entity

sealed trait NodeRecord {
  def id: Int
  def parentId: Int

  def toNode: Node = this match
    case GroupRecord(id, name, parentId) =>
      Node(id, Some(parentId), NodeContent.Group(name))
    case TaskRecord(id, title, description, status, parentId) =>
      Node(id, Some(parentId), NodeContent.Task(title, description, status))
}

final case class GroupRecord(
  id: Int,
  name: String,
  parentId: Int
) extends NodeRecord

final case class TaskRecord(
  id: Int,
  title: String,
  description: Option[String],
  status: TaskStatus,
  parentId: Int
) extends NodeRecord

object NodeRecord {
  def fromNode(node: Node): Option[NodeRecord] = {
    node.parentId.map { parentId =>
      node.content match
      case NodeContent.Task(title, description, status) =>
        TaskRecord(node.id, title, description, status, parentId)
      case NodeContent.Group(name) =>
        GroupRecord(node.id, name, parentId)
    }
  }
}
