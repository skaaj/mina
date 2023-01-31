package net.skaaj.codec

import net.skaaj.Main.GroupRecord
import net.skaaj.entity.{Node, NodeContent}

class GroupRecordCodec extends NodeRecordCodec[GroupRecord]:
  extension (value: GroupRecord) def toNode: Node =
    Node(value.id, value.parentId, NodeContent.Group(value.name))

  def fromNode(node: Node): Option[GroupRecord] = node.content match {
    case group: NodeContent.Group =>
      Some(GroupRecord(node.id, group.name, node.parentId))
    case _ =>
      None
  }
