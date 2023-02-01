package net.skaaj.core

import net.skaaj.core.Tree
import net.skaaj.entity.{GroupRecord, Node, TaskRecord}

final class Tree(edges: Map[Long, Seq[Long]], nodes: Set[Node]) {
  override def toString: String = (edges, nodes).toString
}

object Tree {
  def apply(groupRecords: Seq[GroupRecord], taskRecords: Seq[TaskRecord]): Tree = {
    val edges: Map[Long, Seq[Long]] =
      (groupRecords ++ taskRecords)
        .map(record => (record.parentId, Seq(record.id)))
        .groupMapReduce(_(0))(_(1))(_ ++ _)
    val nodes: Seq[Node] =
      groupRecords.map(_.toNode) ++ taskRecords.map(_.toNode)
    new Tree(edges, nodes.toSet)
  }
}
