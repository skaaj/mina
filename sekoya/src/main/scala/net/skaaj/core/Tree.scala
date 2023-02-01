package net.skaaj.core

import net.skaaj.core.Tree
import net.skaaj.core.Constants.*
import net.skaaj.entity.{GroupRecord, Node, NodeRecord, TaskRecord}
import net.skaaj.entity.NodeContent

import scala.collection.mutable

final class Tree(edges: Map[Long, Seq[Long]], nodes: Map[Long, Node]) {
  def walk[A](startId: Long)(f: Node => A): Seq[A] = {
    def iter(currentId: Long, collected: Seq[A]): Seq[A] = {
      nodes.get(currentId).fold(Seq.empty) { node =>
        node.content match {
          case _: NodeContent.Task =>
            f(node) +: collected
          case _: NodeContent.Group =>
            edges.getOrElse(currentId, Seq.empty)
              .foldLeft(f(node) +: collected)((acc, item) => iter(item, acc))
        }
      }
    }

    if (startId == RootId)
      edges(RootId).foldLeft(Seq.empty)(_ ++ iter(_, Seq.empty).reverse)
    else
      iter(startId, Seq.empty).reverse
  }

  // 
  def walkLazy[A](startId: Long)(f: Node => A): LazyList[A] = {
    def iter(currentId: Long, collected: LazyList[A] = LazyList.empty): LazyList[A] = {
      nodes.get(currentId).fold(LazyList.empty) { node =>
        node.content match {
          case _: NodeContent.Task =>
            collected.appended(f(node))
          case _: NodeContent.Group =>
            edges.getOrElse(currentId, Seq.empty)
              .foldLeft(collected.appended(f(node)))((acc, item) => iter(item, acc))
        }
      }
    }

    if (startId == RootId)
      edges(RootId).foldLeft(LazyList.empty)((acc, id) => acc #::: iter(id))
    else
      iter(startId)
  }

  def nodesCount: Int = nodes.size

  override def toString: String = (edges, nodes).toString
}

object Tree {
  def apply(records: Seq[NodeRecord]): Tree = {
    new Tree(
      edges = records
        .map(record => (record.parentId, Seq(record.id)))
        .groupMapReduce(_(0))(_(1))(_ ++ _),
      nodes = records
        .map(record => (record.id, record.toNode))
        .toMap
    )
  }
}
