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

  lazy val textDiagramRepr = {
    def iter(currentId: Long, depth: Int, last: Boolean, collected: Seq[(String, Int, Boolean)]): Seq[(String, Int, Boolean)] = {
      nodes.get(currentId).fold(Seq.empty) { node =>
        node.content match {
          case t: NodeContent.Task =>
            (t.title, depth, last) +: collected
          case g: NodeContent.Group =>
            val groupEdges = edges.getOrElse(currentId, Seq.empty)
            groupEdges
              .zipWithIndex
              .map({case (item, i) => (item, i == groupEdges.size - 1)})
              .foldLeft((g.name, depth, last) +: collected)((acc, item) => iter(item(0), depth + 1, item(1), acc))
        }
      }
    }

    val rootEdges = edges.getOrElse(RootId, Seq.empty)
    val flatRepr = rootEdges
      .zipWithIndex
      .map({case (item, i) => (item, i == rootEdges.size - 1)})
      .foldLeft(Seq.empty[(String, Int, Boolean)]){(acc, item) =>
        acc ++ iter(item(0), 1, item(1), Seq.empty).reverse
      }

    val indexOfLastRoot = flatRepr.lastIndexWhere {
      case (_, 1, _) => true
      case _ => false
    }

    val stylized = flatRepr.zipWithIndex.map {
      case ((content, 1, isLast), i) =>
        val symbol = if(isLast) "└─" else "├─"
        s"$symbol $content"
      case ((content, depth, isLast), i) =>
        val symbol = if(isLast) "└─" else "├─"
        if(i > indexOfLastRoot) "   " + ("│  " * (depth - 2)) + s"$symbol $content"
        else ("│  " * (depth - 1)) + s"$symbol $content"
    }
    
    "Root\n" + stylized.mkString("\n") + "\n"
  }

  override def toString: String = textDiagramRepr
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

