package net.skaaj.core

import net.skaaj.core.Tree
import net.skaaj.core.Constants.*
import net.skaaj.entity.{GroupRecord, Node, NodeRecord, TaskRecord}
import net.skaaj.entity.NodeContent

import scala.collection.mutable

final class Tree(edges: Map[Long, Seq[Long]], nodes: Map[Long, Node]) {
  def walk[A](startId: Long)(f: (Node, Int) => A): Seq[A] = {
    def iter(currentId: Long, depth: Int, collected: Seq[A]): Seq[A] = {
      nodes.get(currentId).fold(Seq.empty) { node =>
        node.content match {
          case _: NodeContent.Task =>
            f(node, depth) +: collected
          case _: NodeContent.Group =>
            edges.getOrElse(currentId, Seq.empty)
              .foldLeft(f(node, depth) +: collected)((acc, item) => iter(item, depth + 1, acc))
        }
      }
    }

    if (startId == RootId)
      edges(RootId).foldLeft(Seq.empty)(_ ++ iter(_, 1, Seq.empty).reverse)
    else
      iter(startId, 1, Seq.empty).reverse
  }

  def walk[A](startId: Long)(f: Node => A): Seq[A] = {
    walk(startId)((node, _) => f(node))
  }

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
    val flatRepr = walk(RootId) { (node, depth) =>
      node.content match
        case t: NodeContent.Task => 
          (t.title, depth, false)
        case g: NodeContent.Group =>
          (g.name, depth, true)
    }

    val indexOfLastRoot = flatRepr.lastIndexWhere {
      case (_, depth, isGroup) => isGroup && depth == 1
    }

    // How can we find out that an item is the last of its group from the trace ?
    // -> when all elements until the next backtrack have a greater depth
    val traceDepths = flatRepr.map { case (_, depth, _) => depth }
    val flatReprEnriched = for {
      ((content, depth, _), i) <- flatRepr.zipWithIndex
      itemAtSameDepth = traceDepths.drop(i + 1).takeWhile(otherDepth => otherDepth >= depth)
    } yield (content, depth, itemAtSameDepth.forall(_ > depth))

    val stylized = flatReprEnriched.zipWithIndex.map {
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

