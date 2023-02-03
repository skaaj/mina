package net.skaaj.core

import scala.util.chaining.*
import scala.collection.mutable
import net.skaaj.core.Tree
import net.skaaj.core.Constants.*
import net.skaaj.entity.{Node, NodeContent, NodeRecord, TaskRecord, GroupRecord}


final class Tree private (edges: Map[Long, Seq[Long]], nodes: Map[Long, Node]) {
  def walk[A](startId: Long)(f: (Node, Int) => A): Seq[A] = {
    def iter(currentId: Long, depth: Int, collected: Seq[A]): Seq[A] = {
      nodes.get(currentId).fold(Seq.empty) { node =>
        edges
          .getOrElse(currentId, Seq.empty)
          .foldLeft(f(node, depth) +: collected)((xs, x) => iter(x, depth + 1, xs))
      }
    }
    iter(startId, 0, Seq.empty).reverse
  }

  def walk[A](startId: Long)(f: Node => A): Seq[A] = {
    walk(startId)((node, _) => f(node))
  }

  def walkLazy[A](startId: Long)(f: Node => A): LazyList[A] = {
    def iter(currentId: Long, collected: LazyList[A]): LazyList[A] = {
      nodes.get(currentId).fold(LazyList.empty) { node =>
        edges
          .getOrElse(currentId, Seq.empty)
          .foldLeft(collected #::: LazyList(f(node)))((xs, x) => iter(x, xs))
      }
    }
    iter(startId, LazyList.empty)
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

    // FIXME: indexOfLastRoot should be generalized to every groups
    val stylized = flatReprEnriched.zipWithIndex.map {
      case ((content, 0, _), _) =>
        content
      case ((content, 1, isLast), i) =>
        val symbol = if(isLast) "└─" else "├─"
        s"$symbol $content"
      case ((content, depth, isLast), i) =>
        val symbol = if(isLast) "└─" else "├─"
        if(i > indexOfLastRoot) "   " + ("│  " * (depth - 2)) + s"$symbol $content"
        else ("│  " * (depth - 1)) + s"$symbol $content"
    }
    
    stylized.mkString("\n") + "\n"
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
        .prepended((RootId, Node(RootId, None, NodeContent.Group("Root"))))
        .toMap
    )
  }
}
