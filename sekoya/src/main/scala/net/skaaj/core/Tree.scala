package net.skaaj.core

import scala.util.chaining.*
import scala.collection.mutable
import net.skaaj.core.Tree
import net.skaaj.core.Constants.*
import net.skaaj.entity.{Node, NodeContent, NodeRecord, TaskRecord, GroupRecord}


final class Tree private (edges: Map[Int, Seq[Int]], nodes: Map[Int, Node]) {
  def nodesCount: Int = nodes.size

  def collectFrom[A](startId: Int)(pf: PartialFunction[(Node, Int), A]): Seq[A] = {
    def iter(currentId: Int, depth: Int, collected: Seq[A]): Seq[A] = {
      nodes.get(currentId).fold(Seq.empty) { node =>
        edges
          .getOrElse(currentId, Seq.empty)
          .foldLeft {
            if(pf.isDefinedAt((node, depth)))
              pf(node, depth) +: collected
            else
              collected
          } ((xs, x) => iter(x, depth + 1, xs))
      }
    }
    iter(startId, 0, Seq.empty).reverse
  }

  def collect[A](pf: PartialFunction[(Node, Int), A]): Seq[A] =
    collectFrom(RootId)(pf)

  def mapTrace[A](f: (Node, Int) => A): Seq[A] =
    collect { case (node, depth) => f(node, depth) }

  def map[A](f: Node => A): Seq[A] =
    mapTrace((n, _) => f(n))

  def filterTrace(p: (Node, Int) => Boolean): Seq[(Node, Int)] =
    collect { case (node, depth) if p(node, depth) => (node, depth) }

  def filter(p: Node => Boolean): Seq[Node] =
    collect { case (node, _) if p(node) => node }

  def find(p: Node => Boolean): Option[Node] =
    filter(p).headOption

  def walkLazy[A](startId: Int)(f: Node => A): LazyList[A] = {
    def iter(currentId: Int, collected: LazyList[A]): LazyList[A] = {
      nodes.get(currentId).fold(LazyList.empty) { node =>
        edges
          .getOrElse(currentId, Seq.empty)
          .foldLeft(collected #::: LazyList(f(node)))((xs, x) => iter(x, xs))
      }
    }
    iter(startId, LazyList.empty)
  }

  def move(nodeId: Int, targetId: Int): Option[Tree] = {
    for {
      node <- nodes.get(nodeId)
      parentId <- node.parentId
      parentChildren <- edges.get(parentId)
      targetChildren <- edges.get(targetId)
      newEdges = edges
        .updated(targetId, targetChildren.appended(nodeId))
        .updated(parentId, parentChildren.filterNot(_ == nodeId))
      newNodes = nodes
        .updated(nodeId, node.copy(parentId = Some(targetId)))
    } yield new Tree(newEdges, newNodes)
  }

  lazy val textDiagramRepr = {
    val flatRepr = collectFrom(RootId) { (node, depth) =>
      node.content match
        case t: NodeContent.Task => (t.title, depth, false)
        case g: NodeContent.Group => (g.name, depth, true)
    }

    // How can we find out that an item is the last of its group from the trace ?
    // -> when all elements until the next backtrack have a greater depth
    val traceDepths = flatRepr.map { case (_, depth, _) => depth }
    val flatReprEnriched = for {
      ((content, depth, _), i) <- flatRepr.zipWithIndex
      itemsAtSameDepth = traceDepths.drop(i + 1).takeWhile(otherDepth => otherDepth >= depth)
    } yield (content, depth, itemsAtSameDepth.forall(_ > depth))

    val indentMasks = (for {
      ((_, depth, isLast), i) <- flatReprEnriched.zipWithIndex if isLast && depth > 0
      itemsAtSameDepth = traceDepths.drop(i + 1).takeWhile(otherDepth => otherDepth >= depth)
      maskPair <- itemsAtSameDepth.zipWithIndex.map { case (_, j) => (i + 1 + j, Seq(depth)) }
    } yield maskPair).groupMapReduce(_(0))(_(1))(_ ++ _)

    val stylized = flatReprEnriched.zipWithIndex.map {
      case ((content, 0, _), _) =>
        content
      case ((content, depth, isLast), i) =>
        val symbol = if(isLast) "└─ " else "├─ "
        val indentation = (1 until depth).map(indent =>
          val mask = indentMasks.getOrElse(i, Seq.empty)
          if(mask.contains(indent)) "   " else "│  "
        ).mkString
        s"$indentation$symbol$content"
    }
    
    stylized.mkString(start="", sep="\n", end="\n")
  }

  override def toString: String = textDiagramRepr
}

object Tree {
  def apply(records: NodeRecord*): Tree =
    fromSeq(records)

  def fromSeq(records: Seq[NodeRecord]): Tree = {
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
