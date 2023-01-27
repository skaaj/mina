package net.skaaj.entity

import scala.collection.mutable

final case class Node (
  id: Long,
  title: String,
  content: NodeContent
) {
  def walk[A](f: Node => A): Seq[A] = {
    def go(current: Node, visited: Seq[A]): Seq[A] = {
      current.content match {
        case _: NodeContent.Task =>
          f(current) +: visited
        case group: NodeContent.Group =>
          group.nodes.foldLeft(f(current) +: visited)((acc, item) => go(item, acc))
      }
    }
    go(this, Seq.empty).reverse
  }
}
