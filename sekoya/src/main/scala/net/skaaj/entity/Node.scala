package net.skaaj.entity

import scala.collection.mutable

final case class Node (
  id: Int,
  parentId: Option[Int],
  content: NodeContent
)
