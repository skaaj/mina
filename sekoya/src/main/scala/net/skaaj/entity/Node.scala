package net.skaaj.entity

import scala.collection.mutable

final case class Node (
  id: Long,
  parentId: Option[Long],
  content: NodeContent
)
