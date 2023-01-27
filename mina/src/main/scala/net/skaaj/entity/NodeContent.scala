package net.skaaj.entity

import net.skaaj.entity.Node

enum NodeContent {
  case Task(status: TaskStatus)
  case Group(nodes: Seq[Node])
}

object NodeContent {
  def emptyGroup: Group = Group(Seq.empty)
}
