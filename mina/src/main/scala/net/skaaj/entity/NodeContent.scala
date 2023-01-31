package net.skaaj.entity

import net.skaaj.entity.Node

enum NodeContent {
  case Task(title: String, description: Option[String], status: TaskStatus)
  case Group(name: String)
}
