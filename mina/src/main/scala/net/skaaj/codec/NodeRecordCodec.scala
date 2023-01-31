package net.skaaj.codec

import net.skaaj.Main.NodeRecord
import net.skaaj.entity.Node

trait NodeRecordCodec[A <: NodeRecord]:
  extension (value: A) def toNode: Node
  def fromNode(node: Node): Option[A]

object NodeRecordCodec:
  def apply[A <: NodeRecord](using codec: NodeRecordCodec[A]): NodeRecordCodec[A] = codec
