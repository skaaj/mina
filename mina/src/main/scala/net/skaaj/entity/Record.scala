package net.skaaj.entity

enum Record {
  def id: Int
  def title: String

  case GroupRecord(
    override val id: Int,
    parentId: Option[Int],
    override val title: String,
  ) extends Record

  case TaskRecord(
    override val id: Int,
    parentId: Int,
    override val title: String,
    status: TaskStatus,
  ) extends Record
}
