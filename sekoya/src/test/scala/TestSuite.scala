import org.scalatest.funsuite.AnyFunSuite
import net.skaaj.core.Tree
import net.skaaj.core.Constants.RootId
import net.skaaj.entity.{GroupRecord, TaskRecord, TaskStatus}

class TestSuite extends AnyFunSuite {
  def getTree: Tree = Tree(
    GroupRecord(0, "Group 0", RootId),
    TaskRecord(1, "Task 1", None, TaskStatus.Open, RootId),
    TaskRecord(2, "Task 2", None, TaskStatus.Open, 0),
    TaskRecord(3, "Task 3", None, TaskStatus.Open, 0)
  )
  
  test("Tree.map") {
    val tree = getTree
    assert(
      tree.map(n => n.id) == Seq(-1, 0, 2, 3, 1)
    )
    assert(
      tree.mapTrace((n, d) => (n.id, d)) == Seq((-1, 0), (0, 1), (2, 2), (3, 2), (1, 1))
    )
  }

  test("Tree.filter") {
    val tree = getTree
    assert(
      tree
        .filter(n => n.id > 0)
        .map(_.id) == Seq(2, 3, 1)
    )
    assert(
      tree
        .filterTrace((n, d) => n.id > 0 && d > 1)
        .map { case (n, d) => n.id } == Seq(2, 3)
    )
  }

  test("Tree.collect") {
    val tree = getTree
    assert(
      tree.collect {
        case (n, d) if n.id > 0 && d > 1 => n.id
      } == Seq(2, 3)
    )
  }

  test("Tree.find") {
    val tree = getTree
    assert(tree.find(_.id == 9).isEmpty)
    assert(tree.find(_.id == 2).isDefined)
  }
}
