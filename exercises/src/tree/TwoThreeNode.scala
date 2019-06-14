package tree

case class TwoThreeNode(leftVal: Int,
                        rightValOpt: Option[Int],
                        child1: Option[TwoThreeNode],
                        child2: Option[TwoThreeNode],
                        child3: Option[TwoThreeNode],
) {

  def isLeafNode: Boolean = child1.isEmpty && child2.isEmpty && child3.isEmpty

  private var parent: Option[TwoThreeNode] = None

  def setParent(newParent: TwoThreeNode) = {
    this.parent = Some(newParent)
    this
  }

  override def toString: String =
    s"($leftVal, $rightValOpt, ${child1.getOrElse(None)}, ${child2.getOrElse(None)}, ${child3
      .getOrElse(None)}, ${getParent.map(_.leftVal)})"

  def getParent = this.parent

  def split(newNode: TwoThreeNode): TwoThreeNode = {
    println(s"$this is going to rebalance with $newNode ")
    rightValOpt match {
      case None =>
        this.copy(
          rightValOpt = Some(newNode.leftVal),
          child2 = newNode.child1,
          child3 = newNode.child2
        )
      case Some(rightVal) =>
        val values = Seq(leftVal, rightVal, newNode.leftVal).sorted

        val leftNode = TwoThreeNode(
          leftVal = values.head,
          rightValOpt = None,
          None,
          None,
          None,
        )
        val rightNode = TwoThreeNode(
          leftVal = values(2),
          rightValOpt = None,
          None,
          None,
          None,
        )
        val newRoot: TwoThreeNode = TwoThreeNode(
          leftVal = values(1),
          rightValOpt = None,
          child1 = Some(leftNode),
          child2 = Some(rightNode),
          None,
        )
        leftNode.setParent(newRoot)
        rightNode.setParent(newRoot)

        this.parent match {
          case None    => newRoot
          case Some(p) => p.split(newNode)
        }
    }
  }

  def add(e: Int): TwoThreeNode = {
    println(s"Current node $this ")

    if (isLeafNode) {
      println(s"$this is leaf node")
      rightValOpt match {
        case None =>
          val update = this
            .copy(
              leftVal = Math.min(leftVal, e),
              rightValOpt = Some(Math.max(leftVal, e))
            )
          this.getParent.map(p => update.setParent(p))
          update

        case Some(rightVal) =>
          val values = Seq(leftVal, rightVal, e).sorted

          val leftNode = TwoThreeNode(
            leftVal = values.head,
            rightValOpt = None,
            None,
            None,
            None,
          )
          val rightNode = TwoThreeNode(
            leftVal = values(2),
            rightValOpt = None,
            None,
            None,
            None,
          )
          val newNode: TwoThreeNode = TwoThreeNode(
            leftVal = values(1),
            rightValOpt = None,
            child1 = Some(leftNode),
            child2 = Some(rightNode),
            None,
          )
          leftNode.setParent(newNode)
          rightNode.setParent(newNode)
          this.parent match {
            case None    => newNode
            case Some(p) => p.split(newNode)
          }

      }
    } else {
      if (leftVal > e) {
        child1 match {
          case None =>
            this
              .copy(child1 = Some(TwoThreeNode(e, None, None, None, None)))
              .setParent(this)
          case Some(c1) => this.copy(child1 = Some(c1.add(e)))
        }

      } else {
        rightValOpt match {
          case None =>
            child2 match {
              case None =>
                this
                  .copy(child2 = Some(TwoThreeNode(e, None, None, None, None)))
                  .setParent(this)
              case Some(c2) =>
                this.copy(child2 = Some(c2.add(e)))
            }
          case Some(rightVal) =>
            if (e < rightVal) {
              child2 match {
                case None =>
                  this
                    .copy(
                      child2 = Some(TwoThreeNode(e, None, None, None, None))
                    )
                    .setParent(this)
                case Some(c2) => this.copy(child2 = Some(c2.add(e)))

              }
            } else {
              child3 match {
                case None =>
                  this
                    .copy(
                      child3 = Some(TwoThreeNode(e, None, None, None, None))
                    )
                    .setParent(this)
                case Some(c3) => this.copy(child3 = Some(c3.add(e)))
              }
            }

        }
      }
    }

  }
}
object TwoThreeNode {

  def main(args: Array[String]): Unit = {

    val root = TwoThreeNode(1, Some(2), None, None, None)
    val newRoot = root.add(3).add(4).add(5)
//    println(root.leftVal, root.rightValOpt)
    println(newRoot)

  }
}
