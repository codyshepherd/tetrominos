/**
  * Created by codys on 4/3/2017.
  */
package myCell

sealed class Pair(val x: Int, val y: Int) {}

class Footprint(f: List[Pair]) extends Iterable[Pair]{
   def iterator: Iterator[Pair] = f.iterator
}

sealed abstract class Tile extends Iterable[Footprint]{}

case class I() extends Tile {
  private val positions = List(
    new Footprint(List(new Pair(0,0), new Pair(1,0), new Pair(2,0), new Pair(3,0))),
    new Footprint(List(new Pair(0,0), new Pair(0,1), new Pair(0,2), new Pair(0,3))) )
  def iterator: Iterator[Footprint] = positions.iterator

  override def toString(): String = "I"
}

case class L() extends Tile {
  private val positions = List(
    new Footprint(List(new Pair(0,0), new Pair(1,0), new Pair(2,0), new Pair(2,1))),
    new Footprint(List(new Pair(0,0), new Pair(0,1), new Pair(0,2), new Pair(1,0))),
    new Footprint(List(new Pair(0,0), new Pair(0,1), new Pair(1,1), new Pair(2,1))),
    new Footprint(List(new Pair(0,2), new Pair(1,0), new Pair(1,1), new Pair(1,2))) )

  def iterator: Iterator[Footprint] = positions.iterator

  override def toString(): String = "L"
}

case class P() extends Tile {
  private val positions = List(
    new Footprint(List(new Pair(0,0), new Pair(0,1), new Pair(1,0), new Pair(2,0))),
    new Footprint(List(new Pair(0,0), new Pair(0,1), new Pair(0,2), new Pair(1,2))) ,
    new Footprint(List(new Pair(0,1), new Pair(1,1), new Pair(2,0), new Pair(2,1))) ,
    new Footprint(List(new Pair(0,0), new Pair(1,0), new Pair(1,1), new Pair(1,2))) )
  def iterator: Iterator[Footprint] = positions.iterator

  override def toString(): String = "P"
}

case class S() extends Tile {
  private val positions = List(
    new Footprint(List(new Pair(0,1), new Pair(0,2), new Pair(1,0), new Pair(1,1))),
    new Footprint(List(new Pair(0,0), new Pair(1,0), new Pair(1,1), new Pair(2,1))) )
  def iterator: Iterator[Footprint] = positions.iterator

  override def toString(): String = "S"
}

case class Z() extends Tile {
  private val positions = List(
    new Footprint(List(new Pair(0,0), new Pair(0,1), new Pair(1,1), new Pair(1,2))),
    new Footprint(List(new Pair(0,1), new Pair(1,0), new Pair(1,1), new Pair(2,0))) )
  def iterator: Iterator[Footprint] = positions.iterator

  override def toString(): String = "Z"
}

case class T() extends Tile {
  private val positions = List(
    new Footprint(List(new Pair(0,0), new Pair(0,1), new Pair(0,2), new Pair(1,1))),
    new Footprint(List(new Pair(0,1), new Pair(1,0), new Pair(1,1), new Pair(2,1))) ,
    new Footprint(List(new Pair(0,1), new Pair(1,0), new Pair(1,1), new Pair(1,2))) ,
    new Footprint(List(new Pair(0,0), new Pair(1,0), new Pair(1,1), new Pair(2,0))) )
  def iterator: Iterator[Footprint] = positions.iterator

  override def toString(): String = "T"
}

case class O() extends Tile {
  private val positions = List(
    new Footprint(List(new Pair(0,0), new Pair(0,1), new Pair(1,0), new Pair(1,1))))
  def iterator: Iterator[Footprint] = positions.iterator

  override def toString(): String = "O"
}

class Cell(val label: Int, var covered: Boolean = false, var contents: Option[Tile] = None, val black: Boolean) {
  override def toString: String = {
    if (covered){
      contents match {
        case Some(I()) => "[I]"
        case Some(L()) => "[L]"
        case Some(P()) => "[P]"
        case Some(S()) => "[S]"
        case Some(Z()) => "[Z]"
        case Some(T()) => "[T]"
        case Some(O()) => "[O]"
        case _ => "[ ]"
      }
    }
    else
      "[ ]"
  }
}

/*
object Program {
  def main(args: Array[String]): Unit = {
    val i = I()
    for (item<-i) print(item)
  }
}
*/