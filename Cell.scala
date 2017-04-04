/**
  * Created by codys on 4/3/2017.
  */

class Pair(val x: Int, val y: Int) {}

class Footprint(f: List[Pair]) {}

sealed abstract class Tile extends Iterable[Footprint]{}

case class I() extends Tile {
  private val positions = List(
    new Footprint(List(new Pair(0,0), new Pair(1,0), new Pair(2,0), new Pair(3,0))),
    new Footprint(List(new Pair(0,0), new Pair(0,1), new Pair(0,2), new Pair(0,3))) )
  def iterator: Iterator[Footprint] = positions.iterator
}

case class L() extends Tile {
  private val positions = List(
    new Footprint(List(new Pair(0,0), new Pair(1,0), new Pair(2,0), new Pair(2,1))),
    new Footprint(List(new Pair(0,0), new Pair(0,1), new Pair(0,2), new Pair(1,0))),
    new Footprint(List(new Pair(0,0), new Pair(0,1), new Pair(1,1), new Pair(2,1))),
    new Footprint(List(new Pair(0,2), new Pair(1,0), new Pair(1,1), new Pair(1,2))) )

  def iterator: Iterator[Footprint] = positions.iterator
}

case class P() extends Tile {
  private val positions = List(
    new Footprint(List(new Pair(0,0), new Pair(1,0), new Pair(2,0), new Pair(3,0))),
    new Footprint(List(new Pair(0,0), new Pair(0,1), new Pair(0,2), new Pair(0,3))) ,
    new Footprint(List(new Pair(0,0), new Pair(0,1), new Pair(0,2), new Pair(0,3))) ,
    new Footprint(List(new Pair(0,0), new Pair(0,1), new Pair(0,2), new Pair(0,3))) )
  def iterator: Iterator[Footprint] = positions.iterator
}

class Cell(val label: Int, var covered: Boolean, var contents: Option[Tile], val Black: Boolean)

object Program {
  def main(args: Array[String]): Unit = {
    val i = I()
    for (item<-i) print(item)
  }
}
