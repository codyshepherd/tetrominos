
/** Cell.scala
  * Created by Cody Shepherd on 4/3/2017.
  */

/** package myCell
  *
  * Provides classes for running a Tentrominos game, as well as for various pretty-printing
  * of the game to the console.
  *
  * Pair is just a named, boxed tuple representing x,y coordinates. A class is used for this to make
  * location references clear and explicit, and to avoid having anonymous tuples all over the place.
  *
  * Footprint is a class that represents the shape and orientation of a Tetromino/Tile in terms of its effect on the
  * game board. Any given tile's footprint is abstracted as existing in a 4x4 grid, with the origin being
  * at the upper left corner. All tiles are abstracted as being "pinned" to the upper left corner of this
  * 4x4 grid. This abstraction ends up assisting the placement of a tile on a board, as each Pair in a
  * tile's footprint can simply be added arithmetically to the single cell location representing the top left corner
  * of intended placement. The convention I am using for enumeration of a Footprint is that all occupied tiles on
  * row i are listed before any on row i+1, and any occupied tile on column j is listed before any on
  * column j+1. Footprint extends Iterable to facilitate use in list comprehensions.
  *
  * Tile is the abstract class from which all Tetrominos inherit. It extends Iterable to facilitate use
  * in list comprehensions. Polymorphism assists the design of the program in several, probably obvious, ways.
  *
  * The case classes represent the seven types of Tetrominos, and their various possible orientations (which
  * are enumerated as Footprints). The convention I am using is that the first orientation returned by the iterator
  * is the one that most closely represents the shape of the class letter - e.g. the first orientation of I() is a
  * vertical column of cells - with the tile rotating pi/2 rads clockwise thereafter. Isomorphisms are exploited
  * to minimize the number of total possible orientations.
  *
  * Cell represents a given game board grid square. Each Cell tracks whether it is covered (i.e. whether a tile
  * has been placed over it, to prevent overlapping), and what type of Tile, if any, covers it, and a marked
  * flag, to facilitate queue-based neighbor-search or partitioning algorithms (see Board.checkPartition()). It also
  * holds a numeric label, and a boolean value that states whether the cell is black or white, as in a checker board.
  * These last two parameters are not currently used, however, and I am debating whether to incorporate
  * serialization, which the label was supposed to facilitate, or black/white tracking to short-circuit placement
  * of T() cells. Both of these "features" seem like they might be a lot of work for fairly little payoff
  * in terms of performance.
  * */
/** A set of grid coordinates, for explicit typing. */
sealed class Pair(val x: Int, val y: Int) {}

/** An iterable enumerating the grid cells comprising a given tile's shape and orientation.  */
sealed class Footprint(f: List[Pair]) extends Iterable[Pair]{
   def iterator: Iterator[Pair] = f.iterator
}

/** A polymorphic type defining the Tetrominos. */
sealed abstract class Tile extends Iterable[Footprint]{}

/** The I Tile  */
case class I() extends Tile {
  private val positions = List(
    new Footprint(List(new Pair(0,0), new Pair(1,0), new Pair(2,0), new Pair(3,0))),
    new Footprint(List(new Pair(0,0), new Pair(0,1), new Pair(0,2), new Pair(0,3))) )

  /** Iterator definition (using the private list above) to facilitate list comprehensions and other
    * iterator behavior.  */
  def iterator: Iterator[Footprint] = positions.iterator

  /** returns a string for pretty-printing. */
  override def toString(): String = "I"
}

/** The L Tile  */
case class L() extends Tile {
  private val positions = List(
    new Footprint(List(new Pair(0,0), new Pair(1,0), new Pair(2,0), new Pair(2,1))),
    new Footprint(List(new Pair(0,0), new Pair(0,1), new Pair(0,2), new Pair(1,0))),
    new Footprint(List(new Pair(0,0), new Pair(0,1), new Pair(1,1), new Pair(2,1))),
    new Footprint(List(new Pair(-1,2), new Pair(0,0), new Pair(0,1), new Pair(0,2))) )

  /** Iterator definition (using the private list above) to facilitate list comprehensions and other
    * iterator behavior.  */
  def iterator: Iterator[Footprint] = positions.iterator

  /** returns a string for pretty-printing. */
  override def toString(): String = "L"
}

/** The P Tile  */
case class P() extends Tile {
  private val positions = List(
    new Footprint(List(new Pair(0,0), new Pair(0,1), new Pair(1,0), new Pair(2,0))),
    new Footprint(List(new Pair(0,0), new Pair(0,1), new Pair(0,2), new Pair(1,2))) ,
    new Footprint(List(new Pair(0,0), new Pair(1,0), new Pair(2,0), new Pair(2,-1))) ,
    new Footprint(List(new Pair(0,0), new Pair(1,0), new Pair(1,1), new Pair(1,2))) )

  /** Iterator definition (using the private list above) to facilitate list comprehensions and other
    * iterator behavior.  */
  def iterator: Iterator[Footprint] = positions.iterator

  /** returns a string for pretty-printing. */
  override def toString(): String = "P"
}

/** The S Tile  */
case class S() extends Tile {
  private val positions = List(
    new Footprint(List(new Pair(0,0), new Pair(0,1), new Pair(1,-1), new Pair(1,0))),
    new Footprint(List(new Pair(0,0), new Pair(1,0), new Pair(1,1), new Pair(2,1))) )

  /** Iterator definition (using the private list above) to facilitate list comprehensions and other
    * iterator behavior.  */
  def iterator: Iterator[Footprint] = positions.iterator

  /** returns a string for pretty-printing. */
  override def toString(): String = "S"
}

/** The Z Tile  */
case class Z() extends Tile {
  private val positions = List(
    new Footprint(List(new Pair(0,0), new Pair(0,1), new Pair(1,1), new Pair(1,2))),
    new Footprint(List(new Pair(0,0), new Pair(1,-1), new Pair(1,0), new Pair(2,-1))) )

  /** Iterator definition (using the private list above) to facilitate list comprehensions and other
    * iterator behavior.  */
  def iterator: Iterator[Footprint] = positions.iterator

  /** returns a string for pretty-printing. */
  override def toString(): String = "Z"
}

/** The T Tile  */
case class T() extends Tile {
  private val positions = List(
    new Footprint(List(new Pair(0,0), new Pair(0,1), new Pair(0,2), new Pair(1,1))),
    new Footprint(List(new Pair(0,0), new Pair(1,-1), new Pair(1,0), new Pair(2,0))) ,
    new Footprint(List(new Pair(0,0), new Pair(1,-1), new Pair(1,0), new Pair(1,1))) ,
    new Footprint(List(new Pair(0,0), new Pair(1,0), new Pair(1,1), new Pair(2,0))) )

  /** Iterator definition (using the private list above) to facilitate list comprehensions and other
    * iterator behavior.  */
  def iterator: Iterator[Footprint] = positions.iterator

  /** returns a string for pretty-printing. */
  override def toString(): String = "T"
}

/** The O Tile  */
case class O() extends Tile {
  private val positions = List(
    new Footprint(List(new Pair(0,0), new Pair(0,1), new Pair(1,0), new Pair(1,1))))

  /** Iterator definition (using the private list above) to facilitate list comprehensions and other
    * iterator behavior.  */
  def iterator: Iterator[Footprint] = positions.iterator

  /** returns a string for pretty-printing. */
  override def toString(): String = "O"
}

/** The class representing a single square on the game board. */
sealed class Cell(val label: Int, var covered: Boolean = false, var contents: Option[Tile] = None, val black: Boolean, var marked: Boolean = false) {
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