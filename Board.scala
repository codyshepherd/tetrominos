/** Board.scala
  * Created by Cody Shepherd on 4/3/2017.
  */
import myCell._
import sun.security.pkcs.ContentInfo

import scala.collection.mutable

/** The Board class controls all the in-game action, including initialization and attempts to find a
  * solution.
  *
  * Its primary method is game(), which attempts to find a solution given the Board's initial parameters.
  *
  * @param h: the height of the board
  * @param w: the width of the board
  * @param bag: the list of tetrominos on which to attempt a solution (repetition allowed)
  *
  * total: the total number of squares on the board
  * grid: the 2-D array of grid squares that is the playing surface
  */
class Board ( val h: Int, val w: Int, bag: List[Tile]) {
  val total: Int = h*w
  var grid: Array[Array[Cell]] = {
    val labels = Array.range(0,total).iterator
    for (i<-Array.range(0,h)) yield
      for (j<-Array.range(0,w); l = labels.next) yield new Cell(label = l, black = if (l%2==0) true else false)
  }

  /** Returns a pretty-print string of the grid state. */
  override def toString: String = {
    var st = ""
    for (item<-grid) {
      for (i<-item)
        st += i
      st += '\n'
    }
    st
  }

  /** Returns whether or not a solution was found. Also prints the solution grid state and solution permutation
    * to the console.
    */
  def game(): Boolean = {
    for (p<-bag.permutations) {
      if (solve(p)) {
        print(this)
        println("\nSolved!")
        println(p)
        //scala.io.StdIn.readLine()
        return true
      }
    }
    println("No solution found.")
    false
  }

  /** Returns whether the given list of tiles represents a solution for the grid.
    *
    * @param tiles: the ordered list of tiles, which will be applied to the board, top-left to bottom-right (as
    *             one reads), in different orientations, but maintaining the original sequence order.
    *
    * This method is recursive.
    */
  private def solve(tiles: List[Tile]): Boolean = {
    if(tiles.isEmpty)   // This logic relies on the assumption of an earlier test for exact divisibility of grid
      return true       // size by number of tiles given

    val loc = getNextEmptyPos                             // get topmost, leftmost empty square

    loc match {                                           // see if we got anything
      case None => true                                   // if no, huzzah, solution found
      case Some(p) =>                                     // if yes
        val tile = tiles.head                             // get first tile
        for (dir <- tile) {                               // for each orientation of the tile
          if (/*checkPartition(p) &&*/ place(tile, dir, p))   // if the partition is big enough, and tile can be placed
            if (solve(tiles.tail) && isBoardCovered)      // recurse with smaller list, check for recursion success
              return true                                 // and a covered board
            else
              remove(tile, dir, p)      // otherwise take the last piece off, rotate, and try again
        }
        false                   // if all rotations of the piece have been checked, leave and get a new permutation
    }
  }

  /** Returns whether or not all Cells in the grid have covered == true.  */
  private def isBoardCovered:Boolean = {
    for (row<-grid){
      for (item<-row){
        if (!item.covered)
          return false
      }
    }
    true
  }

  /** Returns the leftmost, topmost (in that order) empty grid space, with the possibility of failure.
    * Option type is used here to preclude runtime exceptions.
    */
  private def getNextEmptyPos: Option[Pair] = {
    var i = 0
    var j = 0
    for (row<-grid){
      j = row.indexWhere(_.covered == false)
      if (j >= 0)
        return Some(new Pair(i, j))
      else
        i += 1
    }
    None
  }

  /** Returns whether or not the given tile with the given orientation could be placed at the given location.
    *
    * This function will check for off-grid placement and overlap of any cells in the footprint.
    *
    * This function changes the state of Board.grid in the event of a successful placement by updating the covered
    * and contents fields of each affected Cell. If tile cannot be placed, no state is changed.
    *
    * @param tile:  the tile to be placed
    * @param ft:  the orientation of the tile
    *             TODO: an assumption is being made here that the pairing between
    *                   the tile and the footprint is correct
    * @param loc: the location of an empty grid square that lives in a partition of at least four empty Cells
    * */
  private def place(tile: Tile, ft: Footprint, loc: Pair): Boolean = {
    if (! checkBounds(ft, loc)) {
      /*
      print(this)
      println("\nFailed to place " + tile + " at " + loc.x + "," + loc.y)
      scala.io.StdIn.readLine()
      */

      return false
    }

    for (p<-ft) {
      grid(loc.x + p.x)(loc.y + p.y).covered = true
      grid(loc.x + p.x)(loc.y + p.y).contents = Option(tile)
    }

    /*
    print(this)
    println("\nPlaced " + tile + " at " + loc.x + "," + loc.y)
    scala.io.StdIn.readLine()
    */

    true
  }

  /** Returns whether or not the location given exists in a partition of at least four empty Cells.
    *
    * Diagonals are not considered adjacent.
    *
    * Note: This method incurs a huge performance cost if not used judiciously.
    *
    * Further Note: Because of my placement methodology I am choosing not to use this method.
    *
    * @param loc: the location of the grid Cell to be checked.
    * */
  private def checkPartition(loc: Pair): Boolean = {
    var q: mutable.Queue[Pair] = new mutable.Queue()
    q.enqueue(loc)

    var items = 0

    while(q.nonEmpty){
      var l = q.dequeue()
      if(!grid(l.x)(l.y).covered){
        grid(l.x)(l.y).marked = true
        items += 1

        var i = l.x-1
        var j = l.y
        if(i >= 0 && !grid(i)(j).covered && !grid(i)(j).marked)
          q.enqueue(new Pair(i, j))

        i = l.x+1
        if(i < grid.length && !grid(i)(j).covered && !grid(i)(j).marked)
          q.enqueue(new Pair(i, j))

        i = l.x
        j = l.y-1
        if(j >= 0 && !grid(i)(j).covered && !grid(i)(j).marked)
          q.enqueue(new Pair(i, j))

        j = l.y+1
        if(j < grid(0).length && !grid(i)(j).covered && !grid(i)(j).marked)
          q.enqueue(new Pair(i, j))
      }
    }

    for (row<-grid)
      for (item<-row)
        item.marked = false

    if (items >= 4)
      true
    else
      false
  }

  /** Returns whether or not removing the tile was successful (currently always returns true).
    *
    * @param tile: the tile to remove TODO: unused
    * @param ft: the footprint of the tile
    * @param loc: the "origin" location of the tile (see myCell package docs for discussion of tile location
    *             abstraction; particularly relevant in the case of S/Z, which may not always actually occupy
    *             the given location.
    */
  private def remove(tile: Tile, ft: Footprint, loc: Pair): Boolean = {
    for (p<-ft) {
      grid(loc.x + p.x)(loc.y + p.y).covered = false
      grid(loc.x + p.x)(loc.y + p.y).contents = None
    }

    /*
    print(this)
    println("\nRemoved " + tile + " from " + loc.x + "," + loc.y)
    scala.io.StdIn.readLine()
    */
    true
  }

  /** Returns whether the given footprint is "legal" at the given location. This means in-bounds on the grid, as
    * well as not overlapping any already-placed tiles.
    *
    * @param ft: the footprint to be checked
    * @param loc: the starting location to be checked
    */
  private def checkBounds(ft: Footprint, loc: Pair) : Boolean = {
    for (p<-ft) {
      val i = loc.x + p.x
      val j = loc.y + p.y
      if (!(i >= 0 && i < grid.length && j >= 0 && j < grid(0).length) || grid(i)(j).covered)
        return false
    }
    true
  }
}

/*
object Program {
  def main(args: Array[String]): Unit = {
    val b = new Board(5, 8, List(L(), S(), I(), O(), T(), O(), T(), I(), P(), Z()))
    print(b)
    println("\nStarting...")
    //scala.io.StdIn.readLine()

    val t = b.game()
  }
}
*/