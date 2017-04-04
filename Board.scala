/**
  * Created by codys on 4/3/2017.
  */
import myCell._

class Board ( val h: Int, val w: Int, bag: List[Tile]) {
  val total: Int = h*w
  var grid: Array[Array[Cell]] = {
    val labels = Array.range(0,total).iterator
    for (i<-Array.range(0,h)) yield
      for (j<-Array.range(0,w); l = labels.next) yield new Cell(label = l, black = if (l%2==0) true else false)
  }

  override def toString: String = {
    var st = ""
    for (item<-grid) {
      for (i<-item)
        st += i
      st += '\n'
    }
    st
  }

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
    false
  }

  def solve(tiles: List[Tile]): Boolean = {
    //if (isBoardCovered)   //check if board is covered
    //  return true
    if(tiles.isEmpty)
      return true

    val loc = getNextEmptyPos
    loc match {
      case None => true
      case Some(p) =>
        val tile = tiles.head
        for (dir <- tile) {
          if (checkPartition(p) && place(tile, dir, p))
            if (solve(tiles.tail) && isBoardCovered)
              return true
            else
              remove(tile, dir, p)
        }
        false
    }
  }

  def isBoardCovered:Boolean = {
    for (row<-grid){
      for (item<-row){
        if (!item.covered)
          return false
      }
    }
    true
  }

  def getNextEmptyPos: Option[Pair] = {
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

  def place(tile: Tile, ft: Footprint, loc: Pair): Boolean = {
    //val ft: Footprint = tile.head
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

  def checkPartition(loc: Pair): Boolean = {
    var items = 1

    var i = loc.x - 1
    var j = loc.y

    while (i >= 0 && !grid(i)(j).covered){  //UP
      items += 1
      i -= 1
    }
    i = loc.x + 1
    j = loc.y

    while (i < grid.length && !grid(i)(j).covered){  //UP
      items += 1
      i += 1
    }
    i = loc.x
    j = loc.x - 1

    while (j >= 0 && !grid(i)(j).covered){  //LEFT
      items += 1
      j -= 1
    }
    i = loc.x
    j = loc.x + 1

    while (j < grid(0).length && !grid(i)(j).covered){  //UP
      items += 1
      j += 1
    }

    if (items >= 4)
      true
    else
      false
  }

  def remove(tile: Tile, ft: Footprint, loc: Pair): Boolean = {
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