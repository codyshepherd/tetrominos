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

  def place(tile: Tile, loc: Pair): Boolean = {
    val ft: Footprint = tile.head
    if (! check_bounds(ft, loc))
      return false

    for (p<-ft) {
      grid(loc.x + p.x)(loc.y + p.y).covered = true
      grid(loc.x + p.x)(loc.y + p.y).contents = Option(tile)
    }

    true
  }

  private def check_bounds(ft: Footprint, loc: Pair) : Boolean = {
    for (p<-ft) {
      val i = loc.x + p.x
      val j = loc.y + p.y
      if (!(i >= 0 && i < grid.length && j >= 0 && j < grid(0).length))
        return false
    }
    true
  }
}

object Program {
  def main(args: Array[String]): Unit = {
    val b = new Board(10, 10, List(I(), T()))
    print(b)
    print('\n')
    val tl = T()
    var pr = new Pair(x=0, y=0)
    val tf = b.place(tl, pr)
    print(b)
  }
}
