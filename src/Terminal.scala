/** Terminal.scala
  * Created by Cody Shepherd on 4/4/2017.
  */
import scala.collection.mutable.ListBuffer
import scala.util.Try

/** This class handles IO at the user-interface level for the tetromino board game. */
class Terminal {

  /** These are the commands the terminal accepts.  */
  val cmds = Map(
    "run" -> PartialFunction(run),
    "exit" -> PartialFunction(exit)
  )

  /** The terminal's runtime loop.  */
  //while(true) {
    //val line1: List[String] = scala.io.StdIn.readLine("> ").split(" ").toList
    val line1: List[String] = scala.io.StdIn.readLine().split(" ").toList

    if (line1.contains("exit")){
      var cmd = "exit"
      var fn = cmds(cmd)
      fn(List())
      println("This will never print.")
    }
    else if (line1.length == 2 && line1.forall(isNumeric)) {

      val line2: List[String] = scala.io.StdIn.readLine("  ").split("").toList

      val args: List[String] = line1 ::: line2
      val cmd = "run"
      val fn = cmds(cmd)
      fn(args)
      //println("Done.")
    }
    else
      println("That is not a valid command.")

  //}

  /** This helper function comes from this Stack Overflow page:
    * http://stackoverflow.com/questions/28134729/how-to-check-whether-all-elements-in-list-are-numeric
    */
  def isNumeric(input: String): Boolean = input.forall(_.isDigit)

  /** A command that causes a system exit.  */
  def exit(args: List[String]): Boolean = {
    System.exit(0)
    true
  }

  /** Returns whether the game found solution with the given parameters.
    *
    * Does some parsing and checking of arguments as well.
    *
    * @param args: two ints followed by a series of letters
    *               two ints: height and width of the game board, respectively, the product of which must be
    *               a multiple of four.
    *               letters: chars corresponding to the tileset posited as a possible solution. Number of tiles must
    *               be h*w/4 or this function will reject them.
    */
  def run(args: List[String]): Boolean = {
    val h: Int = Try(args.head.toInt).toOption match {
      case Some(i) => i
      case None => return false
    }
    val w: Int = Try(args(1).toInt).toOption match {
      case Some(i) => i
      case None => return false
    }

    var tiles = new ListBuffer[Tile]()
    for (item<-args.drop(2)){
      var t = stringToTile(item)
      t match {
        case Some(tl) => tiles += tl
        case None => return false
      }
    }

    val ts = tiles.toList
    val num_t = ts.count(_ == T())

    //check for impossible board size or Odd-T short-circuit
    if ((h*w)%4 != 0 ){
      println("Impossible board dimensions.")
      return false
    }

    if ((num_t-1)%2 == 0) {
      println("Odd number of Ts. No solution.")
      return false
    }

    val b = new Board(h, w, ts)
    b.game()
  }

  /** Returns a tile to match a given character, with possible failure.*/
  def stringToTile(s: String): Option[Tile] = {
    s.toUpperCase match {
      case "I" => Some(I())
      case "L" => Some(L())
      case "P" => Some(P())
      case "S" | "5" => Some(S())
      case "Z" | "2" => Some(Z())
      case "T" => Some(T())
      case "O" | "0" => Some(O())
      case _ => None
    }
  }
}

object Program {
  def main(args: Array[String]): Unit = {
    val t = new Terminal()
  }
}
