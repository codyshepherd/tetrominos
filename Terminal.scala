/**
  * Created by codys on 4/4/2017.
  */
import myCell._

import scala.collection.mutable.ListBuffer
import scala.util.Try

class Terminal {
  val cmds = Map(
    "run" -> PartialFunction(run),
    "exit" -> PartialFunction(exit)
  )
  while(true) {
    var line = scala.io.StdIn.readLine("> ").split(" ")
    var cmd = line(0)
    var args = line.drop(1).toList

    var fn = cmds(cmd)
    fn(args)
    println("Done.")

  }

  def exit(args: List[String]): Boolean = {
    System.exit(0)
    true
  }

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

    val b = new Board(h, w, ts)
    b.game()
  }

  def stringToTile(s: String): Option[Tile] = {
    s.toUpperCase match {
      case "I" => Some(I())
      case "L" => Some(L())
      case "P" => Some(P())
      case "S" => Some(S())
      case "Z" => Some(Z())
      case "T" => Some(T())
      case "O" => Some(O())
      case _ => None
    }
  }
}

object Program {
  def main(args: Array[String]): Unit = {
    val t = new Terminal()
  }
}
