package ammonite.terminal
import java.io._

import scala.annotation.tailrec

import Debug.log
import ammonite.terminal.LazyList._


// Test Unicode:  漢語;𩶘da
object Term{
  def main(args: Array[String]): Unit = {
    rec()
    @tailrec def rec(): Unit = {
      TermCore.readLine("@ ", System.in, System.out, multilineFilter orElse defaultFilter) match {
        case None => println("Bye!")
        case Some(s) =>
          println(s)
          rec()
      }
    }
  }

  val TS = TermState
  val TI = TermInfo
  val multilineFilter: TermCore.Filter = {
    case TS(13 ~: rest, b, c) =>
      val open = b.count(_ == '(')
      val close = b.count(_ == ')')
      log(open + "\t" + close)
      if (open == close) Result(b.mkString)
      else {
        val (first, last) = b.splitAt(c)
        TermState(rest, (first :+ '\n') ++ last, c + 1)
      }
  }
  val defaultFilter = {
    advancedNavFilter orElse
    basicNavFilter orElse
    exitFilter orElse
    enterFilter orElse
    loggingFilter orElse
    typingFilter
  }


  lazy val loggingFilter: TermCore.Filter = {
    case TS(5 ~: rest, b, c) => // Ctrl-E
      println("Char Display Mode Enabled! Ctrl-C to exit")
      var curr = rest
      while (curr.head != 3) {
        println("Char " + curr.head)
        curr = curr.tail
      }
      TS(curr, b, c)
  }
  lazy val typingFilter: TermCore.Filter = {
    case TS(pref"\u001b[3~$rest", b, c) =>
      log("fn-delete")
      val (first, last) = b.splitAt(c)
      TS(rest, first ++ last.drop(1), c)

    case TS(127 ~: rest, b, c) => // Backspace
      val (first, last) = b.splitAt(c)
      TS(rest, first.dropRight(1) ++ last, c - 1)




    case TS(char ~: rest, b, c) =>
      log("NORMAL CHAR " + char)
      val (first, last) = b.splitAt(c)
      TS(rest, (first :+ char.toChar) ++ last, c + 1)
  }

  lazy val enterFilter: TermCore.Filter = {
    case TS(13 ~: rest, b, c) => // Enter
      Result(b.mkString)
  }
  lazy val exitFilter: TermCore.Filter = {
    case TS(3 ~: rest, b, c) => // Ctrl-C
      TS(rest, Vector.empty, 0)
    case TS(4 ~: rest, b, c) => Exit // Ctrl-D
  }
  lazy val basicNavFilter : TermCore.Filter = {
    case TI(TS(pref"\u001b[A$rest", b, c), w) => log("up"); TS(rest, b, c - w)
    case TI(TS(pref"\u001b[B$rest", b, c), w) => log("down"); TS(rest, b, c + w)
    case TS(pref"\u001b[C$rest", b, c) => log("right"); TS(rest, b, c + 1)
    case TS(pref"\u001b[D$rest", b, c) => log("left"); TS(rest, b, c - 1)

    case TS(pref"\u001b[5~$rest", b, c) => log("fn-up"); TS(rest, b, c)
    case TS(pref"\u001b[6~$rest", b, c) => log("fn-down"); TS(rest, b, c)
    case TS(pref"\u001b[F$rest", b, c) => log("fn-right"); TS(rest, b, c + 9999)
    case TS(pref"\u001b[H$rest", b, c) => log("fn-left"); TS(rest, b, c - 9999)

  }
  lazy val advancedNavFilter: TermCore.Filter = {
    case TS(pref"\u001b\u001b[A$rest", b, c) => log("alt-up"); TS(rest, b, c)
    case TS(pref"\u001b\u001b[B$rest", b, c) => log("alt-down"); TS(rest, b, c)
    case TS(pref"\u001b\u001b[C$rest", b, c) => log("alt-right"); TS(rest, b, c)
    case TS(pref"\u001b\u001b[D$rest", b, c) => log("alt-left"); TS(rest, b, c)

    case TS(pref"\u001b[1;2A$rest", b, c) => log("shift-up"); TS(rest, b, c)
    case TS(pref"\u001b[1;2B$rest", b, c) => log("shift-down"); TS(rest, b, c)
    case TS(pref"\u001b[1;2C$rest", b, c) => log("shift-right"); TS(rest, b, c)
    case TS(pref"\u001b[1;2D$rest", b, c) => log("shift-left"); TS(rest, b, c)

    case TS(pref"\u001b\u001b[5~$rest", b, c) => log("fn-alt-up"); TS(rest, b, c)
    case TS(pref"\u001b\u001b[6~$rest", b, c) => log("fn-alt-down"); TS(rest, b, c)
    case TS(pref"\u001b[1;9F$rest", b, c) => log("fn-alt-right"); TS(rest, b, c)
    case TS(pref"\u001b[1;9H$rest", b, c) => log("fn-alt-left"); TS(rest, b, c)

    // Conflicts with iTerm hotkeys, same as fn-{up, down}
    // case TS(pref"\u001b[5~$rest", b, c) => TS(rest, b, c) //fn-shift-up
    // case TS(pref"\u001b[6~$rest", b, c) => TS(rest, b, c) //fn-shift-down
    case TS(pref"\u001b[1;2F$rest", b, c) => log("fn-shift-right"); TS(rest, b, c)
    case TS(pref"\u001b[1;2H$rest", b, c) => log("fn-shift-left"); TS(rest, b, c)

    case TS(pref"\u001b[1;10A$rest", b, c) => log("alt-shift-up"); TS(rest, b, c)
    case TS(pref"\u001b[1;10B$rest", b, c) => log("alt-shift-down"); TS(rest, b, c)
    case TS(pref"\u001b[1;10C$rest", b, c) => log("alt-shift-right"); TS(rest, b, c)
    case TS(pref"\u001b[1;10D$rest", b, c) => log("alt-shift-left"); TS(rest, b, c)

    // Same as the case fn-alt-{up,down} without the shift
    // case TS(pref"\u001b\u001b[5~$rest", b, c) => TS(rest, b, c) //fn-alt-shift-up
    // case TS(pref"\u001b\u001b[6~$rest", b, c) => TS(rest, b, c) //fn-alt-shift-down
    case TS(pref"\u001b[1;10F$rest", b, c) => log("fn-alt-shift-right"); TS(rest, b, c)
    case TS(pref"\u001b[1;10H$rest", b, c) => log("fn-alt-shift-left"); TS(rest, b, c)
  }
}

