package ammonite.terminal
import java.io._

import scala.annotation.tailrec

import Debug.log
import LazyList._


// Test Unicode:  漢語;𩶘da
object Term{
  def main(args: Array[String]): Unit = {
    val t = new Term(System.in, System.out)
    @tailrec def rec(): Unit = t.readLine("@ ") match{
      case None =>
      case Some(s) =>
        println(s)
        rec()
    }
    rec()
    t.restore()
  }
}

class Term(input: InputStream,
           output: OutputStream){
  val ansi = new Ansi(output)
  val (width, height, initialConfig) = TTY.init()


  def heightFor(cursor: Int, bufferLength: Int) = {
    val totalLength = (
      // Length of the original text
      bufferLength
      // If the cursor is at the end, it needs one more spot
      + (if (cursor == bufferLength) 1 else 0)
      // We only want to count the last line if there is at
      // least one character on that line
      - 1
    )
    totalLength / width
  }

  def restore() = TTY.stty(initialConfig)

  val reader = new InputStreamReader(input)
  val writer = new OutputStreamWriter(output)
  def readLine(prompt: String): Option[String] = {
    var buffer = Vector.empty[Char]
    object cursor{
      private[this] var value = 0
      def apply() = value
      def bound(n: Int) = {
        if (n < 0) 0
        else if (n > buffer.length) buffer.length
        else n
      }
      def +=(n: Int) = {
        value = bound(value + n)
      }
      def -=(n: Int) = {
        value = bound(value - n)
      }
    }
    def offsetCursor = cursor() + prompt.length
    
    var areaHeight = 0
    def redrawLine() = {
      ansi.moveTo(-1 - areaHeight, 0)
      ansi.clearScreen(0)

      writer.write(prompt)
      writer.write(buffer.toArray)
      writer.flush()
      ansi.moveTo(-1 - (areaHeight - offsetCursor / width), offsetCursor % width)
      writer.flush()
    }

    @tailrec def rec(stream: LazyList[Int]): Option[String] = {
      redrawLine()
      stream match{
        case pref"\u001b[A$rest" => log("up"); cursor -= width; rec(rest)
        case pref"\u001b[B$rest" => log("down"); cursor += width; rec(rest)
        case pref"\u001b[C$rest" => log("right"); cursor += 1; rec(rest)
        case pref"\u001b[D$rest" => log("left"); cursor -= 1; rec(rest)

        case pref"\u001b[5~$rest" => log("fn-up"); rec(rest)
        case pref"\u001b[6~$rest" => log("fn-down"); rec(rest)
        case pref"\u001b[F$rest" => log("fn-right"); cursor += 9999; rec(rest)
        case pref"\u001b[H$rest" => log("fn-left"); cursor -= 9999; rec(rest)

        case pref"\u001b\u001b[A$rest" => log("alt-up"); rec(rest)
        case pref"\u001b\u001b[B$rest" => log("alt-down"); rec(rest)
        case pref"\u001b\u001b[C$rest" => log("alt-right"); rec(rest)
        case pref"\u001b\u001b[D$rest" => log("alt-left"); rec(rest)

        case pref"\u001b[1;2A$rest" => log("shift-up"); rec(rest)
        case pref"\u001b[1;2B$rest" => log("shift-down"); rec(rest)
        case pref"\u001b[1;2C$rest" => log("shift-right"); rec(rest)
        case pref"\u001b[1;2D$rest" => log("shift-left"); rec(rest)

        case pref"\u001b\u001b[5~$rest" => log("fn-alt-up"); rec(rest)
        case pref"\u001b\u001b[6~$rest" => log("fn-alt-down"); rec(rest)
        case pref"\u001b[1;9F$rest" => log("fn-alt-right"); rec(rest)
        case pref"\u001b[1;9H$rest" => log("fn-alt-left"); rec(rest)

        // Conflicts with iTerm hotkeys, same as fn-{up, down}
        // case pref"\u001b[5~$rest" => rec(rest) //fn-shift-up
        // case pref"\u001b[6~$rest" => rec(rest) //fn-shift-down
        case pref"\u001b[1;2F$rest" => log("fn-shift-right"); rec(rest)
        case pref"\u001b[1;2H$rest" => log("fn-shift-left"); rec(rest)

        case pref"\u001b[1;10A$rest" => log("alt-shift-up"); rec(rest)
        case pref"\u001b[1;10B$rest" => log("alt-shift-down"); rec(rest)
        case pref"\u001b[1;10C$rest" => log("alt-shift-right"); rec(rest)
        case pref"\u001b[1;10D$rest" => log("alt-shift-left"); rec(rest)

        // Same as the case fn-alt-{up,down} without the shift
        // case pref"\u001b\u001b[5~$rest" => rec(rest) //fn-alt-shift-up
        // case pref"\u001b\u001b[6~$rest" => rec(rest) //fn-alt-shift-down
        case pref"\u001b[1;10F$rest" => log("fn-alt-shift-right"); rec(rest)
        case pref"\u001b[1;10H$rest" => log("fn-alt-shift-left"); rec(rest)

        case pref"\u001b[3~$rest" =>
          log("fn-delete")
          val (first, last) = buffer.splitAt(cursor())
          buffer = first ++ last.drop(1)
          rec(rest)

        case 127 ~: rest => // Backspace
          val (first, last) = buffer.splitAt(cursor())
          buffer = first.dropRight(1) ++ last
          cursor -= 1
          rec(rest)

        case pref"\u001b[1$rest" =>
          println("UNKNOWN ESCAPE! " + rest)
          rec(rest)

        case 3 ~: rest => // Ctrl-C
          buffer = Vector.empty
          cursor -= 9999
          rec(rest)
        case 4 ~: rest => None // Ctrl-D
        case 5 ~: rest => // Ctrl-E
          println("Char Display Mode Enabled! Ctrl-C to exit")
          var curr = stream
          while(curr.head != 3){
            println("Char " + curr.head)
            curr = curr.tail
          }
          rec(curr)

        case 13 ~: rest => // Enter
          cursor += 9999
          redrawLine()
          writer.write(10)
          writer.write(13)
          writer.flush()
          Some(buffer.mkString)

        case c ~: rest =>
          log("NORMAL CHAR " + c)
          val (first, last) = buffer.splitAt(cursor())
          buffer = (first :+ c.toChar) ++ last

          cursor += 1

          val nextHeight = heightFor(offsetCursor, prompt.length + buffer.length)

          if (nextHeight > areaHeight) {
            Predef.println()
            areaHeight = nextHeight
          }

          rec(rest)
      }

    }
    rec(LazyList.continually(reader.read()))
  }
}
