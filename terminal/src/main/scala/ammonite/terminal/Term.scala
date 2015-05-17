package ammonite.terminal
import java.io.{FileOutputStream, OutputStream, InputStream}

import scala.annotation.tailrec

import Debug.log

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

      output.write(prompt.getBytes)
      output.write(buffer.mkString.getBytes)
      ansi.moveTo(-1 - (areaHeight - offsetCursor / width), offsetCursor % width)
      output.flush()
    }
    output.write(prompt.getBytes)
    val Esc = 27.toChar

    case class Binding(s: String, f: () => Unit)
    implicit class StringBinding(s: String){
      def bind(f: => Unit) = Binding(s, () => f)
    }
    val data = Seq(
      s"$Esc[A".bind(cursor -= width),
      s"$Esc[B".bind(cursor += width),
      s"$Esc[C".bind(cursor += 1),
      s"$Esc[D".bind(cursor -= 1),
      s"$Esc[5".bind(/*page-down*/),
      s"$Esc[6".bind(/*page-up*/),
      s"$Esc[F".bind(cursor += 9999 /*end*/),
      s"$Esc[H".bind(cursor -= 9999 /*home*/),
      s"$Esc$Esc[D".bind(/*alt-left*/),
      s"$Esc$Esc[C".bind(/*alt-right*/),
      s"${13.toChar}".bind()
    )
    @tailrec def rec(): Option[String] = {
      input.read() match{

        case 27 => input.read() match{
          case '[' =>
            input.read() match{
              case 'A' => cursor -= width
              case 'B' => cursor += width
              case 'C' => cursor += 1
              case 'D' => cursor -= 1
              case '5' => // page-down
              case '6' => // page-up
              case 'F' => cursor += 9999 // End
              case 'H' => cursor -= 9999 // Home
              case x => println("Unknown Control: " + x)
            }
            redrawLine()
            output.flush()
            rec()
          case 27 =>
            input.read() match {
              case '[' =>
                input.read() match{
                  case 'D' =>
                  case 'C' =>
                }

            }
            rec()
          case x =>
            println("UNKNOWN ESCAPE " + x)
            output.flush()
            rec()
        }
        case 13 | 10 => // Enter
          output.write(10)
          output.write(13)
          output.flush()
          Some(buffer.mkString)
        case 127 => // Backspace
          val (first, last) = buffer.splitAt(cursor())
          buffer = first.dropRight(1) ++ last
          cursor -= 1

          redrawLine()
          rec()

        case 4 => // Ctrl-D
          None
        case 9 => // Tab
          rec()
        case x =>

          val (first, last) = buffer.splitAt(cursor())
          buffer = (first :+ x.toChar) ++ last
          cursor += 1

          val nextHeight = heightFor(offsetCursor, prompt.length + buffer.length)

          if (nextHeight > areaHeight) {
            Predef.println()
            areaHeight = nextHeight
          }

          redrawLine()
          rec()
      }

    }
    rec()
  }
}
