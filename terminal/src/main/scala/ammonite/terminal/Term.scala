package ammonite.terminal
import java.io.{OutputStream, InputStream}

import scala.annotation.tailrec
object Term{
  def main(args: Array[String]): Unit = {
    val t = new Term(System.in, System.out)
    t.init()
    while(true){
      println(t.readLine("@ "))
    }
  }
}
class Ansi(output: OutputStream){
  def control(n: Int, c: Char) = output.write(s"\033[$n$c".getBytes)

  /**
   * Move up `n` squares
   */
  def up(n: Int) = control(n, 'A')
  /**
   * Move down `n` squares
   */
  def down(n: Int) = control(n, 'B')
  /**
   * Move right `n` squares
   */
  def right(n: Int) = control(n, 'C')
  /**
   * Move left `n` squares
   */
  def left(n: Int) = control(n, 'D')

  /**
   * Clear the screen
   *
   * n=0: clear from cursor to end of screen
   * n=1: clear from cursor to start of screen
   * n=2: clear entire screen
   */
  def clearScreen(n: Int) = control(n, 'K')
  /**
   * Clear the current line
   *
   * n=0: clear from cursor to end of line
   * n=1: clear from cursor to start of line
   * n=2: clear entire line
   */
  def clearLine(n: Int) = control(n, 'K')
}
class Term(input: InputStream,
                output: OutputStream){
  val ansi = new Ansi(output)
  var initialConfig = ""
  var width = 0
  var height = 0
  def stty(s: String) = {
    import sys.process._
    Seq("bash", "-c", s"stty $s < /dev/tty").!!
  }
  def init() = {
    val raw = stty("-a")

    width = "(\\d+) columns;".r.findFirstMatchIn(raw).get.group(1).toInt
    height = "(\\d+) rows;".r.findFirstMatchIn(raw).get.group(1).toInt
    println("Initializing, Width " + width)
    stty("-icanon min 1 -icrnl -inlcr -ixon")
    stty("dsusp undef")
    stty("-echo")

    initialConfig = stty("-g").trim
  }
  def restore() = {
    stty(initialConfig)
  }

  def readLine(prompt: String) = {
    var buffer = Vector.empty[Char]
    var cursor = 0
    var bufHeight = 0
    def redrawLine() = {
      for(i <- 0 until ((cursor + 2) / width)){
        ansi.clearLine(2)
        ansi.up(1)
      }
      ansi.clearLine(2)
      ansi.left(9999)
      output.write(prompt.getBytes)
      output.write(buffer.mkString.getBytes)
      ansi.left(9999)
      ansi.up((cursor + 2) / width)
      ansi.right((cursor + 2) % width)
      ansi.down((cursor + 2) / width)
      output.flush()
    }
    output.write(prompt.getBytes)
    @tailrec def rec(): String = {
      input.read() match{
        case 27 => input.read() match{
          case 91 =>
            input.read() match{
              case 65 =>
                ansi.up(1)
              case 66 =>
                ansi.down(1)
              case 67 =>
                cursor += 1
                ansi.right(1)
              case 68 =>
                cursor -= 1
                ansi.left(1)
            }
            output.flush()
            rec()

          case x =>
            output.flush()
            rec()
        }
        case 13 | 10 => // Enter
          output.write(10)
          output.write(13)
          output.flush()
          buffer.mkString
        case 127 => // Backspace
          val (first, last) = buffer.splitAt(cursor)
          buffer = first.dropRight(1) ++ last
          cursor -= 1
          redrawLine()
          rec()
        case x =>

          val (first, last) = buffer.splitAt(cursor)
          buffer = (first :+ x.toChar) ++ last
          cursor += 1

          redrawLine()
          rec()
      }

    }
    rec()
  }
}
