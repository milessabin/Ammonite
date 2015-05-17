package ammonite.terminal

import java.io.{FileOutputStream, OutputStream}


object Debug{
  val debugOutput= new FileOutputStream(new java.io.File("log"))
  def log(s: Any) = {
    debugOutput.write((System.currentTimeMillis() + "\t\t" + s + "\n").getBytes)
  }
}
import Debug.log
class Ansi(output: OutputStream){
  def control(n: Int, c: Char) = output.write(s"\033[$n$c".getBytes)

  /**
   * Moves to the desired row and column, using individual
   * cursor movements. `0` is in the top/left counting up
   * towards the bottom/right, and `-1` is in the bottom/right
   * counting down towards the top/left
   */
  def moveTo(row: Int, col: Int) = {
    if (row >= 0) {
      up(9999)
      down(row)
    }else{
      down(9999)
      up(-1-row)
    }
    if (col >= 0) {
      left(9999)
      right(col)
    }else{
      right(9999)
      left(-1-col)
    }
  }
  /**
   * Move up `n` squares
   */
  def up(n: Int) = if (n == 0) "" else control(n, 'A')
  /**
   * Move down `n` squares
   */
  def down(n: Int) = if (n == 0) "" else control(n, 'B')
  /**
   * Move right `n` squares
   */
  def right(n: Int) = if (n == 0) "" else control(n, 'C')
  /**
   * Move left `n` squares
   */
  def left(n: Int) = if (n == 0) "" else control(n, 'D')

  /**
   * Clear the screen
   *
   * n=0: clear from cursor to end of screen
   * n=1: clear from cursor to start of screen
   * n=2: clear entire screen
   */
  def clearScreen(n: Int) = control(n, 'J')
  /**
   * Clear the current line
   *
   * n=0: clear from cursor to end of line
   * n=1: clear from cursor to start of line
   * n=2: clear entire line
   */
  def clearLine(n: Int) = control(n, 'K')
}
object TTY{
  def init() = {
    val raw = stty("-a")

    val width = "(\\d+) columns;".r.findFirstMatchIn(raw).get.group(1).toInt
    val height = "(\\d+) rows;".r.findFirstMatchIn(raw).get.group(1).toInt
    log("Initializing, Width " + width)
    stty("-icanon min 1 -icrnl -inlcr -ixon")
    stty("dsusp undef")
    stty("-echo")

    val initialConfig = stty("-g").trim
    (width, height, initialConfig)
  }
  def stty(s: String) = {
    import sys.process._
    Seq("bash", "-c", s"stty $s < /dev/tty").!!
  }
  def restore(initialConfig: String) = {
    stty(initialConfig)
  }
}