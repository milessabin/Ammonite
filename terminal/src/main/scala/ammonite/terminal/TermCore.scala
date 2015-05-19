package ammonite.terminal

import java.io.{OutputStreamWriter, InputStreamReader, OutputStream, InputStream}

import scala.annotation.tailrec
import scala.collection.mutable

/**
 * The core logic around a terminal; it defines the base `filters` API
 * through which anything (including basic cursor-navigation and typing)
 * interacts with the terminal.
 *
 * Maintains basic invariants, such as "cursor should always be within
 * the buffer", and "ansi terminal should reflect most up to date TermState"
 */
object TermCore {
  /*
    width = 2
    0 -> 1
    1 -> 1
    2 -> 1
    3 -> 2
    4 -> 2
    5 -> 3
     */
  def fragHeight(length: Int, width: Int) = math.max(1, (length + 1) / width)

  /**
   * Given a buffer with characters and newlines, calculates how high
   * the buffer is and where the cursor goes inside of it.
   */
  def calculateHeight(buffer: Vector[Char],
                      cursor: Int,
                      width: Int,
                      prompt: String): (Int, Int, Int) = {

    val frags = mutable.Buffer.empty[Vector[Char]]
    frags.append(prompt.toVector)
    for(c <- buffer){
      if (c == '\n') frags.append(Vector.empty)
      else frags(frags.length - 1) = frags(frags.length - 1) :+ c
    }
    val promptCursor = cursor + prompt.length

    val fragHeights =
      frags
        .inits
        .toVector
        .reverse // We want shortest-to-longest, inits gives longest-to-shortest
        .filter(_.nonEmpty) // Without the first empty prefix
        .map{ x =>
          fragHeight(
            x.last.length + (if (x.map(_.length).sum + x.length - 1 == promptCursor) 1 else 0),
            width
          )

        }
    println("fragHeights " + fragHeights)
    val totalHeight = fragHeights.sum

    var leftoverCursor = promptCursor
    println("leftoverCursor " + leftoverCursor)
    var totalPrefix = 0
    var totalPreHeight = 0
    var done = false
    for(i <- 0 until frags.length if !done) {
      val delta = frags(i).length + (if (i == frags.length - 1) 0 else 1) // length of frag and the '\n' after it
      println("delta " + delta)
      val nextCursor = leftoverCursor - delta
      if (nextCursor >= 0) {
        println("nextCursor " + nextCursor)
        leftoverCursor = nextCursor
        totalPrefix += delta
        totalPreHeight += fragHeights(i)
      }else done = true
    }
    println("totalPreHeight " + totalPreHeight)
    println("leftoverCursor " + leftoverCursor)
    println("totalPrefix " + totalPrefix)
    val cursorY = math.max(0, totalPreHeight - 1) + leftoverCursor / width
    val cursorX = leftoverCursor % width
    (totalHeight, cursorY, cursorX)
  }

  type Filter = PartialFunction[TermInfo, TermAction]

  /**
   * Blockingly reads a line from the given input stream and returns it.
   *
   * @param prompt The prompt to display when requesting input
   * @param input The input-stream where characters come in, e.g. System.in
   * @param output The output-stream where print-outs go, e.g. System.out
   * @param filters A set of actions that can be taken depending on the input,
   *                to manipulate the internal state of the terminal.
   * @param displayTransform code to manipulate the display of the buffer and
   *                         cursor, without actually changing the logical
   *                         values inside them.
   */
  def readLine(prompt: String,
               input: InputStream,
               output: OutputStream,
               filters: PartialFunction[TermInfo, TermAction] = PartialFunction.empty,
               displayTransform: (Vector[Char], Int) => (Vector[Char], Int) = (x, i) => (x, i))
               : Option[String] = {

    val noAnsiPrompt = prompt.replaceAll("\u001B\\[[;\\d]*m", "")
    def redrawLine(buffer: Vector[Char], cursor: Int) = {
      ansi.restore()
      ansi.clearScreen(0)
      Debug("CURSOR " + cursor)
      val (transformedBuffer, transformedCursor) = displayTransform(buffer, cursor)
      Debug("TransCURSOR " + transformedCursor)
      writer.write(prompt)
      writer.write(transformedBuffer.toArray)
      writer.flush()
      ansi.restore()
      val (nextHeight, cursorY, cursorX) = calculateHeight(buffer, cursor, width, noAnsiPrompt)
      Debug("DOWN " + cursorY)
      Debug("RIGHT " + cursorX)
      ansi.down(cursorY)
      ansi.right(cursorX)
      writer.flush()
    }


    @tailrec def rec(lastState: TermState, areaHeight: Int): Option[String] = {
      redrawLine(lastState.buffer, lastState.cursor)
      filters(TermInfo(lastState, width)) match {
        case TermState(s, b, c) =>
          val newCursor = math.max(math.min(c, b.length), 0)

          val (nextHeight, cursorY, cursorX) = calculateHeight(b, newCursor, width, noAnsiPrompt)
          if (nextHeight > areaHeight) {
            Predef.println()
            ansi.restore()
            ansi.up(1)
            ansi.save()
          }
          rec(TermState(s, b, newCursor), nextHeight)
        case Result(s) =>
          redrawLine(lastState.buffer, 9999)
          writer.write(10)
          writer.write(13)
          writer.flush()
          Some(s)
        case Exit =>
          None
      }
    }
    lazy val reader = new InputStreamReader(input)
    lazy val writer = new OutputStreamWriter(output)
    lazy val ansi = new Ansi(output)
    lazy val (width, height, initialConfig) = TTY.init()
    try {
      ansi.save()
      rec(TermState(LazyList.continually(reader.read()), Vector.empty, 0), 1)
    }finally{
      // Don't close these! Closing these closes stdin/stdout,
      // which seems to kill the entire program

      // reader.close()
      // writer.close()
      TTY.stty(initialConfig)
    }
  }
}

case class TermInfo(ts: TermState, width: Int)

sealed trait TermAction
case class TermState(inputs: LazyList[Int], buffer: Vector[Char], cursor: Int) extends TermAction
object TermState{
  def unapply(ti: TermInfo): Option[(LazyList[Int], Vector[Char], Int)] = {
    TermState.unapply(ti.ts)
  }
  def unapply(ti: TermAction): Option[(LazyList[Int], Vector[Char], Int)] = ti match{
    case ts: TermState => TermState.unapply(ts)
    case _ => None
  }

}
case object Exit extends TermAction
case class Result(s: String) extends TermAction
