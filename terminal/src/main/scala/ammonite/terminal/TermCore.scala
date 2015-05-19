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
  def calculateHeight(buffer: Vector[Char],
                      cursor: Int,
                      width: Int,
                      promptLength: Int): Int = {
    val frags = mutable.Buffer.empty[Vector[Char]]
    frags.append(Vector.empty)
    for(c <- buffer){
      if (c == '\n') frags.append(Vector.empty)
      else frags(frags.length - 1) = frags(frags.length - 1) :+ c
    }
    // -1 because the first frag has no newline before it
    var cumLength = -1
    val fragHeights = for(frag <- frags) yield {
      val fragLength = (
        // Length of the original text
        frag.length

        // Don't forget the prompt! It moves everything to the right
        + (if (frag == frags.head) promptLength else 0)
      )

      // +1 to account for the newline between frags
      cumLength += fragLength + 1

      // If the cursor is at the end, it needs one more spot

      // We only want to count the last line if there is at
      // least one character on that line

      val height = (fragLength + (if ((cursor + promptLength) == cumLength) 1 else 0) - 1) / width + 1
      height
    }
    fragHeights.sum
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

      Debug("DOWN " + (transformedCursor + noAnsiPrompt.length) / width)
      Debug("RIGHT " + (transformedCursor + noAnsiPrompt.length) % width)
      ansi.down((transformedCursor + noAnsiPrompt.length) / width)
      ansi.right((transformedCursor + noAnsiPrompt.length) % width)
      writer.flush()
    }


    @tailrec def rec(lastState: TermState, areaHeight: Int): Option[String] = {
      redrawLine(lastState.buffer, lastState.cursor)
      filters(TermInfo(lastState, width)) match {
        case TermState(s, b, c) =>
          val newCursor = math.max(math.min(c, b.length), 0)

          val nextHeight = calculateHeight(b, newCursor, width, noAnsiPrompt.length)
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
