package ammonite.repl.frontend

import java.awt.event.{ActionEvent, ActionListener}
import java.io.{IOException, OutputStream, InputStream}
import java.util.Arrays

import ammonite.repl.{Catching, Evaluated, Res}
import jline.console.completer
import acyclic.file
import jline.UnixTerminal
import jline.console.{ConsoleReader, UserInterruptException}
import jline.console.completer.{CompletionHandler, Completer}
import org.fusesource.jansi.AnsiOutputStream

import scala.annotation.tailrec
import scala.tools.nsc.interpreter._
import collection.JavaConversions._

/**
 * All the mucky JLine interfacing code
 */
trait JLineFrontend{
  /**
   * The width of the terminal
   */
  def width: Int
  def action(buffered: String): Res[String]
  def update(buffered: String, r: Res[Evaluated]): Unit
}

class JLineLite(width: Int,
                input: InputStream,
                output: OutputStream){
  def readLine(prompt: String) = {
    var buffer = Vector.empty[Char]
    var cursor = 0
    output.write(prompt.getBytes)
    @tailrec def rec(): String = {
      input.read() match{
        case 27 => input.read() match{
          case 91 => input.read() match{
            case 65 =>

              output.write("\033[1A".getBytes())
            case 66 =>

              output.write("\033[1B".getBytes())
            case 67 =>
              cursor += 1
              output.write("\033[1C".getBytes())
            case 68 =>
              cursor -= 1
              output.write("\033[1D".getBytes())
          }
            output.flush()
            rec()
          case x =>
            output.flush()
            rec()
        }
        case 13 =>
          output.write(10)
          output.write(13)
          output.flush()
          buffer.mkString
        case x =>
          output.write("\033[9999D".getBytes())
          output.write("\033[2K".getBytes)
          val (first, last) = buffer.splitAt(cursor)
          buffer = (first :+ x.toChar) ++ last
          output.write(prompt.getBytes)
          output.write(buffer.mkString.getBytes)
          cursor += 1
          output.write("\033[9999D".getBytes())
          output.write(s"\033[${cursor + 2 /* prompt width! */}C".getBytes())
          output.flush()
          rec()
      }

    }
    rec()
  }
}
object JLineFrontend{
  def apply(input: InputStream,
            output: OutputStream,
            shellPrompt: => String,
            compilerComplete: => (Int, String) => (Int, Seq[String], Seq[String]),
            initialHistory: Seq[String]): JLineFrontend
            = new JLineFrontend with Completer {



    val term = new UnixTerminal()
//    term.init()




    val reader = new ConsoleReader(input, output, term)

    def width = term.getWidth

    reader.setHistoryEnabled(true)
    reader.addCompleter(this)
    reader.setExpandEvents(false)
    reader.setHandleUserInterrupt(true)
    val defaultHandler = reader.getCompletionHandler
    reader.setCompletionHandler(new CompletionHandler {
      def complete(reader: ConsoleReader, candidates: JList[CharSequence], position: Int): Boolean = {
        if (!signatures.isEmpty){
          println()
          signatures.foreach(reader.println)
          reader.drawLine()
        }
        defaultHandler.complete(reader, candidates, position)
      }
    })
    initialHistory.foreach(reader.getHistory.add)

    var signatures = Seq.empty[String]
    def complete(_buf: String, cursor: Int, candidates: JList[CharSequence]): Int = {
      val buf   = if (_buf == null) "" else _buf
      import collection.JavaConversions._
      val (completionBase, completions, sigs) = compilerComplete(
        cursor,
        buf
      )

      if (!completions.isEmpty) {
        candidates.addAll(completions.sorted)
        signatures = sigs.sorted
      } else if (!sigs.isEmpty){
        println()
        sigs.foreach(reader.println)
        reader.drawLine()
      }

      completionBase
    }

    def history =
      reader.getHistory
            .entries()
            .map(_.value().toString)
            .toVector

    def action(buffered: String): Res[String] = for {
      _ <- Catching{ case e: UserInterruptException =>

        val buffer = reader.getCursorBuffer
        val current = e.getPartialLine
        val res = Console.BLUE_B + current.split(" ").mkString(Console.RESET + " " + Console.BLUE_B) + Console.RESET
        println(res)

        if (e.getPartialLine == "" && buffered == "") {
          reader.println("Ctrl-D to exit")
        }
        Res.Skip
      }

      res <- Option(
        new JLineLite(width, input, output).readLine(
          if (buffered.isEmpty) shellPrompt + " "
          // Strip ANSI color codes, as described http://stackoverflow.com/a/14652763/871202
          else " " * (shellPrompt.replaceAll("\u001B\\[[;\\d]*m", "").length + 1)
        )
      ).map(Res.Success(_))
        .getOrElse(Res.Exit)

    } yield buffered + res

    def update(buffered: String, r: Res[Evaluated]) = r match{

      case Res.Buffer(line) =>
        /**
         * Hack to work around the fact that if nothing got entered into
         * the prompt, the `ConsoleReader`'s history wouldn't increase
         */
        if(line != buffered + "\n") reader.getHistory.removeLast()

      case Res.Success(ev) =>
        val last = reader.getHistory.size()-1
        reader.getHistory.set(last, buffered + reader.getHistory.get(last))

      case Res.Exit =>
        // Otherwise the terminal gets left in a funny
        // state after you exit the ammonite REPL
        term.restore()

      case _ =>
    }
  }
}
