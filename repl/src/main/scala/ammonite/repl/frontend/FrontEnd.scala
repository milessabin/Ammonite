package ammonite.repl.frontend

import java.awt.event.{ActionEvent, ActionListener}
import java.io.{IOException, OutputStream, InputStream}
import java.util.Arrays

import ammonite.repl.{Catching, Evaluated, Res}
import ammonite.terminal.LazyList.~:
import ammonite.terminal.{TermState, Term, TermCore}
import fastparse._
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
trait FrontEnd{
  /**
   * The width of the terminal
   */
  def width: Int
  def action(): Res[String]
  def update(r: Res[Evaluated]): Unit
}

object FrontEnd{

  def special(input: InputStream,
              output: OutputStream,
              shellPrompt: => String,
              compilerComplete: => (Int, String) => (Int, Seq[String], Seq[String])) = new FrontEnd{
    val multilineFilter: TermCore.Filter = {
      case TermState(13 ~: rest, b, c) => // Enter
        val code = b.mkString
        ammonite.repl.Splitter.Splitter.parse(code) match {
          case Result.Failure(_, index) if index == code.length =>
            val (first, last) = b.splitAt(c)
            TermState(rest, (first :+ '\n') ++ last, c + 1)
          case _ =>
            ammonite.terminal.Result(code)
        }
    }
    def width = 80
    def action(): Res[String] = {
      TermCore.readLine(shellPrompt, System.in, System.out, multilineFilter orElse Term.defaultFilter)
        .map(Res.Success(_))
        .getOrElse(Res.Exit)
    }
    def update(r: Res[Evaluated]) = {

    }
  }

  def jline(input: InputStream,
            output: OutputStream,
            shellPrompt: => String,
            compilerComplete: => (Int, String) => (Int, Seq[String], Seq[String]),
            initialHistory: Seq[String]): FrontEnd
            = new FrontEnd with Completer {

    val term = new UnixTerminal()
    term.init()

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

    def action(): Res[String] = for {
      _ <- Catching{ case e: UserInterruptException =>

        val buffer = reader.getCursorBuffer
        val current = e.getPartialLine
        val res = Console.BLUE_B + current.split(" ").mkString(Console.RESET + " " + Console.BLUE_B) + Console.RESET
        println(res)

        if (e.getPartialLine == "") {
          reader.println("Ctrl-D to exit")
        }
        Res.Skip
      }

      res <- Option(
        reader.readLine(shellPrompt + " ")
      ).map(Res.Success(_))
        .getOrElse(Res.Exit)

    } yield res

    def update(r: Res[Evaluated]) = r match{

//      case Res.Buffer(line) =>
//        /**
//         * Hack to work around the fact that if nothing got entered into
//         * the prompt, the `ConsoleReader`'s history wouldn't increase
//         */
//        if(line != buffered + "\n") reader.getHistory.removeLast()

      case Res.Success(ev) =>
        val last = reader.getHistory.size()-1
        reader.getHistory.set(last, reader.getHistory.get(last))

      case Res.Exit =>
        // Otherwise the terminal gets left in a funny
        // state after you exit the ammonite REPL
        term.restore()

      case _ =>
    }
  }
}
