package ammonite.repl.frontend

import java.awt.event.{ActionEvent, ActionListener}
import java.io.{IOException, OutputStream, InputStream}
import java.util.Arrays

import ammonite.repl.{Catching, Evaluated, Res}
import ammonite.terminal.LazyList.~:
import ammonite.terminal.{Debug, TermState, Term, TermCore}
import fastparse._
import jline.console.completer
import acyclic.file
import jline.UnixTerminal
import jline.console.{ConsoleReader, UserInterruptException}
import jline.console.completer.{CompletionHandler, Completer}
import org.fusesource.jansi.AnsiOutputStream

import scala.annotation.tailrec
import scala.collection.mutable
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
    def highlight(buffer: Vector[Char], cursor: Int) = {
      val indices = {
        var indices = collection.SortedSet.empty[(Int, Vector[Char])](Ordering.by(_._1))
        ammonite.repl.Splitter.Splitter.parse(buffer.mkString, instrumenter = (name, idx, res) => {
          val resIndex = res() match {
            case s: Result.Success[_] => s.index
            case f: Result.Failure if f.index == buffer.length => f.index
            case _ => -1
          }
          if (resIndex != -1) {
            def doThing(color: String, endColor: String = Console.RESET) = {
              if (resIndex > idx) {
                ammonite.terminal.Debug(name, idx)
                indices = indices.filter(_._1 < resIndex) ++ Seq((idx, color.toVector), (resIndex, endColor.toVector))
              }
            }
            if (name == scalaparse.Scala.Literals.Comment) doThing(Console.BLUE)
            if (name == scalaparse.Scala.ExprLiteral) doThing(Console.GREEN)
            if (name == scalaparse.Scala.IdBinding) doThing(Console.CYAN)
            if (name == scalaparse.Scala.VarId) doThing(Console.CYAN)
            if (name.toString.head == '`' && name.toString.last == '`') {
              import scalaparse.syntax.Identifiers._
              val shortened = name.toString.drop(1).dropRight(1)
              if (alphaKeywords.contains(shortened) || symbolKeywords.contains(shortened))
                doThing(Console.YELLOW)
              else
                doThing(Console.WHITE)
            }
            if (name == scalaparse.Scala.Type) doThing(Console.GREEN)
            //            Debug("NAME " + name.toString)

            if (name.toString == "Interp") doThing(Console.RESET, Console.GREEN)
          }
        })
        indices.toSeq
      }
      // Make sure there's an index right at the start and right at the end! This
      // resets the colors at the snippet's end so they don't bleed into later output
      val boundedIndices = Seq((0, Console.RESET.toVector)) ++ indices ++ Seq((9999, Console.RESET.toVector))

      val buffer2 =
        boundedIndices
          .sliding(2)
          .flatMap{case Seq((s, c1), (e, c2)) => c1 ++ buffer.slice(s, e) }
          .toVector

      (buffer2 , cursor)
    }
    def action(): Res[String] = {
      TermCore.readLine(
        shellPrompt,
        System.in,
        System.out,
        multilineFilter orElse Term.defaultFilter,
        displayTransform = highlight
      )
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
