package ammonite.quasiquote

import scala.annotation.compileTimeOnly
import scala.reflect.api.{Liftables, Universe}
import scala.reflect.macros.blackbox.Context
import scala.language.experimental.macros
case class QTree[Tree, T](cTree: Tree){
  def cTree(c: Context): c.Expr[T] = ???
}
object QTree{
//  def apply[Tree, T](t: T): QTree[Tree, T] = ???
  def fromTree[Tree, T](t: Tree): QTree[Tree, T] = ???
}
case class QName[T](s: String){
  def apply(): T = ???
}
object QQ{
  def apply[T](expr: T)(implicit universe: Universe): QTree[universe.Tree, T] = macro Internal.qqImpl[T]
  def universeExpr[U <: Universe](implicit universe: U): QTree[universe.Tree, U] = macro Internal.universeExprImpl[U]
}
object AST{
  @compileTimeOnly("AST call should only appear inside QQ(...) macro")
  def apply[Tree, T](t: QTree[Tree, T]): T = ???
}
object UQ{
  @compileTimeOnly("UQ call should only appear inside QQ(...) macro")
  def apply[T](t: T): T = ???
}
case class QType(t: Context#Type){
  type Splice = Nothing
}


object Internal{
  def universeExprImpl[U <: Universe: c.WeakTypeTag](c: Context)(universe: c.Expr[U]) = c.Expr[QTree[universe.value.Tree, U]]{
    import c.universe._
    val lifter = QLifters[c.universe.type](c.universe)(new QTree(universe.tree))
    q"new QTree(${lifter.treeLift(universe.tree).t.asInstanceOf[c.Tree]})"
  }
  def qqImpl[T: c.WeakTypeTag](c: Context)(expr: c.Expr[T])(universe: c.Expr[Universe]) = c.Expr[QTree[universe.value.Tree, T]]{
    import c.universe._
    val lifter = QLifters[c.universe.type](c.universe)(new QTree(universe.tree))
    println("EXPRTREE  " + expr.tree)
    val lifted = lifter.treeLift(expr.tree).t.asInstanceOf[c.Tree]
    q"new QTree($lifted)"
  }
}

object QLifters{
  def apply[U0 <: Universe](u0: U0)(universe0: QTree[u0.Tree, U0]) = new QLifters{
    type U = U0
    val u: U = u0
    val universe: QTree[u.Tree, U] = universe0.asInstanceOf[QTree[u.Tree, U]]
  }
}
abstract class QLifters{
  val u: U
  type U <: Universe
  val universe: QTree[u.Tree, U]
  type F[T] = T => QLift
  import u._
  class QLift(val t: Tree)
  implicit def flagSetLift: F[FlagSet] = tree => q"${tree.asInstanceOf[Long]}.asInstanceOf[${universe.cTree}.FlagSet]"


  implicit def modLift: F[Modifiers] =
    tree => thing("Modifiers", tree.flags, tree.privateWithin, tree.annotations)

  implicit def stringLift: F[String] = tree => new QLift(q"$tree")
  implicit def intLift: F[Int] = tree => new QLift(q"$tree")
  implicit def treeSeqLift: F[List[Tree]] = seqLift[Tree]
  implicit def isSeqLift: F[List[ImportSelector]] = seqLift[ImportSelector]
  implicit def treeSeqSeqLift: F[List[List[Tree]]] = seqLift[List[Tree]]
  def seqLift[T](implicit ev: T => QLift): F[List[T]] = tree =>
    new QLift(q"List(..${tree.map(x => ev(x).t)})")

  implicit def nameLift: F[Name] = tree =>
    thing(if (tree.isTermName) "TermName" else "TypeName", tree.encodedName.toString)

  /**
   *  `Constant` instances can wrap certain kinds of these expressions:
   *    1. Literals of primitive value classes (
   *       [[scala.Byte `Byte`]], [[scala.Short `Short`]], [[scala.Int `Int`]],
   *       [[scala.Long `Long`]], [[scala.Float `Float`]], [[scala.Double `Double`]],
   *       [[scala.Char `Char`]], [[scala.Boolean `Boolean`]] and [[scala.Unit `Unit`]]
   *       ) - represented directly as the corresponding type
   *    1. String literals - represented as instances of the `String`.
   *    1. References to classes, typically constructed with [[scala.Predef#classOf]]
   *      - represented as [[scala.reflect.api.Types#Type types]].
   *    1. References to enumeration values
   *      - represented as [[scala.reflect.api.Symbols#Symbol symbols]].
   * @return
   */
  implicit def constantLift: F[Constant] = tree =>
    thing("Constant", new QLift(tree.value match{
      case s: String => q"$s"
      case s: Byte => q"$s"
      case s: Short=> q"$s"
      case s: Int => q"$s"
      case s: Long => q"$s"
      case s: Float => q"$s"
      case s: Double => q"$s"
      case s: Char => q"$s"
      case s: Boolean => q"$s"
      case s: Unit => q"$s"
      case s: Type => q"$s"
      case s: Symbol => q"$s"
    })
    )

  implicit def importSelectorLift: F[ImportSelector] =
    tree => thing("ImportSelector", tree.name, tree.namePos, tree.rename, tree.renamePos)

  def thing(name: String, args: QLift*) = {

    new QLift(q"""${universe.cTree}.${TermName(name)}(..${args.map(_.t)})""")
  }
  implicit def treeLift: F[Tree] = {

    case Apply(fun, args) =>
      fun.symbol.fullName match{
        case "ammonite.quasiquote.AST.apply" => new QLift(q"${args(0)}.cTree")
        case "ammonite.quasiquote.UQ.apply" => new QLift(q"(${args(0)}: lifter.QLift).t")
        case _ => thing("Apply", fun, args)
      }

    case Assign(lhs, rhs)               => thing("Assign", lhs, rhs)
    case Alternative(trees)             => thing("Alternatives", trees)
    case Annotated(annot, arg)          => thing("Annotated", arg)
    case AssignOrNamedArg(lhs, rhs)     => thing("AssignOrNamedArg", lhs, rhs)
    case AppliedTypeTree(tpt, args)     => thing("AppliedTypeTree", tpt, args)
    case Bind(name, body)               => thing("Bind", name, body)
    case Block(stats, expr)             => thing("Block", stats, expr)
    case CaseDef(pat, guard, body)      => thing("CaseDef", pat, guard, body)
    case ClassDef(mods, name, tparams, impl) => thing("ClassDef", mods, name, tparams, impl)
    case CompoundTypeTree(tmpl)         => thing("CompoundTypeTree", tmpl)
    case DefDef(mods, name, tparams, vparamss, tpt, rhs) => thing("DefDef", mods, name, tparams, vparamss, tpt, rhs)
    case ExistentialTypeTree(tpt, whereClauses) => thing("ExistentialTypeTree", tpt, whereClauses)
    case Function(vparams, body)        => thing("Function", vparams, body)
    case Ident(name)                    => thing("Ident", name)
    case If(cond, thenp, elsep)         => thing("If", cond, thenp, elsep)
    case Import(expr, selectors)        => thing("Import", expr, selectors)
    case LabelDef(name, params, rhs)    => thing("LabelDef", name, params, rhs)
    case Literal(value)                 => thing("Literal", value)
    case ModuleDef(mods, name, impl)    => thing("ModuleDef", mods, name, impl)
    case Match(selector, cases)         => thing("Match", selector, cases)
    case New(tpt)                       => thing("New", tpt)
    case PackageDef(pid, stats)         => thing("PackageDef", pid, stats)
    case Return(expr)                   => thing("Return", expr)
    case RefTree(qualifier, name)       => thing("RefTree", qualifier, name)
    case ReferenceToBoxed(ident)        => thing("ReferenceToBoxed", ident)
    case SelectFromTypeTree(qualifier, tree) => thing("SelectFromTypeTree", qualifier, tree)
    case Select(qualifier, name)        => thing("Select", qualifier, name)
    case SingletonTypeTree(ref)         => thing("SingletonTypeTree", ref)
    case Star(elem)                     => thing("Star", elem)
    case Super(qual, mix)               => thing("Super", qual, mix)
    case Template(parents, self, body)  => thing("Template", parents, self, body)
    case Throw(expr)                    => thing("Try", expr)
    case This(qual)                     => thing("This", qual)
    case Try(block, catches, finalizer) => thing("Try", block, catches, finalizer)
    case TypeApply(fun, args)           => thing("TypeApply", fun, args)
    case Typed(expr, tpt)               => thing("Typed", expr, tpt)
    case TypeDef(mods, name, tparams, rhs) => thing("TypeDef", mods, name, tparams, rhs)
    case TypeTree()                     => thing("TypeTree")
    case TypeBoundsTree(lo, hi)         => thing("TypeBoundsTree", lo, hi)
    case UnApply(fun, args)             => thing("UnApply", fun, args)
    case ValDef(mods, name, tpt, rhs)   => thing("ValDef", mods, name, name, tpt, rhs)
  }
}
