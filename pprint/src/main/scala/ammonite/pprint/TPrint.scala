package ammonite.pprint


import language.experimental.macros
import reflect.macros.blackbox.Context
import scala.reflect.macros.TypecheckException
import scala.tools.nsc.Global
import ammonite.quasiquote._
/**
 * Summoning an implicit `TPrint[T]` provides a pretty-printed
 * string representation of the type `T`, much better than is
 * provided by the default `Type#toString`. In particular
 *
 * - More forms are properly supported and printed
 * - Prefixed Types are printed un-qualified, according to
 *   what's currently in scope
 */
trait TPrint[T]{
  def render(implicit cfg: Config): String
}

object TPrint extends TPrintGen[TPrint, Config] with TPrintLowPri{
  def literal[T](s: String) = new TPrint[T]{
    def render(implicit cfg: Config) = cfg.color.literal(s)
  }
  def lambda[T](f: Config => String) = new TPrint[T]{
    def render(implicit cfg: Config) = f(cfg)
  }
  def make[T](f: Config => String) = TPrint.lambda[T](f)
  def get[T](cfg: Config)(implicit t: TPrint[T]) = t.render(cfg)
  def implicitly[T](implicit t: TPrint[T]): TPrint[T] = t
  implicit val NothingTPrint: TPrint[Nothing] = TPrint.literal("Nothing")
}
trait TPrintLowPri{
  implicit def default[T]: TPrint[T] = macro TPrintLowPri.typePrintImpl[T]
}

object TPrintLowPri{
  def typePrintImpl[T: c.WeakTypeTag](c: Context): c.Expr[TPrint[T]] = {
    implicit val u = c.universe
    val lifter = new QLifters{
      val u: c.universe.type = cm.universe
      val universe = QQ.universeExpr(u)
    }
    import lifter._
    type QTree[T] = ammonite.quasiquote.QTree[c.Tree, T]
    import c.universe._
    // Used to provide "empty string" values in quasiquotes
    val s = ""
    val tpe = weakTypeOf[T]
    def printSymString(s: Symbol) =
      if (s.name.decodedName.toString.startsWith("_$")) "_"
      else s.name.decodedName.toString.stripSuffix(".type")

    def printSym(s: Symbol): QTree[String] = {
      QTree(cfgSym().color.literal(UQ(printSymString(s))))
    }

    def printSymFull(s: Symbol): QTree[String] = {
      if (lookup(s)) printSym(s)
      else QQ(UQ(printSymFull(s.owner)) + "." + UQ(printSym(s)))

    }
    /**
     * Looks up a symbol in the enclosing scope and returns
     * whether it exists in scope by the same name
     */
    def lookup(s: Symbol) = {
      val cas = c.asInstanceOf[reflect.macros.runtime.Context]
      val g = cas.global
      val gName = s.name.asInstanceOf[g.Name]
      val lookedUps = for(n <- Stream(gName.toTermName, gName.toTypeName)) yield {
        cas.callsiteTyper
          .context
          .lookupSymbol(n, _ => true)
          .symbol
      }

      if (!s.isType) lookedUps.contains(s)
      else {
        // Try to resolve aliases for types
        lookedUps.exists(x => x == s || x.tpe.typeSymbol == s.asInstanceOf[g.Symbol].tpe.typeSymbol)
      }
    }

    def prefixFor(pre: Type, sym: Symbol): QTree[String] = {
      // Depending on what the prefix is, you may use `#`, `.`
      // or even need to wrap the prefix in parentheses
      val sep = pre match{
        case x if x.toString.endsWith(".type") => QQ(AST(rec0(pre)) + ".")
        case x: TypeRef => QQ(cfgSym().color.literal(AST(implicitRec(pre))) + "#")
        case x: SingleType => QQ(cfgSym().color.literal(AST(rec0(pre))) + "." )
        case x: ThisType => QQ(cfgSym().color.literal(AST(rec0(pre))) + "." )
        case x => QQ("(" + AST(implicitRec(pre)) + ")#" )
      }

      val prefix = if (!lookup(sym)) sep else QQ("")
      QQ(AST(prefix) + AST(printSym(sym)))
    }


    def printArgSyms(args: List[Symbol]): QTree[String] = {
      def added = args.map{x =>
        val TypeBounds(lo, hi) = x.info
        QQ(AST(printSym(x)) +  AST(printBounds(lo, hi)))
      }.reduceLeft[QTree[String]]((l, r) => QQ(AST(l) + ", " + AST(r)))
      if (args == Nil) QQ("") else QQ("[" + AST(added) + "]" )
    }
    def printArgs(args: List[Type]): QTree[String] = {
      def added = args.map(implicitRec(_))
        .reduceLeft[QTree[String]]((l, r) => QQ(AST(l) + ", " + AST(r)))

      if (args == Nil) QQ("") else QQ("[" + AST(added) + "]" )
    }


    def implicitRec(tpe: Type): QTree[String] = {
      val evidence: QTree[TPrint[T]] = QTree.fromTree(c.inferImplicitValue(tpe))
      try {
        // Make sure the type isn't higher-kinded or some other weird
        // thing, and actually can fit inside the square brackets
        c.typecheck(q"null.asInstanceOf[$tpe]")
        QQ(ammonite.pprint.TPrint.implicitly(AST(evidence)).render(cfgSym()))
      }catch{case e: TypecheckException =>
        rec0(tpe)
      }
    }

    def printBounds(lo: Type, hi: Type): QTree[String] = {
      val loTree = if (lo =:= typeOf[Nothing]) QQ("") else QQ(" >: " + AST(implicitRec(lo)) )
      val hiTree = if (hi =:= typeOf[Any]) QQ("") else QQ(" <: " + AST(implicitRec(hi)))
      QQ(AST(loTree) + AST(hiTree))
    }

    def showRefinement(quantified: List[Symbol]): Option[QTree[String]] = {
      def stmts = for{
        t <- quantified
        suffix <- t.info match {
          case PolyType(typeParams, resultType) =>
            val paramTree = printArgSyms(t.asInstanceOf[TypeSymbol].typeParams)
            val resultBounds = if (resultType =:= typeOf[Any]) QQ("") else QQ( " <: " + AST(implicitRec(resultType)) )
            Some(QQ(AST(paramTree) + AST(resultBounds)))
          case TypeBounds(lo, hi) if t.toString.contains("$") && lo =:= typeOf[Nothing] && hi =:= typeOf[Any] =>
            None
          case TypeBounds(lo, hi) =>
            Some( printBounds(lo, hi) )
        }
      } yield {
          if (t.toString.endsWith(".type")) {
            val TypeBounds(lo, hi) = t.info
            val RefinedType(parents, defs) = hi
            val filtered = internal.refinedType(parents.filter(x => !(x =:= typeOf[scala.Singleton])), defs)
            QQ("val " + cfgSym().color.literal(UQ(t.name.toString.stripSuffix(".type"))) + ": " + AST(implicitRec(filtered)))
          }else {
            QQ("type " + AST(printSym(t)) + AST(suffix) )
          }
        }
      if (stmts.length == 0) None
      else Some(stmts.reduceLeft((l, r) => QQ(AST(l) + "; " + AST(r))))
    }
    /**
     * Decide how to pretty-print, based on the type.
     *
     * This is recursive, but we only rarely use direct recursion: more
     * often, we'll use `implicitRec`, which goes through the normal
     * implicit search channel and can thus
     */
    def rec0(tpe: Type, end: Boolean = false): QTree[String] = tpe match {
      case TypeBounds(lo, hi) =>
        val res = printBounds(lo, hi)
        QQ( "_" + AST(res) )
      case ThisType(sym) =>
        QQ(AST(printSymFull(sym)) + UQ(if(sym.isPackage || sym.isModuleClass) "" else ".this.type"))

      case SingleType(NoPrefix, sym)    => QQ(AST(printSym(sym)) + UQ(if (end) ".type" else ""))
      case SingleType(pre, sym)         => QQ(AST(prefixFor(pre, sym)) + UQ(if (end) ".type" else ""))
        // Special-case operator two-parameter types as infix
      case TypeRef(pre, sym, List(left, right))
        if lookup(sym) && sym.name.encodedName.toString != sym.name.decodedName.toString =>

        QQ(AST(implicitRec(left)) + " " + AST(printSym(sym)) + " " + AST(implicitRec(right)))

      case TypeRef(NoPrefix, sym, args) => QQ(AST(printSym(sym)) + AST(printArgs(args)))
      case TypeRef(pre, sym, args)      => QQ(AST(prefixFor(pre, sym)) + AST(printArgs(args)))
      case et @ ExistentialType(quantified, underlying) =>
        showRefinement(quantified) match{
          case None => implicitRec(underlying)
          case Some(block) => QQ(AST(implicitRec(underlying)) + " forSome { " + AST(block) +  " }")
        }
      case AnnotatedType(annots, tp)    =>
        val annotTrees =
          annots.map(x => QQ(" @" + AST(implicitRec(x.tpe))))
                .reduceLeft((x, y) => QQ(AST(x) + AST(y)))
        QQ(AST(implicitRec(tp)) + AST(annotTrees))
      case RefinedType(parents, defs) =>
        val pre =
          if (parents.forall(_ =:= typeOf[AnyRef])) QQ("")
          else parents.map(implicitRec(_)).reduceLeft[QTree[String]]((l, r) => QQ(AST(l)  + " with " + AST(r)))
        QQ(AST(pre) + UQ(if (defs.isEmpty) "" else "{" + defs.mkString(";") + "}"))
    }
    lazy val cfgSym = QName[Config]("cfg")
    QQ(ammonite.pprint.TPrint.lambda[T]{
      ($cfgSym: ammonite.pprint.Config) => AST(rec0(tpe, end = true))
    }).cTree(c)
  }

}
