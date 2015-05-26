package ammonite.quasiquote

import scala.reflect.api.{Liftables, Universe}
import scala.reflect.macros.blackbox.Context

case class QTree[T](cTree: Context#Tree){
  def cTree(c: Context): c.Expr[T] = ???
}
object QTree{
  def apply[T](t: T): QTree[T] = ???
  def fromTree[T](t: Context#Tree): QTree[T] = ???
}
case class QName[T](s: String){
  def apply(): T = ???
}
object QQ{
  def apply[T](t: T): QTree[T] = ???
}
object AST{
  def apply[T](t: QTree[T]): T = ???
}
object UQ{
  def apply[T](t: T): T = ???
}
case class QType(t: Context#Type){
  type Splice = Nothing
}
object Internal{
  def qqImpl[T: c.WeakTypeTag](c: Context)(expr: c.Expr[T]) = c.Expr[QTree[T]]{
    val u = c.universe
    import c.universe._
    implicit val flagSetLift: Liftable[FlagSet] = Liftable{tree =>
      ???
      //      q"u.Modifiers(${tree.flags}, ${tree.privateWithin}, ${tree.annotations})"
    }

    def modLift: Liftable[Modifiers] = Liftable{tree =>
      q"u.Modifiers(${flagSetLift(tree.flags)}, ${nameLift(tree.privateWithin)}, ${tree.annotations})"
    }

    def nameLift: Liftable[Name] = Liftable{ tree =>
      if (tree.isTermName) q"u.TermName(${tree.decodedName.toString})"
      else q"u.TypeName(${tree.decodedName.toString})"
    }

    def importSelectorLift: Liftable[ImportSelector] = Liftable{ tree =>
      q"u.ImportSelector(${nameLift(tree.name)}, ${tree.namePos}}, ${nameLift(tree.rename)}}, ${tree.renamePos}})"
    }

    def treeLift(tree: Tree): Tree = tree match{
      case ClassDef(mods, name, tparams, impl) =>
        q"""u.ClassDef(${modLift(mods)}, ${nameLift(name)}, ${tparams.map(treeLift(_))}, ${treeLift(impl)})"""
      case PackageDef(pid: RefTree, stats: List[Tree]) =>
        q"""u.PackageDef($pid, ..$stats)"""
      case ModuleDef(mods: Modifiers, name: TermName, impl: Template) =>
        q"u.ModuleDef(${modLift(mods)}, ${nameLift(name)}, ${treeLift(impl)})"
      case ValDef(mods: Modifiers, name: TermName, tpt: Tree, rhs: Tree) =>
        q"u.ValDef(${modLift(mods)}, ${nameLift(name)}, ${treeLift(tpt)}, ${treeLift(rhs)})"
      case DefDef(mods: Modifiers, name: TermName, tparams: List[TypeDef], vparamss: List[List[ValDef]], tpt: Tree, rhs: Tree) =>
        q"u.DefDef(${modLift(mods)}, ${nameLift(name)}, ${tparams.map(treeLift(_))}, ${vparamss.map(_.map(treeLift(_)))}, ${treeLift(tpt)}, ${treeLift(rhs)})"
      case TypeDef(mods: Modifiers, name: TypeName, tparams: List[TypeDef], rhs: Tree) =>
        q"u.TypeDef(${modLift(mods)}, ${nameLift(name)}, ${tparams.map(treeLift(_))}, ${treeLift(rhs)})"
      case LabelDef(name: TermName, params: List[Ident], rhs: Tree) => 
        q"u.LabelDef(${nameLift(name)}, ${params.map(treeLift(_))}, ${treeLift(rhs)})"
      case Import(expr: Tree, selectors: List[ImportSelector]) =>
        q"u.Import(${treeLift(expr)}, ${selectors.map(importSelectorLift(_))})"
      case tree: Template => ???

      case tree: Block => ???

      case tree: CaseDef => ???

      case tree: Alternative => ???

      case tree: Star => ???

      case tree: Bind => ???

      case tree: UnApply => ???

      case tree: Function => ???

      case tree: Assign => ???

      case tree: AssignOrNamedArg => ???

      case tree: If => ???

      case tree: Match => ???

      case tree: Return => ???

      case tree: Try => ???

      case tree: Throw => ???

      case tree: New => ???

      case tree: Typed => ???

      case tree: TypeApply => ???

      case tree: Apply => ???

      case tree: Super => ???

      case tree: This => ???

      case tree: Select => ???

      case tree: Ident => ???

      case tree: RefTree => ???

      case tree: ReferenceToBoxed => ???

      case tree: Literal => ???

      case tree: TypeTree => ???

      case tree: Annotated => ???

      case tree: SingletonTypeTree => ???

      case tree: SelectFromTypeTree => ???

      case tree: CompoundTypeTree => ???

      case tree: AppliedTypeTree => ???

      case tree: TypeBoundsTree => ???

      case tree: ExistentialTypeTree => ???
    }

    treeLift(expr.tree).asInstanceOf[c.Tree]

  }
}