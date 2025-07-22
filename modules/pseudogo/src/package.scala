package pseudogo

def i(name: String) = Term.Ident(name)
def q(s: String): String = s"\"$s\""
private def indent(level: Int): String = " " * (2 * level)

sealed trait Term {
  def render(level: Int): String
}
sealed trait Type extends Term {
  def render(level: Int): String = this match
    case tpe: TypeIdent       => tpe.fullName
    case Ptr(tpe)   => s"*${tpe.fullName}"
    case Slice(tpe) => s"[]${tpe.fullName}"
}

final case class TypeIdent(namespace: List[String], shortName: String)
    extends Type {
  def fullName = (namespace :+ shortName).mkString(".")
  override def render(level: Int): String = fullName
}
object TypeIdent {
  def apply(ns: String, shortName: String): TypeIdent =
    TypeIdent(ns :: Nil, shortName)
}

case class Ptr(tpe: TypeIdent) extends Type
case class Slice(tpe: TypeIdent) extends Type

sealed trait Stmt extends Term
sealed trait Expr extends Stmt {
  def `...`: Term.Spread = Term.Spread(this)
  def asRef: Term.AsRef = Term.AsRef(this)
}

object Term {
  case class AsRef(e: Expr) extends Term {
    def render(level: Int): String = "&" + e.render(level)
  }

  case class LitBool(b: Boolean) extends Expr {
    def render(level: Int = 0): String = s"$b"
  }
  case class LitStr(s: String) extends Expr {
    def render(level: Int = 0): String = q(s)
  }
  case class Ident(name: String) extends Term {
    def selected: Select = Select(Nil, this)
    def render(level: Int = 0): String = name
    def select(selectee: String): Select = Select(List(this), Ident(selectee))
    def select(selectee: Ident): Select = Select(List(this), selectee)
    def \\(selectee: Ident): Select = Select(List(this), selectee)
    def \\(selectee: String): Select = Select(List(this), Ident(selectee))
  }
  object Ident {
    val underscore = Ident("_")
  }
  case class Select(paths: List[Ident] = Nil, path: Ident) extends Expr {
    def apply(args: Term*): Apply = Apply(this, args.toList)
    def \\(selectee: String): Select =
      Select(this.paths :+ path, Ident(selectee))
    def \\(selectee: Ident): Select =
      Select(this.paths :+ path, selectee)
    def select(selectee: String): Select =
      Select(this.paths :+ path, Ident(selectee))
    def select(selectee: Ident): Select =
      Select(this.paths :+ path, selectee)
    def render(level: Int): String =
      (paths :+ path).map(_.render(0)).mkString(".")
    def :=(rhs: Expr): Assign = Assign(this, rhs)
  }
  object Select {
    def apply(paths: String*): Select =
      paths.toList match
        case Nil         => throw new Exception("paths must not be empty")
        case head :: Nil => Ident(head).selected
        case head :: remains =>
          Select((head :: remains.init).map(Ident(_)), Ident(remains.last))
  }

  case class StructDecl(
      name: TypeIdent,
      fields: List[Field]
  ) extends Term {
    def render(level: Int): String =
      (
        List(s"type ${name.shortName} struct {") ::
          fields.map(field => indent(1) + field.render(0)) ::
          List("}") ::
          Nil
      ).flatten.mkString("\n")
  }
  case class Field(
      name: String,
      tpe: Type,
      tags: List[(String, String)],
      docs: Option[String]
  ) extends Term {
    def render(level: Int): String =
      val ts =
        tags.map { case (key, value) => s"`$key:${q(value)}`" }.mkString(" ")
      val ds = docs.fold("")(" // " + _)
      s"$name ${tpe.render(level)} $ts$ds"
  }
  case class FnDecl(
      recv: Option[(Option[String], Type)],
      name: String,
      args: List[(String, Type)],
      ret: List[Type],
      body: Block
  ) {
    def render(level: Int = 0): String =
      val argsPart = args
        .map { case (ident, tpe) =>
          s"$ident ${tpe.render(level)}"
        }
        .mkString(", ")
      val maybeRecv = recv match
        case Some((Some(recv), tpe)) => s" ($recv ${tpe.render(level)})"
        case Some((None, tpe))       => s" (${tpe.render(level)})"
        case None                    => ""
      val retPart = ret.map(_.render(level)).mkString(", ")
      s"func$maybeRecv $name($argsPart) $retPart" + body.render(level)
  }
  object FnDecl {
    def apply(
        name: String,
        args: List[(String, Type)],
        ret: List[Type] = Nil
    )(body: Stmt*): FnDecl =
      FnDecl(None, name, args, ret, Block(body*))
  }
  case object GNil extends Term {
    def render(level: Int): String = "nil"
  }
  case class Spread(term: Term) extends Term {
    def render(level: Int): String = term.render(level) + "..."
  }
  case class Apply(sel: Select, args: List[Term]) extends Expr {
    def render(level: Int): String =
      s"${sel.render(level)}(${args.map(_.render(level)).mkString(", ")})"
  }

  case class Ret(term: Option[Term] = None) extends Stmt {
    def render(level: Int = 0): String =
      term match
        case Some(term) => s"return ${term.render(level)}"
        case None       => "return"
  }
  object Ret {
    def apply(term: Term): Ret = Ret(Some(term))
  }

  case class ValDef(lhs: List[Ident], rhs: Expr) extends Stmt {
    def render(level: Int = 0): String =
      s"${lhs.map(_.render(0)).mkString(", ")} := ${rhs.render(level)}"
  }
  extension (lhs: List[Ident]) def :=(rhs: Expr): ValDef = ValDef(lhs, rhs)
  object ValDef {
    def apply(lhs: String, rhs: Expr): ValDef = ValDef(Ident(lhs) :: Nil, rhs)
    def apply(lhs: Ident, rhs: Expr): ValDef = ValDef(lhs :: Nil, rhs)
  }
  case class Assign(lhs: Select, rhs: Expr) extends Stmt {
    def render(level: Int = 0): String =
      s"${lhs.render(level)} = ${rhs.render(level)}"
  }
  case class Attrs(attrs: (String, Expr)*) extends Term {
    def ++(another: Attrs): Attrs = Attrs((this.attrs ++ another.attrs)*)
    def render(level: Int = 0): String =
      if attrs.isEmpty then "{}"
      else
        List(
          "{",
          attrs
            .map { case (name, value) =>
              indent(level + 1) + s"$name: " + value.render(level + 1)
            }
            .mkString("", ",\n", ","),
          indent(level) + "}"
        ).mkString("\n")
  }
  object Attrs {
    def empty = Attrs()
  }
  sealed trait Attr extends Term
  case class Eval(v: String) extends Expr {
    def render(level: Int) = v
  }
  case class Init(owner: TypeIdent, v: Attrs = Attrs.empty) extends Expr {
    def render(level: Int) = owner.fullName + v.render(level)
  }
  case class If(cond: Expr, body: Block) extends Stmt {
    def render(level: Int = 0) =
      s"if ${cond.render(level)} " + body.render(level)
  }
  object If {
    def apply(cond: Expr)(stmts: Stmt*): If = If(cond, Block(stmts*))
  }

  case class For(
      valdef: ValDef,
      cond: Option[(List[Ident] => Term)] = None,
      op: Option[(List[Ident] => Stmt)] = None
  )(body: List[Ident] => List[Stmt])
      extends Stmt {
    def render(level: Int): String =
      val forPart = List(
        Some(s"for ${valdef.render(level)}"),
        cond.map(f => f(valdef.lhs).render(level)),
        op.map(f => f(valdef.lhs).render(level))
      ).flatten.mkString("; ")
      val blc = Block(body(valdef.lhs)*)
      s"$forPart " + blc.render(level)
  }

  case class Block(entries: Stmt*) extends Term {
    def render(level: Int = 0): String =
      List(
        "{",
        entries
          .map { entry =>
            indent(level + 1) + entry.render(level + 1)
          }
          .mkString("\n"),
        indent(level) + "}"
      ).mkString("\n")
  }
}
