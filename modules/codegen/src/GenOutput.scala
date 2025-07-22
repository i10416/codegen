package codegen

import pseudogo.Term.FnDecl
import pseudogo.Term.StructDecl
import pseudogo.Stmt

case class GenOutput(
    implTypeDecl: StructDecl,
    schemaDecl: FnDecl,
    structDecls: List[StructDecl],
    extras: List[FnDecl],
    attrTypeFuncs: List[FnDecl],
    mappings: (String, List[Stmt], Seq[FnDecl]),
    operations: List[FnDecl]
) {
  def render(): String =
    val (decl, stmts, functions) = mappings
    val toplevel = List(
      List(implTypeDecl.render(0)),
      extras.map(_.render(0)),
      attrTypeFuncs.map(_.render(0)),
      structDecls.map(_.render(0)),
      List(schemaDecl.render(0)),
      functions.map(_.render(0)),
      operations.map(_.render(0))
    ).flatten.mkString("\n")
    toplevel
}
