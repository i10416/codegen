package pseudogo

import pseudogo.Term.FnDecl
import pseudogo.Term.If
import pseudogo.Term.Ident
import pseudogo.Term.LitBool
import pseudogo.Term.ValDef
class RenderingExtraSuite extends munit.FunSuite {
  test("selecting ident syntax generates equivalent AST") {
    val plain =
      Ident("foo").select(Ident("bar")).select(Ident("baz")).apply(Ident("qux"))
    val short = (i("foo") \\ "bar" \\ "baz")(i("qux"))
    assertEquals(plain, short)
  }
  test("selecting ident can be represented with backslashes") {
    val selected = (i("foo") \\ "bar" \\ "baz")(i("qux"))
    assertEquals(
      selected.render(0),
      "foo.bar.baz(qux)"
    )
  }
  test("ValDef represents new value definition") {
    val tree = ValDef("foo", LitBool(false))
    assertEquals(
      tree.render(0),
      """
      |foo := false
      |""".stripMargin.trim()
    )
  }
  test(":= represents assigning value to the existing identifier") {
    val expr = i("foo").selected := LitBool(true)
    assertEquals(
      expr.render(0),
      """
      |foo = true
      |""".stripMargin.trim()
    )
  }
  test("render if") {
    val stmt = Term.If((i("foo") \\ "bar" \\ "baz")())(Term.Ret())
    assertEquals(
      stmt.render(0),
      """
      |if foo.bar.baz() {
      |  return
      |}""".stripMargin.trim()
    )
  }
}
