package pseudogo

import pseudogo.Term.FnDecl
class RenderingFnDeclSuite extends munit.FunSuite {
  test("render an empty function") {
    val empty = FnDecl("empty", Nil, Nil)()
    assertEquals(
      empty.render(0),
      """
       |func empty() {
       |
       |}""".stripMargin.trim
    )
  }
  test("render a function with an argument") {
    val empty = FnDecl("f", ("arg", TypeIdent("ns", "Tpe")) :: Nil, Nil)()
    assertEquals(
      empty.render(0),
      """
       |func f(arg ns.Tpe) {
       |
       |}""".stripMargin.trim
    )
  }
}
