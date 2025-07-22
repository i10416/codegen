package pseudogo

import pseudogo.Term.StructDecl
import pseudogo.Term.Field

class RenderingStructDeclSuite extends munit.FunSuite {
  test("empty struct") {
    val struct = StructDecl(
      TypeIdent("ns", "Empty"),
      Nil
    )
    assertEquals(
      struct.render(0),
      """
      |type Empty struct {
      |}""".stripMargin.trim()
    )
  }
  test("struct with a field") {
    val struct = StructDecl(
      TypeIdent("ns", "S"),
      List(
        Field("f", TypeIdent("ns", "T"), List(("key","value")), None)
      )
    )
    assertEquals(
      struct.render(0),
      """
      |type S struct {
      |  f ns.T `key:"value"`
      |}""".stripMargin.trim()
    )
  }
}
