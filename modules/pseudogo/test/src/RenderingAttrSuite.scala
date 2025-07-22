package pseudogo

import pseudogo.Term.Attrs
import pseudogo.Term.LitBool
import pseudogo.Term.LitStr

class RenderingAttrSuite extends munit.FunSuite {
  test("empty attributes") {
    val empty = Attrs.empty
    assertEquals(
      empty.render(0),
      "{}"
    )
  }
  test("non-empty flat attributes") {
    val attrs = Attrs(("bool", LitBool(true)), ("str", LitStr("str")))
    assertEquals(
      attrs.render(0),
      """
      |{
      |  bool: true,
      |  str: "str",
      |}
      |""".stripMargin.trim
    )
  }
}
