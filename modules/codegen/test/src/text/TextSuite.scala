package codegen.text

class TextSuite extends munit.FunSuite {
  test("sanitycheck snake2Camel with empty input") {
    val sample = ""
    assertEquals(
      snake2Camel(sample),
      ""
    )
  }
  test("sanitycheck snake2Camel with snake case input") {
    val sample = "foo_bar_baz"
    assertEquals(
      snake2Camel(sample),
      "fooBarBaz"
    )
  }
  test("sanitycheck snake2UpperCamel with snake case input") {
    val sample = "foo_bar_baz"
    assertEquals(
      snake2UpperCamel(sample),
      "FooBarBaz"
    )
  }

  test("sanitycheck snake2Camel with non snake case input") {
    val sample = "Foo"
    assertEquals(
      snake2Camel(sample),
      "Foo"
    )
  }
  test("sanitycheck camelToSnake with empty input") {
    val sample = ""
    assertEquals(
      camelToSnake(sample),
      ""
    )
  }
  test("sanitycheck camelToSnake with abbr") {
    val sample = "HTTP"
    assertEquals(
      camelToSnake(sample),
      "h_t_t_p"
    )
  }
}
