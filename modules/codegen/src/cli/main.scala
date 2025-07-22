package codegen.cli

import java.nio.file.Files
import io.circe.parser

@main
def program(args: String*) =
  CodegenCommands.gen.parse(args) match
    case Left(value) =>
      println(value)
      sys.exit(1)
    case Right((location, name, namespace, mode)) =>
      val data = Files.readString(location)
      val schema = parser.parse(data).right.get
      println(codegen.run(schema, name, namespace, mode).render())
