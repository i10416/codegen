package codegen.cli
import com.monovore.decline.Opts
import com.monovore.decline.Command
import java.nio.file.Path
import codegen.Mode
import cats.syntax.all.*
object CodegenCommands:
  val schemaLocation = Opts.argument[Path]("JSONSchema path")
  val model = Opts.argument[String]("model")
  val mode = Opts
    .option[String]("mode", "either datasource or resource", "m")
    .mapValidated:
      case "resource"   => Mode.Resource.validNel
      case "datasource" => Mode.DataSource.validNel
      case mode =>
        s"Unexpected $mode. Expected either resource or datasource.".invalidNel
    .withDefault(Mode.DataSource)
  val namespace = Opts.option[String]("namespace", "module namespace", "ns")
  val dest = Opts.option[Path]("dest", "destination path", "d")
  val gen = Command(
    "codegen",
    "terraform provider generator",
    true
  )(
    (schemaLocation, model, namespace, mode).tupled
  )
