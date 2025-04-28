//> using dep io.circe::circe-core::0.14.13
//> using dep io.circe::circe-parser::0.14.13
//> using dep io.circe::circe-optics::0.15.0
//> using dep org.typelevel::cats-core::2.13.0

import J.SchemaDefinition
import J.SchemaDefinition.AnyStructuralType
import J.SchemaDefinition.ArrayDefinition
import J.SchemaDefinition.BoolType
import J.SchemaDefinition.IntEnum
import J.SchemaDefinition.IntegerType
import J.SchemaDefinition.NumberType
import J.SchemaDefinition.ObjectType
import J.SchemaDefinition.PrimitiveType
import J.SchemaDefinition.Reference
import J.SchemaDefinition.StringEnum
import J.SchemaDefinition.StringType
import Mode.DataSource
import Mode.Resource
import Term.ValDef
import TypeAnnot.Direct
import TypeAnnot.Ptr
import cats.data.*
import cats.syntax.all.*
import cats.syntax.alternative.*
import io.circe.*
import io.circe.Decoder.Result
import io.circe.DecodingFailure.Reason
import io.circe.Printer
import io.circe.derivation.Configuration
import io.circe.derivation.ConfiguredCodec
import io.circe.derivation.ConfiguredEnumCodec
import io.circe.optics.*
import io.circe.optics.JsonPath.*
import io.circe.parser
import io.circe.syntax.*

import java.nio.file.Files
import java.nio.file.Path

private def resolve(schema: Json, ref: Reference): ObjectType =
  root.components.schemas
    .selectDynamic(ref.derefName)
    .json
    .getOption(schema)
    .get
    .as[ObjectType] match
    case Left(e)    => throw new Exception(e)
    case Right(obj) => obj

def schemaNamingConvention(schema: Json, value: SchemaDefinition): String =
  value match
    case _: IntEnum    => "Int32Attribute"
    case _: StringEnum => "StringAttribute"
    case _: StringType => "StringAttribute"
    case _: BoolType   => "BoolAttribute"
    case i: IntegerType =>
      if i.format.exists(_ == "int32") then "Int32Attribute"
      else if i.format.exists(i => Set("int64", "timestamp").contains(i)) then
        "Int64Attribute"
      else "Int32Attribute"
    case n: NumberType =>
      if n.format.exists(_ == "float") then "Float32Attribute"
      else if n.format.exists(_ == "double") then "Float64Attribute"
      else "Float32Attribute"
    case _: Reference =>
      "SingleNestedAttribute"
    case l: ArrayDefinition =>
      if l.nested then "ListNestedAttribute" else "ListAttribute"
    case obj: ObjectType if obj.nested        => "SingleNestedAttribute"
    case _: ObjectType | _: AnyStructuralType => "ObjectAttribute"

def indent(level: Int): String = " " * (2 * level)

sealed trait Mode {
  def modelNameSuffix: String = this match
    case Mode.DataSource => "DataSourceModel"
    case Mode.Resource   => "ResourceModel"

  def namespace: String = this match
    case DataSource => "datasource"
    case Resource   => "resource"
  def implementee = this match
    case DataSource => TypeIdent(namespace, "DataSource")
    case Resource   => TypeIdent(namespace, "Resource")
  def metadataRequest = TypeIdent(namespace, "MetadataRequest")
  def metadataResponse = TypeIdent(namespace, "MetadataResponse")
  def configureRequest = TypeIdent(namespace, "ConfigureRequest")
  def configureResponse = TypeIdent(namespace, "ConfigureResponse")
  def schemaRequest = TypeIdent(namespace, "SchemaRequest")
  def schemaResponse = TypeIdent(namespace, "SchemaResponse")
  def readRequest = TypeIdent(namespace, "ReadRequest")
  def readResponse = TypeIdent(namespace, "ReadResponse")

}
object Mode {
  case object DataSource extends Mode
  case object Resource extends Mode
}

def attrTypeFuncNameConvention(typeName: String): String =
  s"${typeName}AttrType"

def attrTypeFunRec(
    schema: Json,
    typeName: String,
    focus: ObjectType,
    ctx: (Map[String, String], Map[String, Term.FnDecl])
): (Map[String, String], Map[String, Term.FnDecl]) =
  val fName = attrTypeFuncNameConvention(typeName)
  val ctx_ = (ctx._1.updated(typeName, fName), ctx._2)
  val ((functionTable, functionRef), mapEntries) =
    focus.properties.foldLeft((ctx_, List.empty[(String, Expr)])) {
      case (((fTable, fRef), fields), (name, property)) =>
        property match
          case p: (IntEnum | StringEnum | PrimitiveType) =>
            (
              (fTable, fRef),
              fields :+
                camelToSnake(name) -> Term.Eval(
                  s"${leafTypeTranslation(p).get.fullName}Type"
                )
            )
          case ref: Reference =>
            fTable.get(ref.derefName) match
              case Some(fn) =>
                (
                  (fTable, fRef),
                  fields :+ camelToSnake(name) -> Term.Select(fn)()
                )
              case None =>
                val fn = attrTypeFuncNameConvention(ref.derefName)
                val (tbl, newFRef) = attrTypeFunRec(
                  schema,
                  ref.derefName,
                  resolve(schema, ref),
                  (fTable.updated(ref.derefName, fn), fRef)
                )
                (
                  (tbl, newFRef),
                  fields :+ camelToSnake(name) -> Term.Select(fn)()
                )
          case ArrayDefinition(_, ref: Reference, _) =>
            fTable.get(ref.derefName) match
              case Some(fn) =>
                (
                  (fTable, fRef),
                  fields :+
                    camelToSnake(name) ->
                    Term.Init(
                      TypeIdent("types", "ListType"),
                      Term.Attrs("ElemType" -> Term.Select(fn)())
                    )
                )
              case None =>
                val fn = attrTypeFuncNameConvention(ref.derefName)
                val (tbl, newFRef) = attrTypeFunRec(
                  schema,
                  ref.derefName,
                  resolve(schema, ref),
                  (fTable.updated(ref.derefName, fn), fRef)
                )
                (
                  (tbl, newFRef),
                  fields :+
                    camelToSnake(name) -> Term.Init(
                      TypeIdent("types", "ListType"),
                      Term.Attrs("ElemType" -> Term.Select(fn)())
                    )
                )
          case ArrayDefinition(_, items, _) if items.isLeaf =>
            (
              (fTable, fRef),
              fields :+
                camelToSnake(name) ->
                Term.Init(
                  TypeIdent("types", "ListType"),
                  Term.Attrs(
                    "ElemType" ->
                      Term.Eval(
                        s"${leafTypeTranslation(items).get.fullName}Type"
                      )
                  )
                )
            )
          case ArrayDefinition(_, _, _)             => ???
          case _: ObjectType | _: AnyStructuralType => ((fTable, fRef), fields)
    }
  val f = Term.FnDecl(
    None,
    fName,
    Nil,
    List(Direct(TypeIdent("attr", "Type"))),
    Term.Block(
      Term.Ret(
        Term.Init(
          TypeIdent("basetypes", "ObjectType"),
          Term.Attrs(
            "AttrTypes" ->
              Term.Init(
                TypeIdent("map[string]attr", "Type"),
                Term.Attrs(
                  (mapEntries.map { case (k, v) => (q(k), v) })*
                )
              )
          )
        )
      )
    )
  )

  (functionTable, functionRef.updated(fName, f))

private def schemaTypeFromSchema(schema: Json, value: SchemaDefinition) =
  TypeIdent("schema", schemaNamingConvention(schema, value))

private def renderAttributes(
    schema: Json,
    schemaDefn: ObjectType,
    level: Int,
    path: List[String],
    mode: Mode
): List[(String, Expr)] =
  val attributes = schemaDefn.properties.map { case (key, value) =>
    val tfsdkName = camelToSnake(key)
    val openDefn_ : (String, Expr) = (
      s"$tfsdkName",
      Term.Init(
        schemaTypeFromSchema(schema, value),
        Term.Attrs(
          List(
            Option.when(!schemaDefn.strictPresence(key))(
              ("Optional", Term.LitBool(true))
            ),
            Some(("Computed", Term.LitBool(true))),
            value.description
              .map(_.linesIterator.mkString(" ").trim())
              .map(d =>
                (
                  "MarkdownDescription",
                  Term.Eval(
                    s"\"${d.replace("\"", "\\\"")}\""
                  )
                )
              ),
            value.asStringEnum
              .map: enums =>
                enums.`enum`
                  .map(variant => s""""$variant"""")
                  .mkString("stringvalidator.OneOf(", ",", "),")
              .map: validator =>
                (
                  "Validators",
                  Term.Eval(s"[]validator.String{$validator}")
                ),
            value.asRef.map: ref =>
              (
                "Attributes",
                Term.Init(
                  TypeIdent("map[string]schema", "Attribute"),
                  if path.contains(ref.derefName) then
                    Term.Attrs(
                      (
                        renderAttributes(
                          schema,
                          ObjectType(
                            "object",
                            None,
                            None,
                            None,
                            Map.empty,
                            None
                          ),
                          level + 2,
                          path,
                          mode
                        ).map((k, v) => (q(k), v))
                      )*
                    )
                  else
                    Term.Attrs(
                      (
                        renderAttributes(
                          schema,
                          resolve(schema, ref),
                          level + 2,
                          path,
                          mode
                        ).map((k, v) => (q(k), v))
                      )*
                    )
                )
              ),
            value.asList.map: arrayDefn =>
              if arrayDefn.nested then
                (
                  "NestedObject",
                  Term.Init(
                    TypeIdent("schema", "NestedAttributeObject"),
                    Term.Attrs(
                      "Attributes" ->
                        Term.Init(
                          // FIXME
                          TypeIdent("map[string]schema", "Attribute"),
                          Term.Attrs(
                            ((arrayDefn.items match
                              case obj: ObjectType =>
                                renderAttributes(
                                  schema,
                                  obj,
                                  level + 3,
                                  path,
                                  mode
                                )
                              case ref: Reference =>
                                renderAttributes(
                                  schema,
                                  resolve(schema, ref),
                                  level + 3,
                                  ref.derefName :: path,
                                  mode
                                )
                            ).map((k, v) => (q(k), v)))*
                          )
                        )
                    )
                  )
                )
              else
                val t = arrayDefn.items.tpe.get.capitalize
                "ElementType" -> Term.Init(TypeIdent("basetypes", s"${t}Type"))
          ).flatten*
        )
      )
    )

    openDefn_
  }
  attributes.toList

def genMutationBoilerplates(
    mode: Mode,
    name: String,
    stateDecl: String,
    mappings: List[Stmt]
): List[Term.FnDecl] =
  mode match
    case DataSource => Nil
    case Resource => {
      val implName = mode match
        case DataSource => name + "DataSource"
        case Resource   => name + "Resource"
      val implType = TypeIdent(Nil, implName)
      val selectData = i("req") \\ "State" \\ "Get"
      val readDataToModel =
        (i("resp") \\ "Diagnostics" \\ "Append")(
          selectData(
            i("ctx"),
            Term.Eval("&state")
          ).`...`
        )
      val returnOnError =
        Term.If((i("resp") \\ "Diagnostics" \\ "HasError")())(Term.Ret())
      val writeModelToData =
        (i("resp") \\ "Diagnostics" \\ "Append")(
          (i("resp") \\ "State" \\ "Set")(
            i("ctx"),
            Term.Eval("&state")
          ).`...`
        )
      val puts =
        for (action <- List("Create", "Update"))
          yield Term.FnDecl(
            Some(("r", Ptr(implType))),
            action,
            List(
              ("ctx", Direct(TypeIdent("context", "Context"))),
              ("req", Direct(TypeIdent(mode.namespace, action + "Request"))),
              ("resp", Ptr(TypeIdent(mode.namespace, action + "Response")))
            ),
            Nil,
            Term.Block(
              (Term.Eval(stateDecl) ::
                readDataToModel ::
                returnOnError ::
                mappings.appended(writeModelToData))*
            )
          )
      val del = Term.FnDecl(
        Some(("r", Ptr(implType))),
        "Delete",
        List(
          ("ctx", Direct(TypeIdent("context", "Context"))),
          ("req", Direct(TypeIdent(mode.namespace, "Delete" + "Request"))),
          ("resp", Ptr(TypeIdent(mode.namespace, "Delete" + "Response")))
        ),
        Nil,
        Term.Block(
          (Term.Eval(stateDecl) ::
            readDataToModel ::
            returnOnError ::
            Nil)*
        )
      ) :: Nil
      val imports = Term.FnDecl(
        Some(("r", Ptr(implType))),
        "ImportState",
        List(
          ("ctx", Direct(TypeIdent("context", "Context"))),
          ("req", Direct(TypeIdent(mode.namespace, "ImportState" + "Request"))),
          (
            "resp",
            Direct(TypeIdent(mode.namespace, "ImportState" + "Response"))
          )
        ),
        Nil,
        Term.Block()
      ) :: Nil
      puts ::: del ::: imports
    }

def genRead(
    mode: Mode,
    name: String,
    stateDecl: String,
    mappings: List[Stmt]
): Term.FnDecl =
  val implName = mode match
    case DataSource => name + "DataSource"
    case Resource   => name + "Resource"
  val implType = TypeIdent(Nil, implName)
  val selectData = mode match
    case DataSource => i("req") \\ "Config" \\ "Get"
    case Resource   => i("req") \\ "State" \\ "Get"

  val readDataToModel =
    (i("resp") \\ "Diagnostics" \\ "Append")(
      selectData(
        i("ctx"),
        Term.Eval("&state")
      ).`...`
    )
  val returnOnError =
    Term.If((i("resp") \\ "Diagnostics" \\ "HasError")())(Term.Ret())
  val writeModelToData =
    (i("resp") \\ "Diagnostics" \\ "Append")(
      (i("resp") \\ "State" \\ "Set")(
        i("ctx"),
        Term.Eval("&state")
      ).`...`
    )
  Term.FnDecl(
    Some(("r", Ptr(implType))),
    "Read",
    List(
      ("ctx", Direct(TypeIdent("context", "Context"))),
      ("req", Direct(mode.readRequest)),
      ("resp", Direct(mode.readResponse))
    ),
    Nil,
    Term.Block(
      (Term.Eval(stateDecl) ::
        readDataToModel ::
        returnOnError ::
        mappings.appended(writeModelToData))*
    )
  )
def i(name: String) = Term.Ident(name)

object J {

  sealed trait SchemaDefinition {
    def explicitNullable: Boolean = nullable.getOrElse(false)
    def nullable: Option[Boolean]
    def tpe: Option[String]
    def asRef: Option[Reference] = this match
      case self: Reference => Some(self)
      case _               => None
    def asObject: Option[ObjectType] = this match
      case self: ObjectType => Some(self)
      case _                => None
    def asStringEnum: Option[StringEnum] = this match
      case self: StringEnum => Some(self)
      case _                => None
    def asList: Option[ArrayDefinition] = this match
      case self: ArrayDefinition => Some(self)
      case _                     => None

    def description: Option[String]
    def isRef: Boolean = this match
      case _: Reference => true
      case _            => false
    def isObject: Boolean = this match
      case _: ObjectType => true
      case _             => false
    def isObjectLike: Boolean = this match
      case _: ObjectType                                               => true
      case _: Reference                                                => true
      case _: AnyStructuralType                                        => true
      case _: (IntEnum | StringEnum | PrimitiveType | ArrayDefinition) => false
    def isList: Boolean = this match
      case _: ArrayDefinition => true
      case _                  => false

    def nested: Boolean = this match
      case _: IntEnum | _: StringEnum | _: PrimitiveType => false
      case obj: ObjectType =>
        obj.properties.values.exists(schema => schema.isObjectLike)
      case _: AnyStructuralType  => false
      case _: Reference          => true
      case list: ArrayDefinition => list.items.nested

    def isLeaf: Boolean = this match
      case _: IntEnum | _: StringEnum | _: PrimitiveType => true
      case _                                             => false
  }
  object SchemaDefinition {
    sealed trait EnumDefinition extends SchemaDefinition
    implicit val codec: Decoder[SchemaDefinition] =
      new Decoder[SchemaDefinition] {
        def apply(c: HCursor): Result[SchemaDefinition] =
          (c.get[String]("$ref"), c.get[String]("type")) match
            case (Right(ref), Left(_)) => Decoder[Reference].apply(c)
            case (Left(_), Right(tpe)) =>
              if tpe == "array" then Decoder[ArrayDefinition].apply(c)
              else if tpe == "object" then
                if c.downField("properties").succeeded then
                  Decoder[ObjectType].apply(c)
                else
                  Right(
                    AnyStructuralType(
                      c.value,
                      c.get[String]("description").toOption,
                      c.get[Boolean]("nullable").toOption
                    )
                  )
              else if tpe == "string" then
                (Decoder[StringEnum].widen <+> Decoder[StringType].widen)
                  .apply(c)
              else if tpe == "integer" then
                (Decoder[IntEnum].widen <+> Decoder[IntegerType].widen).apply(c)
              else
                Decoder[
                  PrimitiveType
                ].apply(c)
            case (Left(_), Left(_)) =>
              Left(
                DecodingFailure(
                  Reason.CustomReason("neither type nor $ref found"),
                  c.root
                )
              )
            case (Right(_), Right(_)) =>
              Left(
                DecodingFailure(
                  Reason.CustomReason(
                    "property must be either one of type or reference. Not both."
                  ),
                  c.root
                )
              )
      }
    case class IntEnum(
        `type`: String,
        description: Option[String],
        format: Option[String],
        `enum`: List[Int],
        example: Option[String],
        nullable: Option[Boolean]
    ) extends EnumDefinition derives Codec {
      def tpe: Option[String] = Some(`type`)
    }

    case class StringEnum(
        `type`: String,
        description: Option[String],
        format: Option[String],
        `enum`: List[String],
        example: Option[String],
        nullable: Option[Boolean]
    ) extends EnumDefinition derives Codec {
      def tpe: Option[String] = Some(`type`)

    }

    case class IntegerType(
        `type`: String,
        format: Option[String],
        description: Option[String],
        example: Option[Int],
        nullable: Option[Boolean]
    ) extends PrimitiveType {
      def tpe: Option[String] = Some(`type`)
    }
    object IntegerType {
      implicit val codec: Codec[IntegerType] =
        Codec.forProduct5[IntegerType, String, Option[String], Option[
          String
        ], Option[Int], Option[Boolean]](
          "type",
          "format",
          "description",
          "example",
          "nullable"
        )(IntegerType.apply(_, _, _, _, _))(p =>
          (p.`type`, p.format, p.description, p.example, p.nullable)
        )
    }
    case class NumberType(
        `type`: String,
        format: Option[String],
        description: Option[String],
        example: Option[Double],
        nullable: Option[Boolean]
    ) extends PrimitiveType {
      def tpe: Option[String] = Some(`type`)
    }
    object NumberType {
      implicit val codec: Codec[NumberType] =
        Codec.forProduct5[NumberType, String, Option[String], Option[
          String
        ], Option[Double], Option[Boolean]](
          "type",
          "format",
          "description",
          "example",
          "nullable"
        )(NumberType.apply(_, _, _, _, _))(p =>
          (p.`type`, p.format, p.description, p.example, p.nullable)
        )
    }

    case class StringType(
        `type`: String,
        format: Option[String],
        description: Option[String],
        example: Option[String],
        nullable: Option[Boolean]
    ) extends PrimitiveType {
      def tpe: Option[String] = Some(`type`)
    }
    object StringType {
      implicit val codec: Codec[StringType] =
        Codec.forProduct5[StringType, String, Option[String], Option[
          String
        ], Option[String], Option[Boolean]](
          "type",
          "format",
          "description",
          "example",
          "nullable"
        )(StringType.apply(_, _, _, _, _))(p =>
          (p.`type`, p.format, p.description, p.example, p.nullable)
        )
    }
    case class BoolType(
        `type`: String,
        description: Option[String],
        nullable: Option[Boolean]
    ) extends PrimitiveType {
      def tpe: Option[String] = Some(`type`)
    }
    object BoolType {
      implicit val codec: Codec[BoolType] =
        Codec.forProduct3[BoolType, String, Option[String], Option[Boolean]](
          "type",
          "description",
          "nullable"
        )(BoolType.apply(_, _, _))(p => (p.`type`, p.description, p.nullable))
    }

    sealed trait PrimitiveType extends SchemaDefinition
    object PrimitiveType {
      implicit val de: Decoder[PrimitiveType] = new Decoder[PrimitiveType] {
        def apply(c: HCursor): Result[PrimitiveType] =
          c.get[String]("type") match {
            case Left(value)      => Left(value)
            case Right("integer") => Codec[IntegerType].decodeJson(c.value)
            case Right("number")  => Codec[NumberType].decodeJson(c.value)
            case Right("string")  => Codec[StringType].decodeJson(c.value)
            case Right("boolean") => Codec[BoolType].decodeJson(c.value)
          }
      }
    }
    case class ObjectType(
        `type`: String,
        description: Option[String],
        example: Option[String],
        required: Option[List[String]],
        properties: Map[String, SchemaDefinition],
        nullable: Option[Boolean]
    ) extends SchemaDefinition derives Decoder {
      def strictPresence(key: String): Boolean =
        properties.keySet.contains(key) &&
          required.getOrElse(Nil).contains(key) &&
          properties(key).nullable.forall(!_)

      def tpe: Option[String] = Some(`type`)
    }
    case class AnyStructuralType(
        value: Json,
        description: Option[String],
        nullable: Option[Boolean]
    ) extends SchemaDefinition {
      def tpe: Option[String] = Some("object")
    }

    case class Reference(ref: String, nullable: Option[Boolean])
        extends SchemaDefinition {
      def derefName: String = ref.stripPrefix("#/components/schemas/")
      def description: Option[String] = None
      def tpe: Option[String] = None
    }
    object Reference {
      def fromName(name: String, nullable: Option[Boolean] = None): Reference =
        Reference(s"#/components/schemas/$name", nullable)
      implicit val codec: Codec[Reference] =
        Codec.forProduct2[Reference, String, Option[Boolean]](
          "$ref",
          "nullable"
        )(Reference(_, _))(p => (p.ref, p.nullable))
    }

    case class ArrayDefinition(
        `type`: String,
        items: SchemaDefinition,
        nullable: Option[Boolean]
    ) extends SchemaDefinition derives Decoder {
      def description: Option[String] = None

      def tpe: Option[String] = Some(`type`)

    }
  }
}

def snake2Camel(str: String) =
  val elements = str.split("_")
  val (head, tail) = (elements.head, elements.tail)
  if tail.isEmpty then head
  else head + tail.map(_.capitalize).mkString

def snake2UpperCamel(str: String) = snake2Camel(str).capitalize

final case class TypeIdent(namespace: List[String], shortName: String) {
  def fullName = (namespace :+ shortName).mkString(".")
}
object TypeIdent {
  def apply(ns: String, shortName: String): TypeIdent =
    TypeIdent(ns :: Nil, shortName)
}

sealed trait Term {
  def render(level: Int): String
}
sealed trait Stmt extends Term
sealed trait Expr extends Stmt {
  def `...`: Term.Spread = Term.Spread(this)
}

object Term {
  case class Comment(str: String) extends Term {
    def render(level: Int): String =
      str.linesIterator.map(line => indent(level) + "// " + line).mkString("\n")
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
      tpe: TypeAnnot,
      tags: List[(String, String)],
      docs: Option[String]
  ) extends Term {
    def render(level: Int): String =
      val ts =
        tags.map { case (key, value) => s"`$key:${q(value)}`" }.mkString(" ")
      val ds = docs.fold("")(" // " + _)
      s"$name ${tpe.fullName} $ts$ds"
  }
  case class FnDecl(
      recv: Option[(String, TypeAnnot)],
      name: String,
      args: List[(String, TypeAnnot)],
      ret: List[TypeAnnot],
      body: Block
  ) {
    def render(level: Int = 0): String =
      val argsPart = args
        .map { case (ident, tpe) =>
          s"$ident ${tpe.fullName}"
        }
        .mkString(", ")
      val maybeRecv = recv match
        case Some((recv, tpe)) => s" ($recv ${tpe.fullName})"
        case None              => ""
      val retPart = ret.map(_.fullName).mkString(", ")
      s"func$maybeRecv $name($argsPart) $retPart" + body.render(level)
  }
  object FnDecl {
    def apply(
        name: String,
        args: List[(String, TypeAnnot)],
        ret: List[TypeAnnot] = Nil
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

def modelNameConvention(
    schemaName: String,
    owner: Option[TypeIdent] = None,
    mode: Mode
): TypeIdent =
  TypeIdent(
    Nil,
    owner.fold("")(_.shortName) + snake2UpperCamel(
      schemaName
    ) + mode.modelNameSuffix
  )
def generateModelsRec(
    schema: Json,
    model: TypeIdent,
    modelDefn: ObjectType,
    history: Map[TypeIdent, List[Term.Field]],
    path: List[TypeIdent],
    mode: Mode
): Map[TypeIdent, List[Term.Field]] =
  modelDefn.properties.foldLeft(history) { case (history, (name, property)) =>
    val fieldName = snake2UpperCamel(name)
    val tfsdkName = camelToSnake(name)
    val fieldOptions = List(("tfsdk", tfsdkName))
    val docs = property.description
      .map(_.linesIterator.mkString(" ").trim())

    property match
      case unnamedObject: ObjectType =>
        ???
      case obj: AnyStructuralType =>
        history
      case ref: Reference =>
        val modelName = modelNameConvention(ref.derefName, mode = mode)
        val ptrMark = "*"
        val field = Term.Field(fieldName, Ptr(modelName), fieldOptions, docs)
        if history.contains(modelName) then
          history.updatedWith(model)(
            _.map(_ :+ field).orElse(Some(field :: Nil))
          )
        else
          generateModelsRec(
            schema,
            modelNameConvention(ref.derefName, mode = mode),
            resolve(schema, ref),
            history + (modelNameConvention(ref.derefName, mode = mode) -> Nil),
            path :+ modelName,
            mode = mode
          ).updatedWith(model)(_.map(_ :+ field).orElse(Some(field :: Nil)))
      case ArrayDefinition(tpe, items, nullable) if items.isLeaf =>
        val field =
          Term.Field(
            fieldName,
            TypeAnnot.Slice(leafTypeTranslation(items).get),
            fieldOptions,
            docs
          )
        history.updatedWith(model)(_.map(_ :+ field).orElse(Some(field :: Nil)))
      case ArrayDefinition(tpe, unnamedObject: ObjectType, nullable) =>
        val syntheticName =
          modelNameConvention(fieldName, Some(model), mode = mode)
        val field = Term.Field(
          fieldName,
          TypeAnnot.Slice(syntheticName),
          fieldOptions,
          docs
        )
        generateModelsRec(
          schema,
          syntheticName,
          unnamedObject,
          history + (syntheticName -> Nil),
          path :+ syntheticName,
          mode
        ).updatedWith(model)(_.map(_ :+ field).orElse(Some(field :: Nil)))
      case ArrayDefinition(tpe, ref: Reference, nullable) =>
        val modelName = modelNameConvention(ref.derefName, mode = mode)

        val field =
          Term.Field(fieldName, TypeAnnot.Slice(modelName), fieldOptions, docs)
        generateModelsRec(
          schema,
          modelName,
          resolve(schema, ref),
          history + (modelName -> Nil),
          path :+ modelName,
          mode
        ).updatedWith(model)(_.map(_ :+ field).orElse(Some(field :: Nil)))
      case leaf: (IntEnum | StringEnum | PrimitiveType) =>
        val field = Term.Field(
          fieldName,
          Direct(leafTypeTranslation(leaf).get),
          fieldOptions,
          docs
        )

        history.updatedWith(model)(_.map(_ :+ field).orElse(Some(field :: Nil)))
      case schemaDefn =>
        throw new Exception(s"Unexpected SchemaDefinition: ${schemaDefn}")
  }

sealed trait TypeAnnot {
  def fullName: String = this match
    case Ptr(tpe)             => s"*${tpe.fullName}"
    case Direct(tpe)          => s"${tpe.fullName}"
    case TypeAnnot.Slice(tpe) => s"[]${tpe.fullName}"
}
object TypeAnnot {
  case class Ptr(tpe: TypeIdent) extends TypeAnnot
  case class Direct(tpe: TypeIdent) extends TypeAnnot
  case class Slice(tpe: TypeIdent) extends TypeAnnot
}

def propertyCanBeAbsent(
    owner: ObjectType,
    propName: String,
    property: SchemaDefinition
): Boolean =
  property.explicitNullable || !owner.required.exists(_.contains(propName))

def generateModelMappingRec(
    schema: Json,
    modelIdent: String,
    modelDefn: ObjectType,
    history: (
        List[Stmt],
        Map[(TypeIdent, TypeIdent), String],
        Map[String, Term.FnDecl]
    ),
    mode: Mode,
    schemaTypeNamespace: List[String]
): (
    List[Stmt],
    Map[(TypeIdent, TypeIdent), String],
    Map[String, Term.FnDecl]
) =
  modelDefn.properties.foldLeft(history) {
    case ((statements, functionTable, functionRef), (name, property)) =>
      val fieldName = snake2UpperCamel(name)
      val nullable = propertyCanBeAbsent(modelDefn, name, property)
      val select: Expr =
        property match
          case i: IntEnum =>
            val cast = i.format.fold("int32") {
              case "int64" | "timestamp" if nullable => "(*int64)"
              case "int64" | "timestamp"             => "int64"
              case _ if nullable                     => "(*int32)"
              case _                                 => "int32"
            }
            Term.Select(cast)(Term.Ident("data") \\ fieldName)
          case int: IntegerType =>
            int.format match
              case None =>
                Term.Select("int32")(i("data") \\ fieldName)
              case Some("timestamp") =>
                Term.Select("int64")(i("data") \\ fieldName)
              case _ =>
                i("data") \\ fieldName
          case (s: StringEnum) =>
            val cast =
              if nullable then "(*string)"
              else "string"
            Term.Select(cast)(i("data") \\ fieldName)
          case _ => i("data") \\ fieldName
      property match
        case p @ (_: PrimitiveType | _: IntEnum | _: StringEnum) =>
          val typeMapping = leafTypeTranslation(p).get
          val maybePtr =
            if nullable then "Pointer"
            else ""
          val statement_ =
            Term.Select(modelIdent, fieldName) := Term.Eval(
              s"${typeMapping.fullName}${maybePtr}Value(${select.render(0)})"
            )
          (statement_ :: statements, functionTable, functionRef)
        case ref: Reference =>
          val schemaType = TypeIdent(schemaTypeNamespace, ref.derefName)
          val modelType = modelNameConvention(ref.derefName, mode = mode)

          val (f, hist, fRef) = functionTable.get(schemaType -> modelType) match
            case None =>
              val fname = modelType.shortName + "From"
              val newFunctionTable =
                functionTable.updated(schemaType -> modelType, fname)
              val obj = resolve(schema, ref)
              val (stmts, newHistory, newFunctionRef) = generateModelMappingRec(
                schema,
                "ret",
                obj,
                (Nil, newFunctionTable, functionRef),
                mode = mode,
                schemaTypeNamespace = schemaTypeNamespace
              )
              val ret = Term.ValDef(i("ret"), Term.Init(modelType))
              val objectMappingFunc_ = Term.FnDecl(
                fname,
                ("data", Direct(schemaType)) :: Nil,
                List(Direct(modelType))
              )(
                ((ret :: stmts) :+ Term.Ret(i("ret")))*
              )
              (
                fname,
                newHistory,
                functionRef.updated(fname, objectMappingFunc_) ++ newFunctionRef
              )
            case Some(fname) =>
              (fname, functionTable, functionRef)
          val mappingStatements =
            if nullable then
              Term.If(Term.Eval(s"${select.render(0)} != nil"))(
                Term.ValDef(fieldName, Term.Eval(s"$f(*${select.render(0)})")),
                i(modelIdent) \\ fieldName := Term.Eval(s"&$fieldName")
              ) :: Nil
            else
              Term.ValDef(fieldName, Term.Select(f)(select)) ::
                (i(modelIdent) \\ fieldName := Term.Eval(
                  s"&$fieldName"
                )) ::
                Nil

          (mappingStatements ::: statements, hist, fRef)
        case ArrayDefinition(_, items, _) if items.isLeaf =>
          val t = leafTypeTranslation(items).get
          val elementsIdent = s"${snake2Camel(name)}Elements"
          val maybeDeref = if (nullable) "*" else ""
          val stmts_ = List(
            Term.ValDef(elementsIdent, Term.Eval(s"[]${t.fullName}{}")),
            // for ...
            Term.For(
              ValDef(
                List(Term.Ident.underscore, i("element")),
                Term.Select("range")(
                  Term.Eval(s"$maybeDeref${select.render(0)}")
                )
              )
            ) { args =>
              (
                Term.Select(elementsIdent) :=
                  Term.Select("append")(
                    i(elementsIdent),
                    Term.Select(s"${t.fullName}Value")(args(1))
                  )
              ) :: Nil
            },
            i(modelIdent) \\ fieldName := Term.Select(elementsIdent)
          )

          val guarded_ = guard(select, nullable, (stmts_)*).toList

          (guarded_ ::: statements, functionTable, functionRef)
        case ArrayDefinition(_, ref: Reference, _) =>
          val schemaType = TypeIdent(schemaTypeNamespace, ref.derefName)
          val modelType = modelNameConvention(ref.derefName, mode = mode)
          val (f, hist, fRef) = functionTable.get(schemaType -> modelType) match
            case Some(fname) =>
              (fname, functionTable, functionRef)
            case None =>
              val fname = modelType.shortName + "From"
              val newFunctionTable =
                functionTable.updated(schemaType -> modelType, fname)
              val obj = resolve(schema, ref)
              val (stmts, newHistory, newFunctionRef) = generateModelMappingRec(
                schema,
                "ret",
                obj,
                (Nil, newFunctionTable, functionRef),
                mode = mode,
                schemaTypeNamespace = schemaTypeNamespace
              )
              val ret = Term.ValDef(i("ret"), Term.Init(modelType))
              val objectMappingFunc_ = Term.FnDecl(
                fname,
                ("data", Direct(schemaType)) :: Nil,
                List(Direct(modelType))
              )(
                ((ret :: stmts) :+ Term.Ret(i("ret")))*
              )

              (
                fname,
                newHistory,
                functionRef.updated(fname, objectMappingFunc_) ++ newFunctionRef
              )

          val maybeDeref = if (nullable) "*" else ""
          val elementsIdent = i(s"${snake2Camel(name)}Elements")
          val stmts_ = List(
            Term.ValDef(
              elementsIdent,
              Term.Eval(s"[]${modelType.fullName}{}")
            ),
            // for ...
            Term.For(
              ValDef(
                List(Term.Ident.underscore, i("element")),
                Term.Select("range")(
                  Term.Eval(s"$maybeDeref${select.render(0)}")
                )
              )
            ) { args =>
              (
                elementsIdent.selected := Term
                  .Select("append")(elementsIdent, Term.Select(f)(args(1)))
              ) :: Nil
            },
            i(modelIdent) \\ fieldName := Term.Select(
              Nil,
              elementsIdent
            )
          )

          val guarded_ = guard(select, nullable, (stmts_)*).toList

          (guarded_ ::: statements, hist, fRef)
        case p @ (_: ObjectType | _: AnyStructuralType | _: ArrayDefinition) =>
          (statements, functionTable, functionRef)
  }

def guard(select: Expr, nullable: Boolean, stmts: Stmt*): Seq[Stmt] =
  if nullable then
    Term.If(Term.Eval(s"${select.render(0)} != nil"))(stmts*) :: Nil
  else stmts

def leafTypeTranslation(tpe: SchemaDefinition): Option[TypeIdent] =
  tpe match
    case i: IntEnum =>
      i.format match
        case Some("int32")     => Some(TypeIdent("types", "Int32"))
        case Some("int64")     => Some(TypeIdent("types", "Int64"))
        case Some("timestamp") => Some(TypeIdent("types", "Int64"))
        case _                 => Some(TypeIdent("types", "Int32"))
    case _: StringEnum => Some(TypeIdent(List("types"), "String"))
    case i: IntegerType =>
      i.format match
        case Some("int32")     => Some(TypeIdent("types", "Int32"))
        case Some("int64")     => Some(TypeIdent("types", "Int64"))
        case Some("timestamp") => Some(TypeIdent("types", "Int64"))
        case _                 => Some(TypeIdent("types", "Int32"))
    case n: NumberType =>
      n.format match
        case Some("float")  => Some(TypeIdent("types", "Float32"))
        case Some("double") => Some(TypeIdent("types", "Float64"))
        case _              => Some(TypeIdent("types", "Float32"))
    case _: BoolType => Some(TypeIdent("types", "Bool"))
    case StringType(tpe, format, description, example, nullable) =>
      Some(TypeIdent("types", "String"))
    case _: (ObjectType | AnyStructuralType | Reference | ArrayDefinition) =>
      None

@main
def integration(args: String*) =
  val platform = "piano"
  val group = args.headOption.getOrElse("publisher")
  val name = args.drop(1).headOption.getOrElse("Term").capitalize
  val mode = args.drop(2).headOption match
    case Some("datasource") => Mode.DataSource
    case Some("resource")   => Mode.Resource
    case Some(_) | None     => Mode.DataSource
  val modelName = modelNameConvention(name, mode = mode)
  val location = s"sdk/$platform/spec/json/$group.json"
  val data = Files.readString(Path.of(location))
  val schema = parser.parse(data).right.get
  val definitions = modelGen(schema, name, mode)
  val schemaDecl = schemaGen(schema, name, mode).render(0)


  // prelude
  println(miscGen(name, mode).map(_.render(0)).mkString("\n"))

  // datasource, resource
  // nested type adaptor
  println(
    attrTypeFunRec(
      schema,
      name,
      resolve(schema, Reference.fromName(name)),
      (Map.empty, Map.empty)
    )._2.map(_._2.render(0)).mkString("\n")
  )
  // types
  println(definitions.map(_.render(0)).mkString("\n"))
  // schema function
  println(schemaDecl)

  // CRUD operation
  val (decl, stmts, functions) = mappingsGen(
    schema,
    name,
    mode,
    schemaTypeNamespace = List(platform + "_" + group)
  )
  println(functions.map(_.render(0)).mkString("\n"))
  println(genRead(mode, name, decl, stmts).render(0))
  println(
    genMutationBoilerplates(mode, name, decl, stmts)
      .map(_.render(0))
      .mkString("\n")
  )

  // nested type adaptors

def modelGen(schema: Json, name: String, mode: Mode): List[Term.StructDecl] =
  val modelName = modelNameConvention(name, mode = mode)
  val modelDefinition = resolve(schema, Reference.fromName(name))
  val models = generateModelsRec(
    schema,
    modelName,
    modelDefinition,
    Map.empty,
    modelName :: Nil,
    mode = mode
  )
  models.map { case (name, fields) =>
    Term.StructDecl(name, fields)
  }.toList

import scala.collection.mutable.ListBuffer

def camelToSnake(str: String) = camelToSnakeRec(str)

private def camelToSnakeRec(
    str: String,
    parts: ListBuffer[String] = ListBuffer.empty
): String =
  val uncapitalized = str.take(1).toLowerCase() + str.drop(1)
  if str.startsWith("_") then camelToSnakeRec(str.drop(1), parts)
  else
    val (part, remains) =
      (uncapitalized.takeWhile(_.isLower), uncapitalized.dropWhile(_.isLower))
    if remains.isEmpty() then (parts :+ part).map(_.toLowerCase()).mkString("_")
    else camelToSnakeRec(remains, parts.appended(part))

def miscGen(name: String, mode: Mode): List[Term.FnDecl] =
  val implName = mode match
    case DataSource => name + "DataSource"
    case Resource   => name + "Resource"
  val implType = TypeIdent(Nil, implName)

  val providerTypeName = camelToSnake(name)

  val ret = mode.implementee
  val constr = Term.FnDecl(s"New$implName", Nil, List(Direct(ret)))(
    Term.Ret(Term.Init(implType))
  )
  val ctx = TypeIdent("context", "Context")
  val configure = Term.FnDecl(
    Some(("r", Ptr(implType))),
    "Configure",
    List(
      ("ctx", Direct(ctx)),
      ("req", Direct(mode.configureRequest)),
      ("resp", Ptr(mode.configureResponse))
    ),
    Nil,
    Term.Block(
      Term.If(Term.Eval("req.ProviderData == nil"))(Term.Ret())
    )
  )
  val met = Term.FnDecl(
    Some(("r", Ptr(implType))),
    "Metadata",
    List(
      ("ctx", Direct(ctx)),
      ("req", Direct(mode.metadataRequest)),
      ("resp", Ptr(mode.metadataResponse))
    ),
    Nil,
    Term.Block(
      i("resp") \\ "TypeName" :=
        Term.Eval(s"req.ProviderTypeName + \"_${providerTypeName}\"")
    )
  )
  List(constr, configure, met)

def schemaGen(schema: Json, name: String, mode: Mode): Term.FnDecl =
  val modelDefinition = resolve(schema, Reference.fromName(name))
  val implName = mode match
    case DataSource => s"${name}DataSource"
    case Resource   => s"${name}Resource"

  val implType = TypeIdent(Nil, implName)
  val schemaFunc = Term.FnDecl(
    Some(("_", Ptr(implType))),
    "Schema",
    List(
      ("ctx", Direct(TypeIdent("context", "Context"))),
      ("req", Direct(mode.schemaRequest)),
      ("resp", Ptr(mode.schemaResponse))
    ),
    Nil,
    Term.Block(
      i("resp") \\ "Schema" :=
        Term.Init(
          TypeIdent("schema", "Schema"),
          Term.Attrs(
            "Attributes" ->
              Term.Init(
                // FIXME: more type safety
                TypeIdent("map[string]schema", "Attribute"),
                Term.Attrs(
                  (
                    renderAttributes(
                      schema,
                      modelDefinition,
                      3,
                      name :: Nil,
                      mode
                    ).map((k, v) => (q(k), v))
                  )*
                )
              )
          )
        )
    )
  )
  schemaFunc

def q(s: String): String = s"\"$s\""
def mappingsGen(
    schema: Json,
    name: String,
    mode: Mode,
    modelIdent: String = "state",
    schemaTypeNamespace: List[String]
): (String, List[Stmt], Seq[Term.FnDecl]) =

  val modelDefinition = resolve(schema, Reference.fromName(name))
  val modelName = modelNameConvention(name, mode = mode)
  val decl = s"var $modelIdent ${modelName.fullName}"
  val (stmts, _, functions) = generateModelMappingRec(
    schema,
    modelIdent,
    modelDefinition,
    (Nil, Map.empty, Map.empty),
    mode,
    schemaTypeNamespace
  )
  (decl, stmts, functions.values.toSeq)
