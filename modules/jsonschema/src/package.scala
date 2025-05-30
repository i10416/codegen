package jsonschema
import io.circe.Codec
import cats.syntax.all.*
import SchemaDefinition.*
import io.circe.syntax.*
import io.circe.*
import io.circe.Decoder.Result
import io.circe.DecodingFailure
import io.circe.DecodingFailure.Reason
import io.circe.optics.JsonPath.root

def resolve(schema: Json, ref: Reference): ObjectType =
  root.components.schemas
    .selectDynamic(ref.derefName)
    .json
    .getOption(schema)
    .get
    .as[ObjectType] match
    case Left(e)    => throw new Exception(e)
    case Right(obj) => obj
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
  def asIntEnum: Option[IntEnum] = this match
    case self: IntEnum => Some(self)
    case _             => None
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
      schema: Json,
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
