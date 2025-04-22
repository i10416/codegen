//> using dep io.circe::circe-core::0.14.12
//> using dep io.circe::circe-parser::0.14.12
//> using dep io.circe::circe-optics::0.15.0
//> using dep org.typelevel::cats-core::2.13.0

import cats.data.*
import io.circe.*
import io.circe.syntax.*
import io.circe.Printer
import io.circe.parser
import cats.syntax.all.*
import io.circe.derivation.ConfiguredCodec
import io.circe.optics.*
import io.circe.optics.JsonPath.*
import cats.syntax.alternative.*
import java.nio.file.Path

import java.nio.file.Files
import io.circe.derivation.Configuration
import scala.sys.Prop
import io.circe.derivation.ConfiguredEnumCodec
import io.circe.Decoder.Result
import io.circe.DecodingFailure.Reason
import J.SchemaDefinition.IntEnum
import J.SchemaDefinition.StringEnum
import J.SchemaDefinition.PrimitiveType
import J.SchemaDefinition.IntegerType
import J.SchemaDefinition.Reference
import J.SchemaDefinition.ArrayDefinition
import J.SchemaDefinition.ObjectType
import J.SchemaDefinition
import J.SchemaDefinition.AnyStructuralType
import Mode.DataSource
import Mode.Resource
import J.SchemaDefinition.StringType
import J.SchemaDefinition.NumberType
import J.SchemaDefinition.BoolType


@main def schemaGenerate(args: String*) = {
  val group = args.headOption.getOrElse("publisher")
  val name =args.drop(1).headOption.getOrElse("Term").capitalize
  val mode = Mode.DataSource
  val data = Files.readString(Path.of(s"sdk/piano/spec/json/${group}.json"))
  val schema = parser.parse(data).right.get
  println(schemaGen(schema, name, mode))
}
def resolve(schema: Json, ref: Reference): ObjectType =
  root.components.schemas.selectDynamic(ref.derefName).json.getOption(schema).get.as[ObjectType] match
    case Left(e) => throw new Exception(e)
    case Right(obj) =>  obj

def schemaNamingConvention(schema: Json,value: SchemaDefinition): String =
  value match
      case _ : IntEnum    => "Int32Attribute"
      case _ : StringEnum => "StringAttribute"
      case _ : StringType => "StringAttribute"
      case _ : BoolType => "BoolAttribute"
      case i : IntegerType =>
        if i.format.exists(_ == "int32") then "Int32Attribute"
        else if i.format.exists(i => Set("int64","timestamp").contains(i)) then "Int64Attribute"
        else "Int32Attribute"
      case n : NumberType =>
         if n.format.exists(_ == "float") then "Float32Attribute"
         else if n.format.exists(_ == "double") then "Float64Attribute"
         else "Float32Attribute"
      case _ : Reference     =>
        "SingleNestedAttribute"
        //schemaNamingConvention(schema, resolve(schema, ref))
      case l: ArrayDefinition => if l.nested then "ListNestedAttribute" else "ListAttribute"
      case obj : ObjectType if obj.nested => "SingleNestedAttribute"
      case _: ObjectType  | _ : AnyStructuralType    => "ObjectAttribute"

def indent(level: Int): String = " " * (2 * level)

sealed trait Mode {
  def modelNameSuffix: String = this match
    case Mode.DataSource => "DataSourceModel"
    case Mode.Resource => "ResourceModel"

  def namespace: String = this match
    case DataSource => "datasource"
    case Resource => "resource"
}
object Mode {
  case object DataSource extends Mode
  case object Resource extends  Mode
}

private def renderAttributes(schema: Json, schemaDefn: ObjectType,level: Int, path: List[String],mode: Mode): String =
  val x = schemaDefn.properties.map: (key, value) =>
    val tfsdkName = camelToSnake(key)
    val openDefn = Some(indent(level + 0) +  s""""$tfsdkName": schema.${{schemaNamingConvention(schema, value)}} {""")
    val requiredAttr =
      Option.when(mode != Mode.DataSource && schemaDefn.strictPresence(key))(indent(level + 1) + "Required: true,")
    val optionalAttr =
      Option.when(mode != Mode.DataSource && !schemaDefn.strictPresence(key))(indent(level + 1) + "Optional: true,")
    val computedAttr =
      Option.when(mode == Mode.DataSource)(indent(level + 1) + "Computed: true,")

    val description =
      value.description.map(_.linesIterator.mkString(" ").trim()).map(d =>
        indent(level + 1) +s"""MarkdownDescription: "${d.replace("\"", "\\\"")}","""
      )

    val stringEnumValidation = value.asStringEnum
      .map: enums =>
        enums.`enum`
          .map(variant => s""""$variant"""")
          .mkString("stringvalidator.OneOf(", ",", "),")
      .map: validator =>
        List(
          indent(level + 1) +"Validators: []validator.String{",
          indent(level + 2) + validator,
          indent(level + 1) +"}"
        ).mkString("", "\n", ",")
    val objectRef = value.asRef.map: ref =>
        List(
          //indent(level + 1) +s"NestedObject: schema.NestedAttributeObject{",
          indent(level + 1)+  "Attributes: map[string]schema.Attribute{",
          {
            if path.contains(ref.derefName) then
              renderAttributes(schema, ObjectType("object",None,None,None,Map.empty,None),level+2,path,mode)
            else
              renderAttributes(schema, resolve(schema,ref), level+2,path,mode)
          },
          //indent(level + 1) + "},",
          indent(level + 1) +"},"
        ).mkString("\n")
    val listInner = value.asList.map: arrayDefn =>
      if arrayDefn.nested then
        List(
          indent(level + 1) +s"NestedObject: schema.NestedAttributeObject{",
         indent(level + 2)+  "Attributes: map[string]schema.Attribute{",
          arrayDefn.items match
            case obj: ObjectType =>
              renderAttributes(schema, obj,level+3,path,mode)
            case ref: Reference =>
              renderAttributes(schema, resolve(schema, ref),level+3,ref.derefName::path,mode),
          indent(level + 2) + "},",
          indent(level + 1) +"},"
        ).mkString("\n")
      else
        val t = arrayDefn.items.tpe.get.capitalize
        indent(level+1)+s"ElementType: basetypes.${t}Type{},"

    val closeDefn = Some( indent(level + 0) + "}")
    List(
      openDefn,
      requiredAttr,
      optionalAttr,
      computedAttr,
      description,
      stringEnumValidation,
      listInner,
      objectRef,
      closeDefn
    ).flatten.mkString("", "\n", ",\n")
  x.mkString

object J {


  sealed trait SchemaDefinition {
    def explicitNullable: Boolean = nullable.getOrElse(false)
    def nullable: Option[Boolean]
    def tpe: Option[String]
    def asRef: Option[Reference] = this match
      case self : Reference => Some(self)
      case _ => None
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
      case _ => false
    def isObject: Boolean = this match
      case _: ObjectType => true
      case _             => false
    def isObjectLike: Boolean = this match
      case _: ObjectType => true
      case _: Reference  => true
      case _ : AnyStructuralType => true
      case _ : (IntEnum |StringEnum | PrimitiveType | ArrayDefinition) => false
    def isList: Boolean = this match
      case _: ArrayDefinition => true
      case _                  => false

    def nested: Boolean = this match
      case _ : IntEnum | _ : StringEnum | _ : PrimitiveType =>  false
      case obj: ObjectType => obj.properties.values.exists(schema => schema.isObjectLike)
      case  _ : AnyStructuralType => false
      case _ : Reference => true
      case list :ArrayDefinition => list.items.nested

    def isLeaf: Boolean = this match
      case _: IntEnum | _: StringEnum | _: PrimitiveType => true
      case _                                             => false
  }
  object SchemaDefinition {
    sealed trait EnumDefinition extends SchemaDefinition
    implicit val codec: Decoder[SchemaDefinition] = new Decoder[SchemaDefinition] {
      def apply(c: HCursor): Result[SchemaDefinition] =
        (c.get[String]("$ref"), c.get[String]("type")) match
          case (Right(ref), Left(_)) => Decoder[Reference].apply(c)
          case (Left(_), Right(tpe)) =>
            if tpe == "array" then Decoder[ArrayDefinition].apply(c)
            else if tpe == "object" then
              if c.downField("properties").succeeded then
                Decoder[ObjectType].apply(c)
              else Right(AnyStructuralType(c.value, c.get[String]("description").toOption,c.get[Boolean]("nullable").toOption))
            else if tpe == "string" then
              (Decoder[StringEnum].widen <+> Decoder[StringType].widen).apply(c)
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
    ) extends EnumDefinition
        derives Codec {
      def tpe: Option[String] = Some(`type`)
    }

    case class StringEnum(
        `type`: String,
        description: Option[String],
        format: Option[String],
        `enum`: List[String],
        example: Option[String],
        nullable: Option[Boolean]
    ) extends EnumDefinition
        derives Codec {
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
    object IntegerType  {
              implicit val codec: Codec[IntegerType] = Codec.forProduct5[IntegerType,String,Option[String],Option[String],Option[Int],Option[Boolean]]("type","format","description","example","nullable")(IntegerType.apply(_,_,_,_,_))(p =>
        (p.`type`, p.format, p.description, p.example, p.nullable))
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
    object NumberType  {
              implicit val codec: Codec[NumberType] = Codec.forProduct5[NumberType,String,Option[String],Option[String],Option[Double],Option[Boolean]]("type","format","description","example","nullable")(NumberType.apply(_,_,_,_,_))(p =>
        (p.`type`, p.format, p.description, p.example, p.nullable))
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
    object StringType  {
              implicit val codec: Codec[StringType] = Codec.forProduct5[StringType,String,Option[String],Option[String],Option[String],Option[Boolean]]("type","format","description","example","nullable")(StringType.apply(_,_,_,_,_))(p =>
        (p.`type`, p.format, p.description, p.example, p.nullable))
    }
    case class BoolType(
        `type`: String,
        description: Option[String],
        nullable: Option[Boolean]
    ) extends PrimitiveType {
        def tpe: Option[String] = Some(`type`)
    }
    object BoolType  {
              implicit val codec: Codec[BoolType] = Codec.forProduct3[BoolType,String,Option[String],Option[Boolean]]("type","description","nullable")(BoolType.apply(_,_,_))(p =>
        (p.`type`, p.description, p.nullable))
    }

    sealed trait PrimitiveType extends SchemaDefinition
    object PrimitiveType {
      implicit val de: Decoder[PrimitiveType] = new Decoder[PrimitiveType] {
        def apply(c: HCursor): Result[PrimitiveType] = c.get[String]("type") match {
            case Left(value) =>  Left(value)
            case Right("integer") => Codec[IntegerType].decodeJson(c.value)
            case Right("number") => Codec[NumberType].decodeJson(c.value)
            case Right("string") => Codec[StringType].decodeJson(c.value)
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
    ) extends SchemaDefinition
        derives Decoder {
      def strictPresence(key: String): Boolean =
        properties.keySet.contains(key) &&
          required.getOrElse(Nil).contains(key) &&
            properties(key).nullable.forall(!_)

      def tpe: Option[String] = Some(`type`)
    }
    case class AnyStructuralType(value: Json, description: Option[String],nullable:Option[Boolean]) extends SchemaDefinition {
      def tpe: Option[String] = Some("object")
    }


    case class Reference(ref: String, nullable: Option[Boolean]) extends SchemaDefinition {
      def derefName: String = ref.stripPrefix("#/components/schemas/")
      def description: Option[String] = None
      def tpe: Option[String] = None
    }
    object Reference {
      def fromName(name: String, nullable: Option[Boolean] = None): Reference = Reference(s"#/components/schemas/$name", nullable)
      implicit val codec: Codec[Reference] =
        Codec.forProduct2[Reference,String,Option[Boolean]]("$ref","nullable")(Reference(_,_))(p =>
          (p.ref, p.nullable)
        )
    }

    case class ArrayDefinition(
        `type`: String,
        items: SchemaDefinition,
        nullable: Option[Boolean]
    ) extends SchemaDefinition
        derives Decoder {
      def description: Option[String] = None

      def tpe: Option[String] = Some(`type`)

    }
  }
}

@main
def generateModels(args: String*) =
  val group = args.headOption.getOrElse("publisher")
  val name = args.drop(1).headOption.getOrElse("Term")
  
  val mode = Mode.DataSource
  
  val data = Files.readString(Path.of(s"sdk/piano/spec/json/${group}.json"))
  val schema = parser.parse(data).right.get
  
  val definitions = modelGen(schema, name, mode)

  println(definitions)
def snake2Camel(str: String) =
  val elements = str.split("_")
  val (head,tail) = (elements.head, elements.tail)
  if tail.isEmpty then head
  else head + tail.map(_.capitalize).mkString

def snake2UpperCamel(str: String) = snake2Camel(str).capitalize

final case class TypeIdent(namespace: List[String], shortName: String){
  def fullName = (namespace :+ shortName).mkString(".")
}
def modelNameConvention(schemaName: String,owner: Option[TypeIdent]= None,mode: Mode): TypeIdent =
  TypeIdent(Nil, owner.fold("")(_.shortName) + snake2UpperCamel(schemaName) +  mode.modelNameSuffix)
def generateModelsRec(schema: Json,model: TypeIdent, modelDefn: ObjectType,history: Map[TypeIdent,List[String]],path: List[TypeIdent],mode: Mode): Map[TypeIdent,List[String]] =
  modelDefn.properties.foldLeft(history){case (history, (name, property)) =>
        val fieldName = snake2UpperCamel(name)
        val tfsdkName = camelToSnake(name)
        val fieldOptions = s"""`tfsdk:"$tfsdkName"`"""
        val docs = property.description.map(_.linesIterator.mkString(" ").trim()).fold("")(" // " + _)

        property match
          case unnamedObject: ObjectType =>
            ???
          case obj: AnyStructuralType =>
            history
          case ref: Reference =>
            val modelName = modelNameConvention(ref.derefName, mode = mode)
            val ptrMark = "*"
            val field = s"$fieldName ${ptrMark}${modelName.fullName} $fieldOptions$docs"
            if history.contains(modelName) then
              history.updatedWith(model)(_.map(_ :+ field).orElse(Some(field::Nil)))
            else
              generateModelsRec(schema,modelNameConvention(ref.derefName,mode = mode),resolve(schema,ref),history + (modelNameConvention(ref.derefName,mode = mode) -> Nil),path:+modelName,mode = mode).updatedWith(model)(_.map(_ :+ field).orElse(Some(field::Nil)))
          case ArrayDefinition(tpe, items, nullable) if items.isLeaf =>
            val field = s"$fieldName []${{leafTypeTranslation(items).get}.fullName} $fieldOptions$docs"
            history.updatedWith(model)(_.map(_ :+ field).orElse(Some(field::Nil)))
          case ArrayDefinition(tpe, unnamedObject: ObjectType, nullable) =>
            val syntheticName = modelNameConvention(fieldName,Some(model),mode = mode)
            val field = s"$fieldName []${syntheticName.fullName} $fieldOptions"
            generateModelsRec(schema, syntheticName,unnamedObject,history + (syntheticName -> Nil),path:+syntheticName,mode).updatedWith(model)(_.map(_ :+ field).orElse(Some(field::Nil)))
          case ArrayDefinition(tpe, ref : Reference, nullable) =>
            val modelName = modelNameConvention(ref.derefName,mode = mode)
            val field = s"$fieldName []${modelName.shortName} $fieldOptions$docs"
            generateModelsRec(schema, modelName,resolve(schema,ref) ,history + (modelName -> Nil),path:+modelName,mode).updatedWith(model)(_.map(_ :+ field).orElse(Some(field::Nil)))
          case leaf : (IntEnum | StringEnum | PrimitiveType) =>
            val field = s"$fieldName ${leafTypeTranslation(leaf).get.fullName} $fieldOptions$docs"
            history.updatedWith(model)(_.map(_ :+ field).orElse(Some(field::Nil)))
          case schemaDefn => throw new Exception(s"Unexpected SchemaDefinition: ${schemaDefn}")
  }
case class FunctionDecl(fname: String, arg: TypeIdent,ret: TypeIdent, stmts: List[String]) {
  def materialize: String =
              List(
                List(
                  s"func $fname(data ${arg.fullName}) ${ret.fullName} {",
                ),
                (s"ret := ${ret.fullName}{}" :: (stmts :+  "return ret")).map(s => indent(1) + s),
                List(
                  s"}"
                )
              ).flatten.mkString("\n")
}

@main
def generateModelMappings(args: String*) =
  val group = args.headOption.getOrElse("publisher")
  val name = args.drop(1).headOption.getOrElse("Term")
  val mode = Mode.DataSource
  val data = Files.readString(Path.of(s"sdk/piano/spec/json/${group}.json"))
  val schema = parser.parse(data).right.get
  val (stmts, functions) = mappingsGen(schema, name, mode,schemaTypeNamespace = Nil)
  println(functions.mkString("\n"))
  println(stmts.mkString("\n"))

def propertyCanBeAbsent(owner: ObjectType, propName: String,property:SchemaDefinition): Boolean =
        property.explicitNullable || !owner.required.exists(_.contains(propName))
def generateModelMappingRec(schema: Json, modelIdent: String, modelDefn: ObjectType,history: (List[String],Map[(TypeIdent,TypeIdent),String],Map[String,FunctionDecl]),mode: Mode, schemaTypeNamespace: List[String]): (List[String],Map[(TypeIdent,TypeIdent),String],Map[String,FunctionDecl]) =

    modelDefn.properties.foldLeft(history) { case ((statements, functionTable,functionRef), (name, property)) =>
      val fieldName = snake2UpperCamel(name)
      val nullable = propertyCanBeAbsent(modelDefn, name, property)
      val select =
        property match
            case i : IntEnum =>
                val cast = i.format.fold("int32"){
                    case "int64" | "timestamp" if nullable => "(*int64)"
                    case "int64" | "timestamp" => "int64"
                    case _ if nullable => "(*int32)"
                    case _ => "int32"
                }
                List("data", fieldName).mkString(s"$cast(",".",")")
            case i: IntegerType =>
                i.format match
                    case None => List("data", fieldName).mkString(s"int32(",".",")")
                    case Some("timestamp") => List("data", fieldName).mkString(s"int64(",".",")")
                    case _ => List("data", fieldName).mkString(".")
            case (s: StringEnum) =>
                val cast =
                    if nullable then "(*string)"
                    else "string"
                List("data", fieldName).mkString(s"$cast(",".",")")
            case _ => List("data", fieldName).mkString(".")
      property match
        case p @ (_ : PrimitiveType | _: IntEnum | _ : StringEnum) =>
          val typeMapping = leafTypeTranslation(p).get
          val maybePtr =
            if nullable then "Pointer"
            else ""
          val statement = s"$modelIdent.$fieldName = ${typeMapping.fullName}${maybePtr}Value($select)"
          (statement::statements, functionTable,functionRef)
        case ref: Reference =>
          val schemaType = TypeIdent(schemaTypeNamespace, ref.derefName)
          val modelType = modelNameConvention(ref.derefName, mode = mode)

          val (f, hist, fRef) = functionTable.get(schemaType -> modelType) match
            case None =>
              val fname = modelType.shortName + "From"
              val newFunctionTable = functionTable.updated(schemaType -> modelType, fname)
              val obj = resolve(schema, ref)
              val (stmts, newHistory, newFunctionRef) = generateModelMappingRec(schema,"ret",obj,(Nil,newFunctionTable,functionRef),mode = mode, schemaTypeNamespace = schemaTypeNamespace)
              val objectMappingFunc = FunctionDecl(fname, schemaType, modelType,stmts)
              (fname, newHistory, functionRef.updated(fname, objectMappingFunc) ++ newFunctionRef)
            case Some(fname) =>
              (fname, functionTable, functionRef)
          val assignStatement = s"$modelIdent.$fieldName = &$fieldName"
          val mappingStatements =
            if nullable then
                List(
                    s"if $select != nil {",
                    indent(1) + s"$fieldName := $f(*${select})",
                    indent(1) + assignStatement,
                    "}"
                )
            else
                s"$fieldName := $f(${select})" :: assignStatement :: Nil
          (mappingStatements ::: statements, hist, fRef)
        case ArrayDefinition(_, items, _) if items.isLeaf =>
          val t = leafTypeTranslation(items).get
          val elementsIdent = s"${snake2Camel(name)}Elements"
          val maybeDeref = if (nullable) "*" else ""
          val stmts =  List(
                s"$elementsIdent := []${t.fullName}{}",
                s"for _, element := range($maybeDeref${select}) {",
                indent(1) + s"$elementsIdent = append($elementsIdent, ${t.fullName}Value(element))",
                "}",
                s"$modelIdent.$fieldName = $elementsIdent"
          )
          val guarded =
            if nullable then List(
                List(s"if $select != nil {"),
                stmts.map(stmt => indent(1) + stmt),
                List("}")
            ).flatten
            else stmts
          (guarded ::: statements, functionTable, functionRef)
        case ArrayDefinition(_, ref: Reference, _) =>
          val schemaType = TypeIdent(schemaTypeNamespace, ref.derefName)
          val modelType = modelNameConvention(ref.derefName, mode = mode)
          val (f, hist, fRef) = functionTable.get(schemaType -> modelType) match
            case Some(fname) =>
              (fname, functionTable, functionRef)
            case None =>
              val fname = modelType.shortName + "From"
              val newFunctionTable = functionTable.updated(schemaType -> modelType, fname)
              val obj = resolve(schema, ref)
              val (stmts,newHistory,newFunctionRef) = generateModelMappingRec(schema,"ret",obj,(Nil,newFunctionTable,functionRef),mode = mode,schemaTypeNamespace=schemaTypeNamespace)
              val objectMappingFunc = FunctionDecl(fname, schemaType, modelType, stmts)
              (fname, newHistory, functionRef.updated(fname, objectMappingFunc) ++ newFunctionRef)
          val maybeDeref = if (nullable) "*" else ""
          val elementsIdent = s"${snake2Camel(name)}Elements"
          val stmts = List(
            s"$elementsIdent := []${modelType.fullName}{}",
            s"for _, element := range($maybeDeref${select}) {",
            indent(1) + s"$elementsIdent = append($elementsIdent, $f(element))",
            "}",
            s"$modelIdent.$fieldName = $elementsIdent"
          )
          val guarded =
            if nullable then List(
                List(s"if $select != nil {"),
                stmts.map(stmt => indent(1) + stmt),
                List("}")
            ).flatten
            else stmts
          (guarded ::: statements, hist, fRef)
        case p @ (_ : ObjectType| _ : AnyStructuralType | _ : ArrayDefinition) =>
          (statements, functionTable, functionRef)
    }

def leafTypeTranslation(tpe: SchemaDefinition): Option[TypeIdent] =
  tpe match
    case i: IntEnum =>
      i.format match
        case Some("int32") => Some(TypeIdent(List("types"),"Int32"))
        case Some("int64") => Some(TypeIdent(List("types"),"Int64"))
        case Some("timestamp") => Some(TypeIdent(List("types"),"Int64"))
        case _ => Some(TypeIdent(List("types"),"Int32"))
    case _ : StringEnum => Some(TypeIdent(List("types"), "String"))
    case i : IntegerType =>
      i.format match
        case Some("int32") => Some(TypeIdent(List("types"),"Int32"))
        case Some("int64") => Some(TypeIdent(List("types"),"Int64"))
        case Some("timestamp") => Some(TypeIdent(List("types"),"Int64"))
        case _ => Some(TypeIdent(List("types"),"Int32"))
    case n: NumberType =>
      n.format match
        case Some("float") => Some(TypeIdent(List("types"),"Float32"))
        case Some("double") => Some(TypeIdent(List("types"),"Float64"))
        case _ => Some(TypeIdent(List("types"),"Float32"))
    case _: BoolType => Some(TypeIdent(List("types"),"Bool"))
    case StringType(tpe, format, description, example, nullable) =>
        Some(TypeIdent(List("types"),"String"))
    case _ : (ObjectType | AnyStructuralType | Reference | ArrayDefinition) => None

@main
def integration(args: String*) =
  val platform = "piano"
  val group = args.headOption.getOrElse("publisher")
  val name =args.drop(1).headOption.getOrElse("Term").capitalize
  val mode = Mode.DataSource
  val modelName = modelNameConvention(name,mode = mode)
  val location = s"sdk/$platform/spec/json/$group.json"
  val data = Files.readString(Path.of(location))
  val schema = parser.parse(data).right.get
  // println(resolve(schema, Reference.fromName(name)))
  val definitions = modelGen(schema, name, mode)
  val schemaDecl = schemaGen(schema, name, mode)
  val (stmts, functions) = mappingsGen(schema, name, mode,schemaTypeNamespace = List(platform + "_" + group))
  println(definitions)
  println(metadataGen(name, mode))
  println(schemaDecl)
  println(functions.mkString("\n"))
  println(stmts)

def modelGen(schema: Json, name: String, mode: Mode) =
  val modelName = modelNameConvention(name,mode = mode)
  val modelDefinition = resolve(schema, Reference.fromName(name))
  val models = generateModelsRec(schema, modelName,modelDefinition, Map.empty,modelName::Nil,mode = mode)
  models.flatMap{ case (name, fields) =>
    List(
    List(s"type ${name.shortName} struct {"),
    fields.sorted.map(field => indent(1) + field),
    List("}")
    )
  }.flatten.mkString("\n")
import scala.collection.mutable.ListBuffer

def camelToSnake(str: String) = camelToSnakeRec(str)

private def camelToSnakeRec(str: String,parts: ListBuffer[String]= ListBuffer.empty): String =
  val uncapitalized = str.take(1).toLowerCase() + str.drop(1)
  if str.startsWith("_") then
    camelToSnakeRec(str.drop(1), parts)
  else
    val (part,remains) = (uncapitalized.takeWhile(_.isLower),uncapitalized.dropWhile(_.isLower))
    if remains.isEmpty() then (parts :+ part).map(_.toLowerCase()).mkString("_")
    else camelToSnakeRec(remains, parts.appended(part))

def metadataGen(name: String, mode: Mode): String =
  val implName = mode match
    case DataSource => name + "DataSource"
    case Resource => name + "Resource"

  val providerTypeName = camelToSnake(name)
  List(
    s"func (r *$implName) Metadata(ctx context.Context, req ${mode.namespace}.MetadataRequest, resp *${mode.namespace}.MetadataResponse) {",
    s"  resp.TypeName = req.ProviderTypeName + \"_${providerTypeName}\"",
    "}"
  ).mkString("\n")
def schemaGen(schema: Json, name: String, mode: Mode) =
  val modelDefinition = resolve(schema, Reference.fromName(name))
  val impl = mode match
    case DataSource => s"${name}DataSource"
    case Resource => s"${name}Resource"
  val namespace = mode match
    case DataSource => "datasource"
    case Resource => "resource"

  List(
    s"func (_ *${impl}) Schema(ctx context.Context, req $namespace.SchemaRequest, resp *$namespace.SchemaResponse) {",
      "  resp.Schema = schema.Schema{",
      s"    MarkdownDescription: \"${name} $namespace\",",
      "      Attributes: map[string]schema.Attribute{",
      renderAttributes(schema, modelDefinition, 3, name :: Nil,mode),
      "    },",
      "  }",
      "}"
    ).mkString("\n")

def mappingsGen(schema: Json, name: String, mode: Mode, modelIdent: String = "state",schemaTypeNamespace: List[String]) =

  val modelDefinition = resolve(schema, Reference.fromName(name))
  val modelName = modelNameConvention(name,mode = mode)
  val decl = s"var $modelIdent ${modelName.fullName}"
  val (stmts, _,functions) = generateModelMappingRec(schema, modelIdent, modelDefinition, (Nil,Map.empty,Map.empty),mode,schemaTypeNamespace)
  ((decl :: stmts).mkString("\n"), functions.values.map(_.materialize))
