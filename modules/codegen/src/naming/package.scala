package codegen.naming

import pseudogo.TypeIdent
import codegen.Mode
import codegen.text.snake2UpperCamel
import io.circe.Json
import jsonschema.SchemaDefinition
import jsonschema.SchemaDefinition.*

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

def attrTypeFuncNameConvention(typeName: String): String =
  s"${typeName}AttrType"