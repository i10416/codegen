package codegen

import codegen.cli.CodegenCommands
import codegen.naming.*
import codegen.text.*
import io.circe.*
import jsonschema.SchemaDefinition
import jsonschema.SchemaDefinition.*
import jsonschema.resolve
import pseudogo.*

import java.nio.file.Files
import java.nio.file.Path

import Mode.DataSource
import Mode.Resource
import Term.ValDef

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
    List(TypeIdent("attr", "Type")),
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
            value.asIntEnum
              .map: enums =>
                val size = enums.format match
                  case Some("int64" | "timestamp") => "int64"
                  case _                           => "int32"
                val args = enums.`enum`.map(i => Term.Eval(i.toString()))
                (i(s"${size}validator") \\ "OneOf")(args*) -> size
              .map: (validator, size) =>
                (
                  "Validators",
                  Term.Eval(
                    s"[]validator.${size.capitalize}{${validator.render(0)}}"
                  )
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
            Some((Some("r"), Ptr(implType))),
            action,
            List(
              ("ctx", TypeIdent("context", "Context")),
              ("req", TypeIdent(mode.namespace, action + "Request")),
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
        Some((Some("r"), Ptr(implType))),
        "Delete",
        List(
          ("ctx", TypeIdent("context", "Context")),
          ("req", TypeIdent(mode.namespace, "Delete" + "Request")),
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
        Some((Some("r"), Ptr(implType))),
        "ImportState",
        List(
          ("ctx", TypeIdent("context", "Context")),
          ("req", TypeIdent(mode.namespace, "ImportState" + "Request")),
          (
            "resp",
            TypeIdent(mode.namespace, "ImportState" + "Response")
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
    Some((Some("r"), Ptr(implType))),
    "Read",
    List(
      ("ctx", TypeIdent("context", "Context")),
      ("req", mode.readRequest),
      ("resp", Ptr(mode.readResponse))
    ),
    Nil,
    Term.Block(
      (Term.Eval(stateDecl) ::
        readDataToModel ::
        returnOnError ::
        mappings.appended(writeModelToData))*
    )
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
        throw new Exception(
          s"Unexpected SchemaDefinition: ${unnamedObject} at ${path.map(_.fullName).mkString(" / ")}"
        )
      case obj: AnyStructuralType =>
        // FIXME
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
            Slice(leafTypeTranslation(items).get),
            fieldOptions,
            docs
          )
        history.updatedWith(model)(_.map(_ :+ field).orElse(Some(field :: Nil)))
      case ArrayDefinition(tpe, unnamedObject: ObjectType, nullable) =>
        val syntheticName =
          modelNameConvention(fieldName, Some(model), mode = mode)
        val field = Term.Field(
          fieldName,
          Slice(syntheticName),
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
          Term.Field(fieldName, Slice(modelName), fieldOptions, docs)
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
          leafTypeTranslation(leaf).get,
          fieldOptions,
          docs
        )

        history.updatedWith(model)(_.map(_ :+ field).orElse(Some(field :: Nil)))
      case schemaDefn =>
        throw new Exception(
          s"Unexpected SchemaDefinition: ${schemaDefn} at ${path.map(_.fullName).mkString(" / ")}"
        )
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
                ("data", schemaType) :: Nil,
                List(modelType)
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
                ("data", schemaType) :: Nil,
                List(modelType)
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
  CodegenCommands.gen.parse(args) match
    case Left(value) =>
      println(value)
      sys.exit(1)
    case Right((location, name, namespace, mode)) =>
      val modelName = modelNameConvention(name, mode = mode)
      val data = Files.readString(location)
      val schema = parser.parse(data).right.get
      val definitions = modelGen(schema, name, mode)
      val schemaDecl = schemaGen(schema, name, mode)
      println(miscGen(name, mode).map(_.render(0)).mkString("\n"))
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
      println(schemaDecl.render(0))

      // CRUD operation
      val (decl, stmts, functions) = mappingsGen(
        schema,
        name,
        mode,
        schemaTypeNamespace = List(namespace)
      )
      println(functions.map(_.render(0)).mkString("\n"))
      println(genRead(mode, name, decl, stmts).render(0))
      println(
        genMutationBoilerplates(mode, name, decl, stmts)
          .map(_.render(0))
          .mkString("\n")
      )

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

def miscGen(name: String, mode: Mode): List[Term.FnDecl] =
  val implName = mode match
    case DataSource => name + "DataSource"
    case Resource   => name + "Resource"
  val implType = TypeIdent(Nil, implName)

  val providerTypeName = camelToSnake(name)

  val ret = mode.implementee
  val constr = Term.FnDecl(s"New$implName", Nil, List(ret))(
    Term.Ret(Term.Init(implType).asRef)
  )
  val ctx = TypeIdent("context", "Context")
  val configure = Term.FnDecl(
    Some((Some("r"), Ptr(implType))),
    "Configure",
    List(
      ("ctx", ctx),
      ("req", mode.configureRequest),
      ("resp", Ptr(mode.configureResponse))
    ),
    Nil,
    Term.Block(
      Term.If(Term.Eval("req.ProviderData == nil"))(Term.Ret())
    )
  )
  val met = Term.FnDecl(
    Some((Some("r"), Ptr(implType))),
    "Metadata",
    List(
      ("ctx", ctx),
      ("req", mode.metadataRequest),
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
    Some((None, Ptr(implType))),
    "Schema",
    List(
      ("ctx", TypeIdent("context", "Context")),
      ("req", mode.schemaRequest),
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
