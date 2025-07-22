package codegen
import io.circe.parser
class CodegenSuite extends munit.FunSuite {
  test("sanity check by the simplest input") {
    // The mapping uses types.Int32Value for code as code is non-nullable and non-undefine-able value
    // The mapping uses types.StringPointerValue for message as message is nullable and undefine-able value.
    val Right(schema) = parser.parse("""
    |{
    |  "components": {
    |    "schemas": {
    |      "Sample": {
    |        "type": "object",
    |        "required": ["code"],
    |        "properties": {
    |          "code": {
    |            "type": "integer",
    |            "nullable": false
    |          },
    |          "message": {
    |            "type": "string",
    |            "nullable": true
    |          },
    |          "details": {
    |            "type": "array",
    |            "items": {
    |              "type": "string"
    |            }
    |          }
    |        }
    |      }
    |    }
    |  }
    |}
    |""".stripMargin.trim): @unchecked
    val sample = run(schema, "Sample", "sample", Mode.DataSource)
    assertEquals(
      sample.render(),
      """
      |type SampleDataSource struct {
      |}
      |func NewSampleDataSource() datasource.DataSource{
      |  return &SampleDataSource{}
      |}
      |func (r *SampleDataSource) Configure(ctx context.Context, req datasource.ConfigureRequest, resp *datasource.ConfigureResponse) {
      |  if req.ProviderData == nil {
      |    return
      |  }
      |}
      |func (r *SampleDataSource) Metadata(ctx context.Context, req datasource.MetadataRequest, resp *datasource.MetadataResponse) {
      |  resp.TypeName = req.ProviderTypeName + "_sample"
      |}
      |func SampleAttrType() attr.Type{
      |  return basetypes.ObjectType{
      |    AttrTypes: map[string]attr.Type{
      |      "code": types.Int32Type,
      |      "message": types.StringType,
      |      "details": types.ListType{
      |        ElemType: types.StringType,
      |      },
      |    },
      |  }
      |}
      |type SampleDataSourceModel struct {
      |  Code types.Int32 `tfsdk:"code"`
      |  Message types.String `tfsdk:"message"`
      |  Details []types.String `tfsdk:"details"`
      |}
      |func (*SampleDataSource) Schema(ctx context.Context, req datasource.SchemaRequest, resp *datasource.SchemaResponse) {
      |  resp.Schema = schema.Schema{
      |    Attributes: map[string]schema.Attribute{
      |      "code": schema.Int32Attribute{
      |        Required: true,
      |      },
      |      "message": schema.StringAttribute{
      |        Optional: true,
      |      },
      |      "details": schema.ListAttribute{
      |        Optional: true,
      |        ElementType: basetypes.StringType{},
      |      },
      |    },
      |  }
      |}
      |func (r *SampleDataSource) Read(ctx context.Context, req datasource.ReadRequest, resp *datasource.ReadResponse) {
      |  var state SampleDataSourceModel
      |  resp.Diagnostics.Append(req.Config.Get(ctx, &state)...)
      |  if resp.Diagnostics.HasError() {
      |    return
      |  }
      |  if data.Details != nil {
      |    detailsElements := []types.String{}
      |    for _, element := range(*data.Details) {
      |      detailsElements = append(detailsElements, types.StringValue(element))
      |    }
      |    state.Details = detailsElements
      |  }
      |  state.Message = types.StringPointerValue(data.Message)
      |  state.Code = types.Int32Value(int32(data.Code))
      |  resp.Diagnostics.Append(resp.State.Set(ctx, &state)...)
      |}""".stripMargin.trim
    )
  }
}
