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
      |    },
      |  }
      |}
      |type SampleDataSourceModel struct {
      |  Code types.Int32 `tfsdk:"code"`
      |  Message types.String `tfsdk:"message"`
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
      |    },
      |  }
      |}
      |func (r *SampleDataSource) Read(ctx context.Context, req datasource.ReadRequest, resp *datasource.ReadResponse) {
      |  var state SampleDataSourceModel
      |  resp.Diagnostics.Append(req.Config.Get(ctx, &state)...)
      |  if resp.Diagnostics.HasError() {
      |    return
      |  }
      |  state.Message = types.StringPointerValue(data.Message)
      |  state.Code = types.Int32Value(int32(data.Code))
      |  resp.Diagnostics.Append(resp.State.Set(ctx, &state)...)
      |}""".stripMargin.trim
    )
  }
}
