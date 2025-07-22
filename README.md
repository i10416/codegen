# codegen

OpenAPI schema to terraform provider boilerplate compiler（alpha）

 ## About
This is a tool to convert OpenAPI schema into Golang to make Terraform Provider development easier.

## Usage & Example

```sh
Usage: codegen --namespace <string> [--mode <string>] <JSONSchema path> <model>

terraform provider generator

Options and flags:
    --help
        Display this help text.
    --namespace <string>, -n <string>, -s <string>
        module namespace
    --mode <string>, -m <string>
        either datasource or resource
```

### Example

```sh
./mill --ticker=false modules.codegen.run \
   sdk/piano/spec/json/publisher.json Term  \
   --namespace piano_publisher
```