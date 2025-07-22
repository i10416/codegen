package codegen

import codegen.Mode.DataSource
import codegen.Mode.Resource
import pseudogo.TypeIdent

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
