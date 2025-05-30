package codegen.text

import scala.collection.mutable.ListBuffer

def snake2Camel(str: String) =
  val elements = str.split("_")
  val (head, tail) = (elements.head, elements.tail)
  if tail.isEmpty then head
  else head + tail.map(_.capitalize).mkString

def snake2UpperCamel(str: String) = snake2Camel(str).capitalize
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
