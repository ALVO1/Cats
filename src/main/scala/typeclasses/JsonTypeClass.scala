package typeclasses

import typeclasses.TypeClass.Json.JsNull
import typeclasses.TypeClass.{JsonInterfaceUsage, JsonSyntaxUsage, JsonSyntaxUsageWithOption}

/**
  * A type class is an interface or API that represents some functionality we
  * want to implement.
  */
object TypeClass {

  final case class Person(name: String, email: String, age: Int)

  sealed trait Json

  object Json {

    final case class JsObject(get: Map[String, Json]) extends Json

    final case class JsString(get: String) extends Json

    final case class JsNumber(get: Double) extends Json

    final case object JsNull extends Json

  }

  trait JsonWriter[A] {
    def write(content: A): Json
  }

  object JsonWriterInstances {

    import Json.{JsNumber, JsObject, JsString}

    implicit val stringJsonWriter: JsonWriter[String] = (content: String) => JsString(content)

    implicit val numberJsonWriter: JsonWriter[Double] = (content: Double) => JsNumber(content)

    implicit val personJsonWriter: JsonWriter[Person] = (person: Person) => {
      JsObject(
        Map(
          "name" -> JsString(person.name),
          "email" -> JsString(person.email),
          "age" -> JsNumber(person.age)
        )
      )
    }

    implicit def optionWriter[A](implicit writer: JsonWriter[A]): JsonWriter[Option[A]] = {
      case Some(value: A) => writer.write(value)
      case None => JsNull
    }
  }

  /**
    * An interface is any functionality we expose to users. Interfaces to type
    * classes are generic methods that accept instances of the type class as
    * implicit parameters.
    */
  object JsonInterface {
    // appropriate implicit jsonWriter will be automatically captured from the context (JsonWriterInstances)
    def toJson[A](value: A)(implicit jsonWriter: JsonWriter[A]): Json = {
      jsonWriter.write(value)
    }
  }

  object JsonSyntax {
    implicit class JsonWriterOps[A](value: A) {
      def toJson(implicit writer: JsonWriter[A]): Json = writer.write(value)
    }
  }

  //Test value
  val person = Person("Alvo", "testmail@test.com", 24)

  //Usage of type-class interface
  object JsonInterfaceUsage {

    import JsonWriterInstances._

    def json: Json = JsonInterface.toJson(person)
  }

  //Usage of type-class syntax
  object JsonSyntaxUsage {

    import JsonSyntax._
    import JsonWriterInstances._

    def json: Json = person.toJson
  }

  object JsonSyntaxUsageWithOption {
    import JsonSyntax._
    import JsonWriterInstances._

    def json: Json = Option(person).toJson
  }
}

object TypeClassRunner extends App {
  println(JsonInterfaceUsage.json)
  println(JsonSyntaxUsage.json)
  println(JsonSyntaxUsageWithOption.json)
}