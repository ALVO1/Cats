import TypeClass.{JsonInterfaceUsage, JsonSyntaxUsage}

/**
  * A type class is an interface or API that represents some functionality we
  * want to implement.
  */
object TypeClass {

  sealed trait Json
  object Json {
    final case class JsObject(get: Map[String, Json]) extends Json
    final case class JsString(get: String) extends Json
    final case class JsNumber(get: Double) extends Json
  }

  trait JsonWriter[A] {
    def write(content: A): Json
  }

  final case class Person(name: String, email: String, age: Int)

  object JsonWriterInstances {
    import Json.{JsString, JsObject, JsNumber}

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

  //Usage of type-class interface:
  object JsonInterfaceUsage {
    import JsonWriterInstances._

    def json: Json = JsonInterface.toJson(Person("Alvo", "testmail@test.com", 24))
  }

  object JsonSyntaxUsage {
    import JsonSyntax._
    import JsonWriterInstances._

    val json: Json = Person("Alvo", "testmail@test.com", 24).toJson
  }
}

object TypeClassRunner extends App {
  println(JsonInterfaceUsage.json)
  println(JsonSyntaxUsage.json)
}