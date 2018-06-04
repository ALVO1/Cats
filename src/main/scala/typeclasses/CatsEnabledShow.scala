package typeclasses

import cats._
import cats.implicits._

object CatsEnabledCatShow {
  implicit val catShow: Show[Cat] =
    (cat: Cat) => s"${cat.name} is a ${cat.age} years old ${cat.color} cat."
}

object CatsEnabledShow extends App {

  import CatsEnabledCatShow._

  Console.println(45.show)
  Console.println("String show test".show)
  Console.println(Cat("Misha", 6, "Red").show)
}
