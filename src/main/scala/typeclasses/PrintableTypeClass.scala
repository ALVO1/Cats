package typeclasses

object PrintableTypeClass {

  trait PrintableFormatter[A] {
    def format(content: A): String
  }

  object PrintableInstances {
    implicit val stringFormatter: PrintableFormatter[String] =
      (content: String) => content

    implicit val intFormatter: PrintableFormatter[Int] =
      (content: Int) => String.valueOf(content)

    implicit val catFormatter: PrintableFormatter[Cat] =
      (cat: Cat) => s"${cat.name} is a ${cat.age} years old ${cat.color} cat."
  }

  object Printable {
    def format[A](value: A)(implicit formatter: PrintableFormatter[A]): String =
      formatter.format(value)

    def print[A](value: A)(implicit formatter: PrintableFormatter[A]): Unit =
      Console.println(formatter.format(value))
  }

  object PrintableSyntax {

    implicit class PrintableOps[A](value: A) {
      def print(implicit formatter: PrintableFormatter[A]): Unit =
        Console.println(formatter.format(value))
    }

  }

  object PrintableInterfaceUsage {

    import PrintableInstances._

    def print(): Unit = {
      Printable.print("String Test")
      Printable.print(42)
    }
  }

  object PrintableSyntaxUsage {

    import PrintableInstances._
    import PrintableSyntax._

    def print(): Unit = {
      "String Test".print
      42.print
      Cat("Hiocha", 5, "Black").print
    }
  }

}

final case class Cat(name: String, age: Int, color: String)

object PrintableTypeClassTest extends App {

  import typeclasses.PrintableTypeClass.PrintableInterfaceUsage
  import typeclasses.PrintableTypeClass.PrintableSyntaxUsage

  PrintableInterfaceUsage.print()
  PrintableSyntaxUsage.print()
}
