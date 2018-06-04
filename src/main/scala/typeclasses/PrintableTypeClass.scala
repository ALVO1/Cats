package typeclasses

object PrintableTypeClass {

  sealed trait PrintableADT

  object PrintableADT {

    case class AsString(value: String) extends PrintableADT

    case class AsInt(value: Int) extends PrintableADT

  }

  trait PrintableFormatter[A] {
    def format(content: A): PrintableADT
  }

  object PrintableInstances {

    import PrintableADT._

    implicit val stringFormatter: PrintableFormatter[String] =
      (content: String) => AsString(content)

    implicit val intFormatter: PrintableFormatter[Int] =
      (content: Int) => AsInt(content)

    implicit val catFormatter: PrintableFormatter[Cat] =
      (cat: Cat) => AsString(s"${cat.name} is a ${cat.age} years old ${cat.color} cat.")
  }

  object Printable {
    def format[A](value: A)(implicit formatter: PrintableFormatter[A]): PrintableADT =
      formatter.format(value)

    def print[A](value: A)(implicit formatter: PrintableFormatter[A]): Unit =
      Console.println(formatter.format(value))
  }

  object PrintableSyntax {

    implicit class PrintableOps[A](value: A) {
      def asString(implicit formatter: PrintableFormatter[A]): PrintableADT =
        formatter.format(value)

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

object PrintableTest extends App {

  import typeclasses.PrintableTypeClass.PrintableInterfaceUsage
  import typeclasses.PrintableTypeClass.PrintableSyntaxUsage

  PrintableInterfaceUsage.print()
  PrintableSyntaxUsage.print()
}
