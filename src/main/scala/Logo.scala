import Logo._
import cats.free.Free
import cats.{Id, ~>}

object Logo {

  type InstructionFree[A] = Free[Instruction, A]

  final case class Position(x: Double, y: Double, heading: Degree)
  final case class Degree(private val d: Int){
    val value: Int = d % 360
  }

  sealed trait Instruction[A]
  final case class Forward(position: Position, length: Int) extends Instruction[Position]
  final case class Backward(position: Position, length: Int) extends Instruction[Position]
  final case class RotateLeft(position: Position, degree: Degree) extends Instruction[Position]
  final case class RotateRight(position: Position, degree: Degree) extends Instruction[Position]
  final case class ShowPosition(position: Position) extends Instruction[Unit]

  def forward(position: Position, length: Int): InstructionFree[Position] = Free.liftF(Forward(position, length))
  def backward(position: Position, length: Int): InstructionFree[Position] = Free.liftF(Backward(position, length))
  def rotateRight(position: Position, degree: Degree): InstructionFree[Position] = Free.liftF(RotateRight(position, degree))
  def rotateLeft(position: Position, degree: Degree): InstructionFree[Position] = Free.liftF(RotateLeft(position, degree))
  def showPosition(position: Position): InstructionFree[Unit] = Free.liftF(ShowPosition(position))
}

// object Interpreter extends (Instruction ~> Option) {
//   import Logo._

//   val nonNegative: (Position) => Option[Position] = {
//     p => if (p.x >= 0 && p.y >= 0) Some(p) else None
//   }

//   override def apply[A](fa: Instruction[A]) = fa match {
//     case Forward(p, length) => nonNegative(forward(p, length))
//     case Backward(p, length) => nonNegative(backward(p, length))
//     case RotateLeft(p, degree) => Some(rotateLeft(p, degree))
//     case RotateRight(p, degree) => Some(rotateRight(p, degree))
//     case ShowPosition(p) => Some(println(s"showing position $p"))
//   }
// }

// object LogoRunner extends App {

//   import Logo._

//   // val program: Position => InstructionFree[Position] = {
//   //   start: Position =>
//   //     for {
//   //       pos1 <- forward(start, 10)
//   //       pos2 <- rotateRight(pos1, Degree(90))
//   //       pos3 <- forward(pos2, 10)
//   //     } yield pos3
//   // }

//   val program: (Position => Free[Instruction, Unit]) = {
//     s: Position =>
//       for {
//         p1 <- forward(s, 10)
//         p2 <- rotateRight(p1, Degree(90))
//         p3 <- forward(p2, 10)
//         p4 <- backward(p3, 20)//Here the computation stops, because result will be None
//         _ <- showPosition(p4)
//       } yield ()
//   }

//   val startPos = Position(0.0D, 0.0D, Degree(0))
//   val resultPos = program(startPos).foldMap(Interpreter)
//   println(resultPos)
// }
