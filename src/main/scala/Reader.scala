package bucharestfp

trait Reader[E, S] {
  def read(stream: S): Option[(E, S)]
}

object Reader {
  def apply[E, S](fn: S => Option[(E, S)]): Reader[E, S] =
    new Reader[E, S] {
      def read(stream: S): Option[(E, S)] = fn(stream)
    }

  val string: Reader[Char, String] =
    new Reader[Char, String] {
      def read(stream: String): Option[(Char, String)] = {
        stream match {
          case "" => None
          case _ => Some(stream.head -> stream.tail)
        }
      }
    }

  def consume[E, S](reader: Reader[E, S], source: S): List[E] = {
    @annotation.tailrec
    def loop(source: S, result: List[E]): List[E] = {
      reader.read(source) match {
        case None => result.reverse
        case Some(t -> rest) => loop(rest, t :: result)
      }
    }

    loop(source, List.empty)
  }
}
