package bucharestfp

object Lexer {
  def lex[S](chars: Reader[Char, S]): Reader[Token, S] = {
    new Reader[Token, S] {
      def read(stream: S) = {
        val trimmed = skipWhitespace(chars, stream)

        lexInteger(chars).read(trimmed) match {
          case some @ Some(_) => some
          case None =>
            lexSymbol(chars).read(trimmed) match {
              case some @ Some(_) => some
              case None =>
                lexIdentifier(chars).read(trimmed) match {
                  case some @ Some(_) => some
                  case None => None
                }
            }
        }
      }
    }
  }

  def skipWhitespace[S](chars: Reader[Char, S], stream: S): S = {
    chars.read(stream) match {
      case Some(char -> stream) if Character.isWhitespace(char) =>
        skipWhitespace(chars, stream)
      case _ => stream
    }
  }

  def lexInteger[S](chars: Reader[Char, S]): Reader[Token, S] = {
    def loop(stream: S, result: Int): Option[(Token, S)] = {
      chars.read(stream) match {
        case None => Some(Token.INT(result) -> stream)
        case Some((char, restStream)) =>
          if (Character.isDigit(char)) {
            loop(restStream, result * 10 + Character.digit(char, 10))
          } else {
            Some(Token.INT(result) -> stream)
          }
      }
    }

    new Reader[Token, S] {
      def read(stream: S) = {
        chars.read(stream) match {
          case None => None
          case Some(('0', restStream)) => Some(Token.INT(0) -> restStream)
          case Some((char, restStream)) =>
            if (Character.isDigit(char)) {
              loop(restStream, Character.digit(char, 10))
            } else {
              None
            }
        }
      }
    }
  }

  def keyword(name: String): Token = {
    name match {
      case "if" => Token.IF
      case "then" => Token.THEN
      case "else" => Token.ELSE
      case "let" => Token.LET
      case "val" => Token.VAL
      case "in" => Token.IN
      case "end" => Token.END
      case "fn" => Token.FN
      case "true" => Token.TRUE
      case "false" => Token.FALSE
      case other => Token.VAR(other)
    }
  }

  def lexIdentifier[S](chars: Reader[Char, S]): Reader[Token, S] = {
    def loop(stream: S, result: String): Option[(Token, S)] = {
      chars.read(stream) match {
        case None => Some(keyword(result) -> stream)
        case Some((char, restStream)) =>
          if (Character.isLetter(char)) {
            loop(restStream, result + char.toString)
          } else {
            Some(keyword(result) -> stream)
          }
      }
    }

    new Reader[Token, S] {
      def read(stream: S) = {
        chars.read(stream) match {
          case None => None
          case Some((char, restStream)) =>
            if (Character.isLetter(char)) {
              loop(restStream, char.toString)
            } else {
              None
            }
        }
      }
    }
  }

  def lexSymbol[S](chars: Reader[Char, S]): Reader[Token, S] = {
    new Reader[Token, S] {
      def read(stream: S) = {
        chars.read(stream) match {
          case Some('(' -> rest) => Some(Token.LPAREN -> rest)
          case Some(')' -> rest) => Some(Token.RPAREN -> rest)
          case Some('+' -> rest) => Some(Token.ADD -> rest)
          case Some('-' -> rest) => Some(Token.SUB -> rest)
          case Some('=' -> rest) =>
            chars.read(rest) match {
              case Some('>' -> rest) => Some(Token.DARROW -> rest)
              case _ => Some(Token.EQUAL -> rest)
            }
          case _ => None
        }
      }
    }
  }
}
