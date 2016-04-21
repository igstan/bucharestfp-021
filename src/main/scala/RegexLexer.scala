package bucharestfp

import java.util.regex.Pattern

object RegexLexer {
  sealed trait Action
  case object Continue extends Action
  case class Failure(where: String) extends Action
  case class Success(token: Token) extends Action

  val patterns = List[(String, String => Action)](
    "(\\s+)"          -> { lexeme => Continue },
    "(0|[1-9][0-9]*)" -> { lexeme => Success(Token.INT(lexeme.toInt)) },
    "(=>)"            -> { lexeme => Success(Token.DARROW) },
    "(=)"             -> { lexeme => Success(Token.EQUAL) },

    """ \"[^"]*\" """             -> { lexeme => Success(Token.STRING(lexeme)) },

    "(\\+)"           -> { lexeme => Success(Token.ADD) },
    "(-)"             -> { lexeme => Success(Token.SUB) },
    "(\\()"           -> { lexeme => Success(Token.LPAREN) },
    "(\\))"           -> { lexeme => Success(Token.RPAREN) },
    "(if\\b)"         -> { lexeme => Success(Token.IF) },
    "(then\\b)"       -> { lexeme => Success(Token.THEN) },
    "(else\\b)"       -> { lexeme => Success(Token.ELSE) },
    "(let\\b)"        -> { lexeme => Success(Token.LET) },
    "(val\\b)"        -> { lexeme => Success(Token.VAL) },
    "(in\\b)"         -> { lexeme => Success(Token.IN) },
    "(end\\b)"        -> { lexeme => Success(Token.END) },
    "(fn\\b)"         -> { lexeme => Success(Token.FN) },
    "(true\\b)"       -> { lexeme => Success(Token.TRUE) },
    "(false\\b)"      -> { lexeme => Success(Token.FALSE) },
    "([a-zA-Z]+)"     -> { lexeme => Success(Token.VAR(lexeme)) },
    "(.)"             -> { lexeme => Failure(lexeme) }
  )

  val pattern = Pattern.compile(patterns.map(_._1).mkString("|"))

  def lex(source: String): Either[String, Seq[Token]] = {
    val matcher = pattern.matcher(source)

    def loop(tokens: Vector[Token]): Either[String, Vector[Token]] = {
      if (!matcher.find()) {
        Right(tokens)
      } else {
        val result = patterns
          .zipWithIndex
          .find { case ((_, handler), i) => matcher.group(i + 1) != null }
          .map { case ((_, handler), i) => handler(matcher.group(i + 1)) }

        result match {
          case None => Right(tokens)
          case Some(Continue) => loop(tokens)
          case Some(Failure(where)) => Left(s"unexpected: $where")
          case Some(Success(token)) => loop(tokens :+ token)
        }
      }
    }

    loop(Vector.empty)
  }

  def main(args: Array[String]): Unit = {
    val source = """
    |
    |  let
    |    val inc = fn a => a + 1
    |  in
    |    if isZero n
    |    then inc 42
    |    else false
    |  end
    """.trim.stripMargin

    lex(source) match {
      case Left(error) => println(s"lexical error: $error")
      case Right(tokens) => println(s"tokens: $tokens")
    }
  }
}
