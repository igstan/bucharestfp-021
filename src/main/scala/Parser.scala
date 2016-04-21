package bucharestfp

object Parser {
  def parse[S](tokens: Reader[Token, S], stream: S): Absyn = {
    exp(tokens).read(stream) match {
      case None => throw new Exception("no tokens")
      case Some(absyn -> stream) =>
        tokens.read(stream) match {
          case Some(token -> stream) => throw new Exception(s"trailing token: $token")
          case None => absyn
        }
    }
  }

  def exp[S](tokens: Reader[Token, S]): Reader[Absyn, S] = {
    Reader { stream =>
      infexp(tokens).read(stream) match {
        case Some((exp, stream)) => Some((exp, stream))
        case None =>
          tokens.read(stream) match {
            case None => None
            case Some(Token.FN -> stream)     => parseFn(tokens).read(stream)
            case Some(Token.IF -> stream)     => parseIf(tokens).read(stream)
          }
      }
    }
  }

  def infexp[S](tokens: Reader[Token, S]): Reader[Absyn, S] = {
    def loop(a: Absyn, stream: S): Option[(Absyn, S)] = {
      tokens.read(stream) match {
        case Some((Token.ADD, stream)) =>
          app(tokens).read(stream) match {
            case Some((b, stream)) => loop(Absyn.ADD(a, b), stream)
            case None => throw new Exception("operator expected")
          }
        case Some((Token.SUB, stream)) =>
          app(tokens).read(stream) match {
            case Some((b, stream)) => loop(Absyn.SUB(a, b), stream)
            case None => throw new Exception("operator expected")
          }
        case _ => Some((a, stream))
      }
    }

    Reader { stream =>
      app(tokens).read(stream) match {
        case Some(a -> stream) => loop(a, stream)
        case None => None
      }
    }
  }

  def app[S](tokens: Reader[Token, S]): Reader[Absyn, S] = {
    def loop(a: Absyn, stream: S): Option[(Absyn, S)] = {
      atexp(tokens).read(stream) match {
        case None => Some(a -> stream)
        case Some(b -> stream) => loop(Absyn.APP(a, b), stream)
      }
    }

    Reader { stream =>
      atexp(tokens).read(stream) match {
        case Some(a -> stream) => loop(a, stream)
        case None => None
      }
    }
  }

  def atexp[S](tokens: Reader[Token, S]): Reader[Absyn, S] = {
    Reader { stream =>
      tokens.read(stream) match {
        case Some(Token.LET -> stream)    => parseLet(tokens).read(stream)
        case Some(Token.LPAREN -> stream) => parseParens(tokens).read(stream)
        case Some(Token.INT(n) -> stream) => Some(Absyn.INT(n) -> stream)
        case Some(Token.VAR(v) -> stream) => Some(Absyn.VAR(v) -> stream)
        case Some(Token.TRUE -> stream)   => Some(Absyn.BOOL(true) -> stream)
        case Some(Token.FALSE -> stream)  => Some(Absyn.BOOL(false) -> stream)
        case _                            => None
      }
    }
  }

  def parseFn[S](tokens: Reader[Token, S]): Reader[Absyn, S] = {
    Reader { stream =>
      tokens.read(stream) match {
        case Some(Token.VAR(param) -> stream) =>
          tokens.read(stream) match {
            case Some(Token.DARROW -> stream) =>
              exp(tokens).read(stream) match {
                case Some(body -> stream) => Some(Absyn.FN(param, body) -> stream)
                case _ => throw new Exception("EXP expected")
              }
            case _ => throw new Exception("DARROW expected")
          }
        case _ => throw new Exception("VAR expected")
      }
    }
  }

  def parseIf[S](tokens: Reader[Token, S]): Reader[Absyn, S] = {
    Reader { stream =>
      exp(tokens).read(stream) match {
        case None => throw new Exception("EXP expected")
        case Some(predicate -> stream) =>
          tokens.read(stream) match {
            case Some(Token.THEN -> stream) =>
              exp(tokens).read(stream) match {
                case None => throw new Exception("EXP expected")
                case Some(thenBranch -> stream) =>
                  tokens.read(stream) match {
                    case Some(Token.ELSE -> stream) =>
                      exp(tokens).read(stream) match {
                        case None => throw new Exception("EXP expected")
                        case Some(elseBranch -> stream) =>
                          Some(Absyn.IF(predicate, thenBranch, elseBranch) -> stream)
                      }
                    case _ => throw new Exception("ELSE expected")
                  }
              }
            case _ => throw new Exception("THEN expected")
          }
      }
    }
  }

  def parseLet[S](tokens: Reader[Token, S]): Reader[Absyn, S] = {
    Reader { stream =>
      tokens.read(stream) match {
        case Some(Token.VAL -> stream) =>
          tokens.read(stream) match {
            case Some(Token.VAR(name) -> stream) =>
              tokens.read(stream) match {
                case Some(Token.EQUAL -> stream) =>
                  exp(tokens).read(stream) match {
                    case None => throw new Exception("EXP expected")
                    case Some(value -> stream) =>
                      tokens.read(stream) match {
                        case Some(Token.IN -> stream) =>
                          exp(tokens).read(stream) match {
                            case None => throw new Exception("EXP expected")
                            case Some(body -> stream) =>
                              tokens.read(stream) match {
                                case Some(Token.END -> stream) =>
                                  Some(Absyn.LET(name, value, body) -> stream)
                                case None => throw new Exception("END expected")
                              }
                          }
                        case _ => throw new Exception("IN expected")
                      }
                  }
                case _ => throw new Exception("EQUAL expected")
              }
            case _ => throw new Exception("VAR expected")
          }
        case _ => throw new Exception("VAL expected")
      }
    }
  }

  def parseParens[S](tokens: Reader[Token, S]): Reader[Absyn, S] = {
    Reader { stream =>
      exp(tokens).read(stream) match {
        case None => throw new Exception("VAL expected")
        case Some(a -> stream) =>
          tokens.read(stream) match {
            case Some(Token.RPAREN -> stream) => Some(a -> stream)
            case _ => throw new Exception("RPAREN expected")
          }
      }
    }
  }
}
