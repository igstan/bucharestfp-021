package bucharestfp

sealed trait Token

object Token {
  case class INT(value: Int) extends Token
  case class VAR(value: String) extends Token
  case object IF extends Token
  case object THEN extends Token
  case object ELSE extends Token
  case object FN extends Token
  case object DARROW extends Token
  case object LET extends Token
  case object VAL extends Token
  case object EQUAL extends Token
  case object IN extends Token
  case object END extends Token
  case object LPAREN extends Token
  case object RPAREN extends Token
  case object ADD extends Token
  case object SUB extends Token
  case object TRUE extends Token
  case object FALSE extends Token
}
