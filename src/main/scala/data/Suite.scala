package data

sealed trait Suite
case object Spade extends Suite
case object Heart extends Suite
case object Club extends Suite
case object Diamond extends Suite