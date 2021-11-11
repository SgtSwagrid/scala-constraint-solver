package solver

case class Variable[T](name: String, domain: Set[T])