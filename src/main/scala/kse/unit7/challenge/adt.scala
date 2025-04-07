package kse.unit7.challenge

object adt:

  enum Try[+V]:

    case Success(value: V)         extends Try[V]
    case Failure(error: Throwable) extends Try[Nothing]

    def flatMap[Q](f: V => Try[Q]): Try[Q] =
      this match
        case Failure(error) => Failure(error)
        case Success(value) => f(value)

    def map[Q](f: V => Q): Try[Q] =
      this match
        case Failure(error) => Failure(error)
        case Success(value) =>
          try Success(f(value))
          catch case error: Throwable => Failure(error)

  object Try:

    def apply[V](v: V): Try[V] =
      try Success(v)
      catch case error: Throwable => Failure(error)
