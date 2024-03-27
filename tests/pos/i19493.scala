
import scala.compiletime.{summonAll, summonInline}
import deriving.Mirror

type Sc[X] = X
case class Row[T[_]](name: T[String])

class DialectTypeMappers:
  given String = ???

inline def metadata(dialect: DialectTypeMappers)(using m: Mirror.Of[Row[Sc]]): m.MirroredElemTypes =
  import dialect.given
  summonAll[m.MirroredElemTypes]

def f = metadata(???)


object Minimization:

  class GivesString:
    given aString: String = ???

  inline def foo(x: GivesString): Unit =
    import x.aString
    summon[String]
    summonInline[String] // was error

  foo(???)
end Minimization
