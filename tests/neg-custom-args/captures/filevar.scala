import language.experimental.captureChecking
import compiletime.uninitialized

class File:
  def write(x: String): Unit = ???

class Service:
  var file: File^ = uninitialized
  def log = file.write("log")

def withFile[T](op: (l: caps.Cap) ?-> (f: File^{l}) => T): T =
  op(new File)

def test =
  withFile: f =>
    val o = Service()
    o.file = f  // error
    o.log
