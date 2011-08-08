case class Configuration(
  hostname: String
, port: Int
, outfile: String
)

abstract class ConfigReader[A] {
  def apply(config: Configuration): A

  def map[B](f: A => B): ConfigReader[B] = {
    new ConfigReader[B] {
      def apply(c: Configuration) =
        f(ConfigReader.this.apply(c))
    }
  }

  def flatMap[B](f: A => ConfigReader[B]): ConfigReader[B] = {
    new ConfigReader[B] {
      def apply(c: Configuration) =
        f(ConfigReader.this.apply(c))(c)
    }
  }
}

object ConfigReader {
  // (A => B => C => D) => (CR[A] => CR[B] => CR[C] => CR[D])
  def lift3ConfigReader[A, B, C, D](f: A => B => C => D):
    ConfigReader[A] =>
    ConfigReader[B] =>
    ConfigReader[C] =>
    ConfigReader[D] =
      a => b => c =>
        for {
          aa <- a
          bb <- b
          cc <- c
        } yield f(aa)(bb)(cc)
}

object Main {
  import ConfigReader._
  def main(args: Array[String]) {
    // utility construction
    def configReader[A](k: Configuration => A): ConfigReader[A] =
      new ConfigReader[A] {
        def apply(c: Configuration) = k(c)
      }

    val hostname = configReader(conf => conf.hostname)
    val port = configReader(_.port)
    val outfile = configReader(_.outfile)

    val conf = Configuration("localhost", 80, "/etc/hosts")

    val r: ConfigReader[String] =
      for {
        h <- hostname
        p <- port
        o <- outfile
      } yield "Hello there " + h + ":" + p + "! Want " + o + "?"
    println(r(conf))

    val r2: ConfigReader[String] =
      hostname.flatMap(h =>
      port.flatMap(p =>
      outfile.map(o =>
          "Hi %s:%s, %s?" format (h, p, o)
      )))
    println(r2(conf))

    val lifted: ConfigReader[String] = lift3ConfigReader(
      (h: String) =>
      (p: Int) =>
      (o: String) =>
      "Lifted %s:%s, %s?" format (h, p, o)
    )(hostname)(port)(outfile)
    println(lifted(conf))
  }
}
