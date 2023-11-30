import days.Day00
import utility.IDay

import java.io.File
import java.net.URL
import scala.io.Source

object TestRunner {
  val dayRunners: Array[IDay] = getDayRunners()  // Array(Day00)

  def getDayRunners(): Array[IDay] = {
    val classLoader = Thread.currentThread().getContextClassLoader
    val path = "days"
    val resources = classLoader.getResources(path.replace('.', '/')).asIterator()
    var resourcesList: List[URL] = Nil
    resources.forEachRemaining({x => resourcesList = x :: resourcesList})

    resourcesList.flatMap(resource => {
      val dir = new File(resource.toURI)

      if (dir.exists && dir.isDirectory) {
        dir.listFiles.filter(_.getName.endsWith(".class")).map(_.getName).map { className =>
          val classInPackage = className.replace(".class", "")
          val dayClass = Class.forName(s"$path.$classInPackage")
          if (dayClass.getInterfaces.contains(classOf[IDay])) {
            dayClass.getDeclaredConstructor().newInstance().asInstanceOf[IDay]
          } else {
            null
          }
        }.filter(_ != null)
      }
      else {
        Seq.empty
      }
    }
    ).toArray
  }

  def main(args: Array[String]): Unit = {
    val n: Int = args(0).toInt
    println(getDayResult(n))
  }

  def dayString(n: Int): String = if (n < 10) s"0$n" else s"$n"

  def getDayResult(n: Int): String = {
    val dayRunner: IDay = dayRunners(n)
    val source: Source = Source.fromFile(s"input/${dayString(n)}.txt")
    val input: String = try source.mkString finally source.close()
    val startTime = System.currentTimeMillis()
    val result = dayRunner.execute(input)
    val executionTime = System.currentTimeMillis() - startTime
    s"Day $n: (Part 1: ${result._1}, Part 2: ${result._2}) ${executionTime}ms"
  }
}
