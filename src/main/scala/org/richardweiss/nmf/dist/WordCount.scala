package org.richardweiss.nmf.dist

//import com.nicta.scoobi.Scoobi._
import metascala.Nats
//import com.nicta.scoobi.lib.{LinearAlgebra, DMatrix}
import no.uib.cipr.matrix.Matrix
import spark._
import SparkContext._

/*
object WordCount extends ScoobiApp {
  def run() {
    val size = 1000
    val nums = (0 until size*size).map(x => x.toDouble)
    val zip: List[(Long, Long)] = (0 until size).map(x => (0 until size).map(y => x.toLong -> y.toLong)).flatten.toList
    val numsA = zip.zip(nums).toDList
    val numsB = zip.zip(nums).toDList
    val m1 = DMatrix(numsA)
    val m2 = DMatrix(numsB)
    val m3 = m1.byMatrix(m2, {(a: Double, b: Double) => a + b}, {(a: Double, b: Double) => a + b} )
    persist(toTextFile(m3, "/out", overwrite=true))
    //val nmf = new DistEuclidianNMF((10, 10), m1, 3, 10, 100)
    //persist(toTextFile(nmf.debug, "/debug", overwrite=true))
  }
}
*/

object SparkPi {
  def main(args: Array[String]) {
    if (args.length == 0) {
      System.err.println("Usage: SparkPi <host> [<slices>]")
      System.exit(1)
    }

    val spark = new SparkContext(args(0), "SparkPi",  System.getenv("SPARK_HOME"), List(System.getenv("SPARK_EXAMPLES_JAR")))
    val slices = if (args.length > 1) args(1).toInt else 2
    val n = 100000 * slices
    val count = spark.parallelize(1 to n, slices).map { i =>
      val x = math.random * 2 - 1
      val y = math.random * 2 - 1
      if (x*x + y*y < 1) 1 else 0
    }.reduce(_ + _)
    println("Pi is roughly " + 4.0 * count / n)
    System.exit(0)
  }
}
