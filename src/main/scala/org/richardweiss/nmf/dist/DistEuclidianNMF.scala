package org.richardweiss.nmf.dist

import org.richardweiss.nmf.{NMF_ExitReason, DistNMF, NMF}
import util.Random
import com.nicta.scoobi.lib.{LinearAlgebra, DMatrix}
import com.nicta.scoobi.Scoobi._
import scala.collection.mutable.ArrayBuffer
import com.nicta.scoobi.{Persister, ScoobiConfiguration, Emitter, DObject}
import com.nicta.scoobi.DObject._
import java.util.SortedMap
import LinearAlgebra._


class DistEuclidianNMF(val size: (Long, Long),
                       val v: DMatrix[Long, Double],
                       val r: Long,
                       val minDistance: Double,
                       val maxIterations: Int) extends DistNMF{
  type Matrix = DMatrix[Long, Double]
  val n: Long = size._1
  val m: Long = size._2
  def mult(a: Double, b: Double): Double = a*b
  def add(a: Double, b: Double): Double = a+b
  def distance(a: Matrix, b: Matrix): DObject[Double] = {
    var eDistance: Double = 0
    val r = a.byMatrix(DMatrix(b.map(x => x._1 -> (-1 * x._2))), mult, add)
    r.map(x => x._2*x._2).reduce(add)
  }
  var debug: DList[((Long, Long), Double)] = null
  private def run(): (Matrix, Matrix, NMF_ExitReason.EuclidianNMF_ExitReason, Double) = {
    val tmpWFile = (0.toLong until n).toDList.parallelDo(
      new BasicDoFn[Long, ((Long, Long), Double)] {
        private val rand = new Random()
        def process(input: Long, emitter: Emitter[((Long, Long), Double)]) {
          for (j <- 0.toLong until r){
            emitter.emit((input, j), rand.nextDouble()*10)
          }
        }
      }
    )
    debug = tmpWFile
    null
  }
  val w = null
  val h = null
  val finalExitReason = null
  val finalDistance = Double.NaN
}
