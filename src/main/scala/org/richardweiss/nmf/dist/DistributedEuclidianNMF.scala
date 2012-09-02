package org.richardweiss.nmf.dist

import org.richardweiss.nmf.{NMF_ExitReason, VectorColIterator, NMF}
import spark.{RDD, SparkContext}
import SparkContext._
import no.uib.cipr.matrix._
import org.richardweiss.nmf.util.PimpedMatrix._
import scala.collection.JavaConversions._
import no.uib.cipr.matrix.sparse.SparseVector
import util.Random


class DistributedEuclidianNMF[N, M, R](val v: DistVectorMatrix[N, M],
                              val r: Int,
                              val minDistance: Double,
                              val maxIterations: Int,
                              val context: SparkContext){

  val n = v.rows
  val m = v.cols

  def distance(a: DistVectorMatrix[N, M], b: DistVectorMatrix[N, M]): Double = {
    val c = a.add(b.scale(-1))
    val d = c.hadamard(c)
    d.matrix.map{r =>
      var sum = 0.0
      for (e <- r._2) sum += e.get
      sum
    }.reduce(_ + _)
  }

  def run(): (
      DistVectorMatrix[N, R],
      DistVectorMatrix[R, M],
      NMF_ExitReason.EuclidianNMF_ExitReason,
      Double) = {
    val wInner = context.parallelize((0 until n).map{k =>
        val mat = new SparseVector(r)
        val rand = new Random()
        for (i <- 0 until r) mat.set(i, rand.nextDouble())
        k -> mat
    })
    val hInner = context.parallelize((0 until m).map{k =>
      val mat = new SparseVector(r)
      val rand = new Random()
      for (i <- 0 until r) mat.set(i, rand.nextDouble())
      k -> mat
    })
    var tmpW = new DistColVectorMatrix[N,R](wInner, n, r)
    var tmpH = new DistRowVectorMatrix[R,M](hInner, r, m)
    var previousDistance = 0.0
    var delta = 0.0
    var iterations = 0
    do {
      println("Iterations" + iterations)
      previousDistance = delta
      val hNomMat = tmpW.transpose.multiply(v)
      val hDenomMat = tmpW.transpose.multiply(tmpW).multiply(tmpH)
      println(hNomMat.dimensions)
      println(hDenomMat.dimensions)
      assert(hNomMat.dimensions == (r, m))
      assert(hDenomMat.dimensions == (r, m))

      tmpH = tmpH.hadamard(hNomMat.inverseHadamard(hDenomMat))
      tmpH.map{x: Double => if (x.isNaN) 0.0 else x}
      val wNomMat = v.multiply(tmpH.transpose)
      val wDenomMat = tmpW.multiply(tmpH.multiply(tmpH.transpose))
      tmpW = tmpW.hadamard(wNomMat.inverseHadamard(wDenomMat))
      tmpW.map {x: Double => if (x.isNaN) 0.0 else x}
      val newMatrix = tmpW.multiply(tmpH)
      delta = distance(v, newMatrix)
      iterations += 1
    } while (iterations < maxIterations && delta > minDistance && previousDistance != delta)
    val finalDistance = delta
    val exitReason = if (iterations >= maxIterations) {
      NMF_ExitReason.MaxIterations
    } else if (delta <= minDistance) {
      NMF_ExitReason.MinimizedDistance
    } else {
      NMF_ExitReason.StableDistance
    }
    (tmpW, tmpH, exitReason, finalDistance)
  }
  val rs = run()
  val w = rs._1
  val h = rs._2
  val finalExitReason = rs._3
  val finalDistance = rs._4

}

object DistributedEuclidianNMF {
  /*def apply(v: AbstractMatrix,
            r: Int,
            minDistance: Double,
            maxIterations: Int)
           (implicit sparkContext: SparkContext): DistributedEuclidianNMF = {
    val mat: RDD[DenseVector] = sparkContext.parallelize(new VectorColIterator(v).toList)
  }*/
}