package org.richardweiss.nmf.dense

import no.uib.cipr.matrix.{MatrixEntry, DenseMatrix, AbstractMatrix}
import no.uib.cipr.matrix.Matrices._
import scala.collection.JavaConversions._
import util.Random
import java.io.FileWriter
import org.richardweiss.nmf._
import no.uib.cipr.matrix.sparse.{FlexCompColMatrix, FlexCompRowMatrix}
import collection.mutable

/**
 * Find the Non-negative factorization based on euclidian distance. This uses
 * the multaplicative update method.
 *
 * <b>Note that if v is a sparse matrix, only those entries which are in it are considered in the distance function</b>
 * In order to achieve this, if any value in w or h becomes NaN, it is reset to zero
 * @param v - the matrix being factored
 * @param r - the extra dimension for w and h
 * @param minDistance - The minimum distance required
 * @param maxIterations - The maximum iterations allowed.
 */
class EuclidianNMF(val v: AbstractMatrix, val r: Int, val minDistance: Double, val maxIterations: Int)
  extends NMF with mutable.Publisher[Int] {
  private val rand = new Random()
  val n = v.numRows()
  val m = v.numColumns()
  type Pub = EuclidianNMF
  // The following are used in some proofs to check dimensions of the
  // results.
  type N <: Nat
  type M <: Nat
  type R <: Nat
  def distance(a: AbstractMatrix, b: AbstractMatrix): Double = {
    var eDistance: Double = 0
    for (e <- v)
      eDistance += math.pow(a.get(e.row(), e.column()) - b.get(e.row(), e.column()), 2)
    eDistance
  }
  def distanceWH(w: FlexCompRowMatrix, h: FlexCompColMatrix): Double = {
    var eDistance: Double = 0
    for (e <- v)
      eDistance += math.pow(e.get - w.getRow(e.row()).dot(h.getColumn(e.column())), 2)
    eDistance
  }

  private def validMatrixTest(matrix: AbstractMatrix) {
    for (e <- matrix) {
      if (e.get().isNaN) {
        println("NAN found at %s".format((e.row(), e.column()).toString()))
      } else if (e.get().isInfinite) {
        println("Inifnity found at %s".format((e.row(), e.column()).toString()))
      }
      assert(!e.get().isNaN)
      assert(!e.get().isInfinite)
    }
    for (r <- new VectorRowIterator(matrix)) assert(r.forall(_ != 0))
    for (c <- new VectorColIterator(matrix)) assert(c.forall(_ != 0))
  }
  private def validVTest(matrix: AbstractMatrix){
    for (r <- new VectorRowIterator(matrix)) assert(r.forall(_ != 0))
    for (c <- new VectorColIterator(matrix)) assert(c.forall(_ != 0))
  }
  private val TESTS = false
  private def compute(): (
      AbstractMatrix,
      AbstractMatrix,
      NMF_ExitReason.EuclidianNMF_ExitReason,
      Double)
  = {
    val tmpW = new FlexCompRowMatrix(n, r)
    val tmpH = new FlexCompColMatrix(r, m)
    random(tmpW)
    random(tmpH)

    val vP = Matrix.matrix[N, M]
    val hP = Matrix.matrix[R, M]
    val wP = Matrix.matrix[N, R]
    var iterations = 0
    var delta = 0.0

    val hNomMat = new DenseMatrix(r, m)
    val hNomP = Matrix.matrix[R, M]
    val hDenomMat = new DenseMatrix(r, m)
    val hDenomP = Matrix.matrix[R, M]
    val hDenomWTW = new DenseMatrix(r, r)
    val hDenomWTWP = Matrix.matrix[R, R]

    val wNomMat = new DenseMatrix(n, r)
    val wNomP = Matrix.matrix[N, R]
    val wDenomMat = new DenseMatrix(n, r)
    val wDenomP = Matrix.matrix[N, R]
    val wDenomWWTMat = new DenseMatrix(r, r)
    val wDenomWWTP = Matrix.matrix[R,R]

    var previousDistance = 0.0
    if (TESTS) validMatrixTest(v)
    if (TESTS) validVTest(v)
    if (TESTS) validMatrixTest(tmpH)
    if (TESTS) validMatrixTest(tmpW)

    do {
      previousDistance = delta
      publish(iterations)
      tmpW.transAmult(v, hNomMat)
      if (TESTS) wP.transpose.multiplyAndPack(vP, hNomP)
      if (TESTS) validMatrixTest(hNomMat)
      if (TESTS) for (e <- hNomMat) assert(e.get() != 0)

      tmpW.transAmult(tmpW, hDenomWTW)
      if (TESTS) wP.transpose.multiplyAndPack(wP, hDenomWTWP)
      if (TESTS) validMatrixTest(hDenomWTW)

      hDenomWTW.mult(tmpH, hDenomMat)
      if (TESTS) hDenomWTWP.multiplyAndPack(hP, hDenomP)
      if (TESTS) validMatrixTest(hDenomMat)

      for (i <- 0 until r)
        for (j <- 0 until m) {
          tmpH.set(i, j, tmpH.get(i, j) * hNomMat.get(i, j) / hDenomMat.get(i, j))
        }
      for (e <- tmpH) if (e.get().isNaN) e.set(0)

      if (TESTS) for (e <- tmpH) if (e.get().isNaN) e.set(0)
      if (TESTS) for (e <- tmpH) assert(e.get() != 0)
      if (TESTS) validMatrixTest(tmpH)

      v.transBmult(tmpH, wNomMat)
      if (TESTS) vP.multiplyAndPack(hP.transpose, wNomP)
      if (TESTS) validMatrixTest(wNomMat)

      tmpH.transBmult(tmpH, wDenomWWTMat)
      if (TESTS) hP.multiplyAndPack(hP.transpose, wDenomWWTP)
      if (TESTS) validMatrixTest(wDenomWWTMat)

      tmpW.mult(wDenomWWTMat, wDenomMat)
      if (TESTS) wP.multiplyAndPack(wDenomWWTP, wDenomP)
      if (TESTS) validMatrixTest(wDenomMat)

      for (i <- 0 until n)
        for (j <- 0 until r) {
          tmpW.set(i, j, tmpW.get(i, j) * wNomMat.get(i, j) / wDenomMat.get(i, j))
        }
      for (e <- tmpW) if (e.get().isNaN) e.set(0)

      if (TESTS) validMatrixTest(tmpW)

      delta = distanceWH(tmpW, tmpH)
      iterations += 1
      //println(delta)
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

  var w: AbstractMatrix = null
  var h: AbstractMatrix = null
  var finalExitReason: NMF_ExitReason.EuclidianNMF_ExitReason = null
  var finalDistance: Double = Double.NaN
  def run() {
    val results = compute()
    w = results._1
    h = results._2
    finalExitReason = results._3
    finalDistance = results._4
  }
}

/**
 * Find the Non-negative factorization based on euclidian distance. This uses
 * the multaplicative update method.
 *
 * <b>Note that if v is a sparse matrix, only those entries which are in it are considered in the distance function</b>
 * In order to achieve this, if any value in w or h becomes NaN, it is reset to zero.
 * @param v - the matrix being factored
 * @param r - the extra dimension for w and h
 * @param minDistance - The minimum distance required
 * @param maxIterations - The maximum iterations allowed.
 * @param numInstances - optional (default 2) the number of runs to do
 * @param parallel - optional (default true) whether to allow parallel execution of the runs.
 */
class MultiEuclidianNMF(
                         val v: AbstractMatrix,
                         val r: Int,
                         val minDistance: Double,
                         val maxIterations: Int,
                         val numInstances: Int,
                         val parallel: Boolean) extends NMF {
  def this(v: AbstractMatrix, r: Int, minDelta: Double, maxIterations: Int) =
    this(v, r, minDelta, maxIterations, 2, true)
  def this(v: AbstractMatrix, r: Int, minDelta: Double, maxIterations: Int, numInstances: Int) =
    this(v, r, minDelta, maxIterations, numInstances, true)

  private val allResults = if (parallel) {
    (0 until numInstances).par.map {
      _ => EuclidianNMF(v, r, minDistance, maxIterations)
    }
  } else {
    (0 until numInstances).map {
      _ => EuclidianNMF(v, r, minDistance, maxIterations)
    }
  }
  allResults.map(_.run())
  private val result: EuclidianNMF = allResults.minBy(x => x.finalDistance)
  var w = result.w
  var h = result.h
  var finalExitReason = result.finalExitReason
  var finalDistance = result.finalDistance
  var worstDistance = allResults.maxBy(x => x.finalDistance).finalDistance
}


object EuclidianNMF {
  def apply(v: AbstractMatrix, r: Int, minDistance: Double, maxIterations: Int) = {
    val nmf = new EuclidianNMF(v,r,minDistance, maxIterations)
    nmf.run()
    nmf
  }
  def apply(v: AbstractMatrix, r: Int, minDistance: Double, maxIterations: Int, subscribers: mutable.Subscriber[Int, EuclidianNMF]*) = {
    val nmf = new EuclidianNMF(v,r,minDistance, maxIterations)
    for (s <- subscribers) nmf.subscribe(s)
    nmf.run()
    nmf
  }
}