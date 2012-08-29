package org.richardweiss.nmf.dense

import no.uib.cipr.matrix.{MatrixEntry, DenseMatrix, AbstractMatrix}
import no.uib.cipr.matrix.Matrices._
import breeze.linalg
import metascala.Nats.Nat
import scala.collection.JavaConversions._
import util.Random
import java.io.FileWriter
import org.richardweiss.nmf._

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
class EuclidianNMF(val v: linalg.Matrix[Double], val r: Int, val minDistance: Double, val maxIterations: Int) extends NMF {
  private val rand = new Random()
  val n = v.rows
  val m = v.cols

  // The following are used in some proofs to check dimensions of the
  // results.
  type N <: Nat
  type M <: Nat
  type R <: Nat
  val x = linalg.CSCMatrix.zeros(3,3).activeIterator
  def distance(a: linalg.Matrix[Double], b: linalg.Matrix[Double]): Double = {
    var eDistance: Double = 0
    for (e <- v.activeValuesIterator)
      eDistance += math.pow(e, 2)
    eDistance
  }

  private def validMatrixTest(matrix: linalg.Matrix[Double]) {
    for (e <- matrix.activeIterator) {
      if (e._2.isNaN) {
        println("NAN found at %s".format(e._1.toString()))
      } else if (e._2.isInfinite) {
        println("Inifnity found at %s".format(e._1.toString()))
      }
      assert(!e._2.isNaN)
      assert(!e._2.isInfinite)
    }
    for (r <- new VectorRowIterator(matrix)) assert(r.forall(_ != 0))
    for (c <- new VectorColIterator(matrix)) assert(c.forall(_ != 0))
  }
  private def validVTest(matrix: AbstractMatrix){
    for (r <- new VectorRowIterator(matrix)) assert(r.forall(_ != 0))
    for (c <- new VectorColIterator(matrix)) assert(c.forall(_ != 0))
  }
  private val TESTS = false
  private def run(): (
    DenseMatrix,
      DenseMatrix,
      NMF_ExitReason.EuclidianNMF_ExitReason,
      Double)
  = {
    val tmpW = new DenseMatrix(n, r)
    val tmpH = new DenseMatrix(r, m)
    random(tmpW)
    random(tmpH)

    val newMatrix: DenseMatrix = new DenseMatrix(v.numRows(), v.numColumns()).zero().asInstanceOf[DenseMatrix]
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
    tmpW.mult(tmpH, newMatrix)
    if (TESTS) validMatrixTest(v)
    if (TESTS) validVTest(v)
    if (TESTS) validMatrixTest(tmpH)
    if (TESTS) validMatrixTest(tmpW)

    do {
      previousDistance = delta

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

      tmpW.mult(tmpH, newMatrix)
      delta = distance(v, newMatrix)
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

  private val results = run()
  val w = results._1
  val h = results._2
  val finalExitReason = results._3
  val finalDistance = results._4
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
      _ => new EuclidianNMF(v, r, minDistance, maxIterations)
    }
  } else {
    (0 until numInstances).map {
      _ => new EuclidianNMF(v, r, minDistance, maxIterations)
    }
  }
  private val result: EuclidianNMF = allResults.minBy(x => x.finalDistance)
  val w = result.w
  val h = result.h
  val finalExitReason = result.finalExitReason
  val finalDistance = result.finalDistance
  val worstDistance = allResults.maxBy(x => x.finalDistance).finalDistance
}

