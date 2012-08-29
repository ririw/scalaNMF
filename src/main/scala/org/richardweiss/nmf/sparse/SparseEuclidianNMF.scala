package org.richardweiss.nmf.sparse

import no.uib.cipr.matrix.{MatrixEntry, DenseMatrix, AbstractMatrix}
import no.uib.cipr.matrix.Matrices._
import metascala.Nats.Nat
import scala.collection.JavaConversions._
import util.Random
import org.richardweiss.nmf._
import org.richardweiss.nmf.util.PimpedMatrix._

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
class SparseEuclidianNMF(val v: AbstractMatrix, val r: Int, val minDistance: Double, val maxIterations: Int) extends NMF {
  private val rand = new Random()
  val n = v.numRows()
  val m = v.numColumns()

  // The following are used in some proofs to check dimensions of the
  // results.
  type N <: Nat
  type M <: Nat
  type R <: Nat

  def distance(vMat: AbstractMatrix, wMat: AbstractMatrix, hMat: AbstractMatrix): Double = {
    var eDistance: Double = 0
    for (e <- v){
      val r = e.row()
      val c = e.column()
      val row = wMat.getRow(r)
      val col = hMat.getCol(c)
      val bVal = row.dot(col)
      eDistance += math.pow(vMat.get(r,c) - bVal, 2)
    }
    eDistance
  }

  private def validMatrixTest(matrix: AbstractMatrix) {
    for (e: MatrixEntry <- matrix) {
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
    val wDenomWHMat = new DenseMatrix(n, m)
    val wDenomWHP = Matrix.matrix[N, M]

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

      tmpW.mult(tmpH, wDenomWHMat)
      if (TESTS) wP.multiplyAndPack(hP, wDenomWHP)
      if (TESTS) validMatrixTest(wDenomWHMat)

      wDenomWHMat.transBmult(tmpH, wDenomMat)
      if (TESTS) wDenomWHP.multiplyAndPack(hP.transpose, wDenomP)
      if (TESTS) validMatrixTest(wDenomMat)

      for (i <- 0 until n)
        for (j <- 0 until r) {
          tmpW.set(i, j, tmpW.get(i, j) * wNomMat.get(i, j) / wDenomMat.get(i, j))
        }
      for (e <- tmpW) if (e.get().isNaN) e.set(0)

      if (TESTS) validMatrixTest(tmpW)

      delta = distance(v, tmpW, tmpH)
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
      _ => new SparseEuclidianNMF(v, r, minDistance, maxIterations)
    }
  } else {
    (0 until numInstances).map {
      _ => new SparseEuclidianNMF(v, r, minDistance, maxIterations)
    }
  }
  private val result: SparseEuclidianNMF = allResults.minBy(x => x.finalDistance)
  val w = result.w
  val h = result.h
  val finalExitReason = result.finalExitReason
  val finalDistance = result.finalDistance
  val worstDistance = allResults.maxBy(x => x.finalDistance).finalDistance
}

