package org.richardweiss.nmf.sparse

import no.uib.cipr.matrix.{VectorEntry, DenseMatrix, MatrixEntry, AbstractMatrix}
import metascala.Nats.Nat
import scala.collection.JavaConversions._
import util.Random
import org.richardweiss.nmf.{SparseMatrixIterator, Matrix, NMF_ExitReason, NMF}
import no.uib.cipr.matrix.sparse.{FlexCompColMatrix, FlexCompRowMatrix}
import collection.GenSeq
import java.io.InvalidClassException

class SparseEuclidianNMF(val v: AbstractMatrix, val r: Int, val minDelta: Double, val maxIterations: Int) extends NMF {
  private val rand = new Random()
  private val n = v.numRows()
  private val m = v.numColumns()

  // The following are used in some proofs to check dimensions of the
  // results.
  type N <: Nat
  type M <: Nat
  type R <: Nat

  def distance(a: AbstractMatrix, b: AbstractMatrix): Double = {
    var eDistance: Double = 0
    for (e <- SparseMatrixIterator(v)){
      val r = e.row()
      val c = e.column()
      println(r,c)
      eDistance += math.pow(a.get(r, c) - b.get(r, c), 2)
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
  }

  private def run(): (
    AbstractMatrix,
      AbstractMatrix,
    NMF_ExitReason.EuclidianNMF_ExitReason,
    Double)
  = {
    val tmpW = new DenseMatrix(n, r)
    val tmpH = new DenseMatrix(r, m)
    for (e <- tmpW) {
      e.set(rand.nextDouble())
    }
    for (e <- tmpH) {
      e.set(rand.nextDouble())
    }
    val newMatrix: DenseMatrix =
      new DenseMatrix(v.numRows(), v.numColumns()).zero().asInstanceOf[DenseMatrix]
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

    do {
      previousDistance = delta

      tmpW.transAmult(v, hNomMat)
      wP.transpose.multiplyAndPack(vP, hNomP)

      tmpW.transAmult(tmpW, hDenomWTW)
      wP.transpose.multiplyAndPack(wP, hDenomWTWP)

      hDenomWTW.mult(tmpH, hDenomMat)
      hDenomWTWP.multiplyAndPack(hP, hDenomP)

      for (i <- 0 until r)
        for (j <- 0 until m) {
          tmpH.set(i, j, tmpH.get(i, j) * hNomMat.get(i, j) / hDenomMat.get(i, j))
        }

      v.transBmult(tmpH, wNomMat)
      vP.multiplyAndPack(hP.transpose, wNomP)

      tmpW.mult(tmpH, wDenomWHMat)
      wP.multiplyAndPack(hP, wDenomWHP)

      wDenomWHMat.transBmult(tmpH, wDenomMat)
      wDenomWHP.multiplyAndPack(hP.transpose, wDenomP)

      for (i <- 0 until n)
        for (j <- 0 until r) {
          tmpW.set(i, j, tmpW.get(i, j) * wNomMat.get(i, j) / wDenomMat.get(i, j))
        }

      tmpW.mult(tmpH, newMatrix)
      delta = distance(v, newMatrix)
      iterations += 1
      //println(delta)
    } while (iterations < maxIterations && delta > minDelta && previousDistance != delta)
    val finalDistance = delta
    val exitReason = if (iterations >= maxIterations) {
      NMF_ExitReason.MaxIterations
    } else if (delta <= minDelta) {
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

class MultiEuclidianNMF(
                         val v: AbstractMatrix,
                         val r: Int,
                         val minDelta: Double,
                         val maxIterations: Int,
                         val numInstances: Int,
                         val parallel: Boolean) extends NMF {
  def this(v: AbstractMatrix, r: Int, minDelta: Double, maxIterations: Int) = this(v, r, minDelta, maxIterations, 1, false)

  def this(v: AbstractMatrix, r: Int, minDelta: Double, maxIterations: Int, numInstances: Int) =
    this(v, r, minDelta, maxIterations, numInstances, true)

  private val allResults = if (parallel) {
    (0 until numInstances).par.map {
      _ => new SparseEuclidianNMF(v, r, minDelta, maxIterations)
    }
  } else {
    (0 until numInstances).map {
      _ => new SparseEuclidianNMF(v, r, minDelta, maxIterations)
    }
  }
  private val result: SparseEuclidianNMF = allResults.minBy(x => x.finalDistance)
  val w = result.w
  val h = result.h
  val finalExitReason = result.finalExitReason
  val finalDistance = result.finalDistance
  val worstDistance = allResults.maxBy(x => x.finalDistance).finalDistance
}
