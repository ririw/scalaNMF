package org.richardweiss.nmf.util

import no.uib.cipr.matrix.{DenseMatrix, AbstractVector, DenseVector, AbstractMatrix}
import no.uib.cipr.matrix.sparse.SparseVector

class PimpedMatrix(matrix: AbstractMatrix) {
  def getRow(r: Int): AbstractVector = {
    val newVec = matrix match {
      case e:DenseMatrix => new DenseVector(matrix.numColumns()).zero()
      case _ => new SparseVector(matrix.numColumns())
    }
    for (c <- 0 until matrix.numColumns()) {
      val item = matrix.get(r, c)
      if (item != 0) newVec.set(c, item)
    }
    newVec
  }
  def getCol(c: Int): AbstractVector = {
    val newVec = matrix match {
      case e:DenseMatrix => new DenseVector(matrix.numRows()).zero()
      case _ => new SparseVector(matrix.numRows())
    }
    for (r <- 0 until matrix.numRows()) {
      val item = matrix.get(r, c)
      if (item != 0) newVec.set(r, item)
    }
    newVec
  }
}

object PimpedMatrix {
  implicit def matToPimpedMat[A <: AbstractMatrix](m: A): PimpedMatrix = new PimpedMatrix(m)
}