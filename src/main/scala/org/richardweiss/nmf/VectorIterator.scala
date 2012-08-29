package org.richardweiss.nmf

import no.uib.cipr.matrix.{DenseVector, DenseMatrix, AbstractVector, AbstractMatrix}

abstract class VectorIterator extends TraversableOnce[DenseVector]{
  def isTraversableAgain = true
  def toStream = toIterator.toStream
  def foreach[U](f: (DenseVector) => U) {
    for (v <- toIterator) f(v)
  }
  def isEmpty = false
  def hasDefiniteSize = true
  def seq = toIterator.toSeq
  def forall(p: (DenseVector) => Boolean) = toIterator.forall(p)
  def exists(p: (DenseVector) => Boolean) = toIterator.exists(p)
  def find(p: (DenseVector) => Boolean) = toIterator.find(p)
  def copyToArray[B >: DenseVector](xs: Array[B], start: Int, len: Int) {
    toIterator.copyToArray(xs, start, len)
  }
  def toTraversable = toIterator.toTraversable
}

class VectorRowIterator(matrix: AbstractMatrix) extends VectorIterator {
  def toIterator = new Iterator[DenseVector] {
    private var c = 0
    def hasNext = c < matrix.numColumns()
    def next() = {
      val newVec = new DenseVector(matrix.numRows())
      for (r <- 0 until matrix.numRows()) newVec.set(r, matrix.get(r, c))
      c += 1
      newVec
    }
  }
}

class VectorColIterator(matrix: AbstractMatrix) extends VectorIterator {
  def toIterator = new Iterator[DenseVector] {
    private var r = 0
    def hasNext = r < matrix.numRows()
    def next() = {
      val newVec = new DenseVector(matrix.numColumns())
      for (c <- 0 until matrix.numColumns()) newVec.set(c, matrix.get(r, c))
      r += 1
      newVec
    }
  }
}

object TraversalType extends Enumeration {
  type TraversalType = Value
  val Row = Value
  val Col = Value
}