package org.richardweiss.nmf

import no.uib.cipr.matrix.{AbstractMatrix, VectorEntry, MatrixEntry}
import no.uib.cipr.matrix.sparse.{FlexCompColMatrix, FlexCompRowMatrix}
import scala.collection.JavaConversions._

abstract class SparseMatrixIterator extends TraversableOnce[MatrixEntry]
class SparseMatrixIterator_RowWise(m: FlexCompRowMatrix) extends SparseMatrixIterator{
  def isTraversableAgain = true
  def toStream = {
    val rows = m.numRows()
    def matrixEnter(theRow: Int)(entry: VectorEntry): MatrixEntry = {
      new MatrixEntry {
        def set(value: Double) {entry.set(value)}
        def get() = entry.get()
        def column() = entry.index()
        def row() = theRow
      }
    }
    def streamBuilder(row: Int): Stream[MatrixEntry] = {
      if (row > rows) Stream()
      else (m.getRow(row).iterator().toStream.map(matrixEnter(row))) ++ (streamBuilder(row + 1))
    }
    streamBuilder(0)
  }
  def toIterator = toStream.iterator
  def foreach[U](f: (MatrixEntry) => U) {toStream.foreach(f)}
  def isEmpty = m.numColumns() != 0 && m.numRows() != 0
  def hasDefiniteSize = true
  def seq = toStream.toSeq
  def forall(p: (MatrixEntry) => Boolean) = toStream.forall(p)
  def exists(p: (MatrixEntry) => Boolean) = toStream.exists(p)
  def find(p: (MatrixEntry) => Boolean) = toStream.find(p)
  def copyToArray[B >: MatrixEntry](xs: Array[B], start: Int, len: Int) {
    toStream.copyToArray(xs, start, len)
  }
  def toTraversable = toStream.toTraversable
}
class SparseMatrixIterator_ColWise(m: FlexCompColMatrix) extends SparseMatrixIterator{
  def isTraversableAgain = true
  def toStream = {
    val cols = m.numColumns()
    def matrixEnter(theCol: Int)(entry: VectorEntry): MatrixEntry = {
      new MatrixEntry {
        def set(value: Double) {entry.set(value)}
        def get() = entry.get()
        def column() = theCol
        def row() = entry.index()
      }
    }
    def streamBuilder(col: Int): Stream[MatrixEntry] = {
      if (col > cols) Stream()
      else (m.getColumn(col).iterator().toStream.map(matrixEnter(col))) ++ (streamBuilder(col + 1))
    }
    streamBuilder(0)
  }
  def toIterator = toStream.iterator
  def foreach[U](f: (MatrixEntry) => U) {toStream.foreach(f)}
  def isEmpty = m.numColumns() != 0 && m.numRows() != 0
  def hasDefiniteSize = true
  def seq = toStream.toSeq
  def forall(p: (MatrixEntry) => Boolean) = toStream.forall(p)
  def exists(p: (MatrixEntry) => Boolean) = toStream.exists(p)
  def find(p: (MatrixEntry) => Boolean) = toStream.find(p)
  def copyToArray[B >: MatrixEntry](xs: Array[B], start: Int, len: Int) {
    toStream.copyToArray(xs, start, len)
  }
  def toTraversable = toStream.toTraversable
}
object SparseMatrixIterator {
  def apply(m: FlexCompRowMatrix): SparseMatrixIterator = {
    new SparseMatrixIterator_RowWise(m)
  }
  def apply(m: FlexCompColMatrix): SparseMatrixIterator = {
    new SparseMatrixIterator_ColWise(m)
  }
  def apply(m: AbstractMatrix): SparseMatrixIterator = {
    m.getClass match {
      case mc: FlexCompColMatrix => new SparseMatrixIterator_ColWise(mc)
      case mr: FlexCompRowMatrix => new SparseMatrixIterator_RowWise(mr)
      case _ => throw new IllegalArgumentException
    }
  }
}
