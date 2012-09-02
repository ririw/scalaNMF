package org.richardweiss.nmf.dist

import spark.{RDD, SparkContext}
import SparkContext._
import no.uib.cipr.matrix.AbstractMatrix
import scala.collection.JavaConversions._
import no.uib.cipr.matrix.sparse.{FlexCompColMatrix, FlexCompRowMatrix, SparseVector}
import util.Random
import DistVectorMatrix.{Row, Col}

abstract class DistVectorMatrix[Rows, Cols]
(val matrix: RDD[(Int, SparseVector)], val rows: Row, val cols: Col) extends Serializable {
  val dimensions = (rows, cols)
  def transpose: DistVectorMatrix[Cols, Rows]
  def switch: DistVectorMatrix[Rows, Cols]
  def add(b: DistVectorMatrix[Rows, Cols]): DistVectorMatrix[Rows, Cols]
  def multiply[N](b: DistVectorMatrix[Cols, N]):
    DistVectorMatrix[Rows, N]
  def scale(d: Double): DistVectorMatrix[Rows,Cols]
  def hadamard(b: DistVectorMatrix[Rows, Cols]): DistVectorMatrix[Rows, Cols]
  def inverseHadamard(b: DistVectorMatrix[Rows, Cols]): DistVectorMatrix[Rows, Cols]
  /** Convert to a sparse matrix */
  def toLocalMatrix: AbstractMatrix
  def map(f: Double => Double): DistVectorMatrix[Rows, Cols]
  /**Print matrix - in a style matlab/octave can understand. The value at n, m may appear twice.
   * spconvert is used to convert the output to a matlab/octave matrix. To quote matlab's
   * documentation: "A row of the form [m n 0] or [m n 0 0] anywhere in D can be used to specify size(S)"
   */
  override def toString = {
    val m = toLocalMatrix
    m.toString.dropWhile(c => c != '\n').tail + ("%10d %10d % .12e\n".format(rows, cols, 0D))
  }
}


class DistRowVectorMatrix[Rows, Cols]
  (matrix: RDD[(Row, SparseVector)], rows: Row, cols: Col) extends
  DistVectorMatrix[Rows, Cols](matrix, rows, cols) with Serializable{

  assert(matrix.count() <= cols)
  if (matrix.count() != 0) assert(matrix.map {v => v._2.size() == rows}.reduce(_ && _))
  def transpose = new DistColVectorMatrix[Cols, Rows](matrix.map {x => x._1 -> x._2.copy()}, cols, rows)
  def switch: DistColVectorMatrix[Rows, Cols] = {
    val newInner = matrix.flatMap{case (row, vec) =>
      var items: List[(Int, (Int, Double))] = List()
      for (x <- vec) items ::= (x.index().asInstanceOf[Col] -> (row -> x.get()))
      items
    }.groupByKey().map {case (col, rowSet) =>
      val newVec = new SparseVector(cols)
      rowSet.foreach {case (row, value) => newVec.set(row, value)}
      col -> newVec
    }
    new DistColVectorMatrix[Rows, Cols](newInner, rows, cols)
  }
  def add(b: DistVectorMatrix[Rows, Cols]): DistRowVectorMatrix[Rows, Cols] =
    if (b.getClass == this.getClass){
      val m = (matrix ++ b.matrix).groupByKey().map
      {x => x._1 -> x._2.reduce{(a: SparseVector, b: SparseVector) =>
        val newVec = a.copy()
        newVec.add(b)
        newVec
      }}
      new DistRowVectorMatrix[Rows, Cols](m, rows, cols)
    } else {
      this.add(b.switch)
    }
  def multiply[N](b: DistVectorMatrix[Cols, N]): DistRowVectorMatrix[Rows, N] =
    DistVectorMatrix.multiplyR(this, b)
  def hadamard(b: DistVectorMatrix[Rows, Cols]): DistRowVectorMatrix[Rows, Cols] = {
    assert(this.rows == b.rows)
    assert(this.cols == b.cols)

    if (b.getClass == this.getClass){
      val m = (matrix ++ b.matrix).groupByKey().map
      {x => x._1 -> x._2.reduce{(a: SparseVector, b: SparseVector) =>
        val newVec = a.copy()
        for (e <- newVec) e.set(e.get()*(b.get(e.index())))
        newVec
        }
      }

      new DistRowVectorMatrix[Rows, Cols](m, rows, cols)
    } else {
      this.hadamard(b.switch)
    }
  }
  def inverseHadamard(b: DistVectorMatrix[Rows, Cols]): DistRowVectorMatrix[Rows, Cols] = {
    if (b.getClass == this.getClass){
      val m = (matrix ++ b.matrix).groupByKey().map
      {x => x._1 -> x._2.reduce{(a: SparseVector, b: SparseVector) =>
        val newVec = a.copy()
        for (e <- newVec) e.set(e.get()/(b.get(e.index())))
        newVec
        }
      }
      new DistRowVectorMatrix[Rows, Cols](m, rows, cols)
    } else {
      this.hadamard(b.switch)
    }
  }
  /**Convert to a sparse matrix */
  def toLocalMatrix = {
    val newMat = new FlexCompColMatrix(rows, cols)
    matrix.collect().map{case (i, r) => newMat.setColumn(i, r)}
    newMat
  }

  def scale(d: Double): DistRowVectorMatrix[Rows, Cols] = new DistRowVectorMatrix[Rows, Cols](matrix.map(x => x._1 -> x._2.scale(d)), rows, cols)
  def map(f: Double => Double): DistRowVectorMatrix[Rows, Cols] =
    new DistRowVectorMatrix(matrix.map {case (i, vec) =>
      val newVec = vec.copy()
      for (e <- newVec)
        e.set(f(e.get))
      i -> newVec
    }, rows, cols)
}


class DistColVectorMatrix[Rows, Cols]
  (matrix: RDD[(Col, SparseVector)], rows: Row, cols: Col) extends
  DistVectorMatrix[Rows, Cols](matrix: RDD[(Int, SparseVector)], rows, cols) with Serializable {
  assert(matrix.count() <= rows)
  if (matrix.count() != 0) assert(matrix.map {v => v._2.size() == cols}.reduce(_ && _))

  def transpose = new DistRowVectorMatrix[Cols, Rows](matrix.map {x => x._1 -> x._2.copy()}, cols, rows)
  def switch: DistRowVectorMatrix[Rows, Cols] = {
    val newInner = matrix.flatMap{case (col, vec) =>
      var items: List[(Int, (Int, Double))] = List()
      for (x <- vec) items ::= (x.index().asInstanceOf[Row] -> (col -> x.get()))
      items
    }.groupByKey().map {case (row, colSet) =>
      val newVec = new SparseVector(rows)
      colSet.foreach {case (col, value) => newVec.set(col, value)}
      row -> newVec
    }
    new DistRowVectorMatrix[Rows, Cols](newInner, rows, cols)
  }
  def add(b: DistVectorMatrix[Rows, Cols]): DistColVectorMatrix[Rows, Cols] =
  if (b.getClass == this.getClass){
    val m = (matrix ++ b.matrix).groupByKey().map{x => x._1 -> x._2.reduce {(a: SparseVector, b: SparseVector) =>
      val newVec = a.copy()
      newVec.add(b)
      newVec
      }
    }
    new DistColVectorMatrix[Rows, Cols](m, rows, cols)
  } else {
    this.add(b.switch)
  }
  def multiply[N](b: DistVectorMatrix[Cols, N]): DistColVectorMatrix[Rows, N] = DistVectorMatrix.multiplyC(this, b)
  def hadamard(b: DistVectorMatrix[Rows, Cols]): DistColVectorMatrix[Rows,Cols] =
    if (b.getClass == this.getClass){
      val m = (matrix ++ b.matrix).groupByKey().map
      {x => x._1 -> x._2.reduce{(a: SparseVector, b: SparseVector) =>
        val newVec = a.copy()
        for (e <- newVec) e.set(e.get()*(b.get(e.index())))
        newVec
        }
      }
      new DistColVectorMatrix[Rows, Cols](m, rows, cols)
    } else {
      this.hadamard(b.switch)
    }
  def inverseHadamard(b: DistVectorMatrix[Rows, Cols]): DistColVectorMatrix[Rows,Cols] =
    if (b.getClass == this.getClass){
      val m = (matrix ++ b.matrix).groupByKey().map
      {x => x._1 -> x._2.reduce{(a: SparseVector, b: SparseVector) =>
        val newVec = a.copy()
        for (e <- newVec) e.set(e.get()/(b.get(e.index())))
        newVec
      }
      }
      new DistColVectorMatrix[Rows, Cols](m, rows, cols)
    } else {
      this.hadamard(b.switch)
    }

  /**Convert to a sparse matrix */
  def toLocalMatrix = {
    val newMat = new FlexCompRowMatrix(rows, cols)
    matrix.collect().map{case (i, r) => newMat.setRow(i, r)}
    newMat
  }

  def scale(d: Double): DistColVectorMatrix[Rows,Cols] = new DistColVectorMatrix[Rows, Cols](matrix.map(x => x._1 -> x._2.scale(d)), rows, cols)
  def map(f: Double => Double): DistColVectorMatrix[Rows, Cols] =
    new DistColVectorMatrix(matrix.map {case (i, vec) =>
      val newVec = vec.copy()
      for (e <- newVec)
        e.set(f(e.get))
      i -> newVec
    }, rows, cols)
}

object DistVectorMatrix{
  type Row = Int
  type Col = Int
  def multiplyR[N, M, P]
    (ap: DistVectorMatrix[N, P], bp: DistVectorMatrix[P,M]):
    DistRowVectorMatrix[N, M] = {
    val a = if (ap.isInstanceOf[DistRowVectorMatrix[N, P]]) ap.switch else ap
    val b = if (bp.isInstanceOf[DistRowVectorMatrix[P, M]]) bp else bp.switch
    assert(a.cols == b.rows)
    assert(a.isInstanceOf[DistColVectorMatrix[N, P]])
    assert(b.isInstanceOf[DistRowVectorMatrix[P, N]])

    val values = a.matrix.cartesian(b.matrix).map{case ((i, veci), (j, vecj)) =>
      (i.asInstanceOf[Row], j.asInstanceOf[Col]) -> veci.dot(vecj)
    }
    val r = values.groupBy(_._1._2).map{v =>
      val newVec = new SparseVector(a.rows)
      v._2.foreach(x => newVec.set(x._1._1, x._2))
      v._1 -> newVec
    }
    new DistRowVectorMatrix[N, M](r, a.rows, b.cols)
  }
  def multiplyC[N, M, P]
  (ap: DistVectorMatrix[N, P], bp: DistVectorMatrix[P,M]):
  DistColVectorMatrix[N, M] = {
    val a = if (ap.isInstanceOf[DistRowVectorMatrix[N, P]]) ap.switch else ap
    val b = if (bp.isInstanceOf[DistRowVectorMatrix[P, M]]) bp else bp.switch
    val values = a.matrix.cartesian(b.matrix).map{case ((i, veci), (j, vecj)) =>
      (i.asInstanceOf[Row], j.asInstanceOf[Col]) -> veci.dot(vecj)
    }
    val r = values.groupBy(_._1._1).map{v =>
      val newVec = new SparseVector(b.cols)
      v._2.foreach(x => newVec.set(x._1._2, x._2))
      v._1 -> newVec
    }
    new DistColVectorMatrix[N, M](r, a.rows, b.cols)
  }
  def randomMatrix[N, M](rows: Int, cols: Int, spark: SparkContext): DistVectorMatrix[N, M] = {
    val rand = new Random()
    val rowMat = rand.nextBoolean()
    val n = if (rowMat) rows else cols
    val m = if (rowMat) cols else rows
    val inner = spark.parallelize((0 until m).map{k =>
      val mat = new SparseVector(n)
      val rand = new Random()
      for (i <- 0 until n) mat.set(i, rand.nextDouble())
      //for (i <- 0 until m) mat.set(i, k*m + i)
      k -> mat
    })
    if (rowMat) new DistRowVectorMatrix[N, M](inner, rows, cols) else new DistColVectorMatrix[N, M](inner, rows, cols)
  }
  def randomRowMatrix[N, M](rows: Int, cols: Int, spark: SparkContext): DistRowVectorMatrix[N, M] = {
    val n = rows
    val m = cols
    val inner = spark.parallelize((0 until m).map{k =>
      val mat = new SparseVector(n)
      val rand = new Random()
      for (i <- 0 until n) mat.set(i, rand.nextDouble())
      //for (i <- 0 until m) mat.set(i, k*m + i)
      k -> mat
    })
    new DistRowVectorMatrix[N, M](inner, rows, cols)
  }
  def randomColMatrix[N, M](rows: Int, cols: Int, spark: SparkContext): DistColVectorMatrix[N, M] = {
    val rowMat = false
    val n = cols
    val m = rows
    val inner = spark.parallelize((0 until m).map{k =>
      val rand = new Random()
      if (rand.nextBoolean()){
        val mat = new SparseVector(n)
        for (i <- 0 until n) mat.set(i, rand.nextDouble())
        //for (i <- 0 until m) mat.set(i, k*m + i)
        Some(k -> mat)
      } else {
        None
      }
    }.flatten)
    new DistColVectorMatrix[N, M](inner, rows, cols)
  }

}