package org.richardweiss.nmf.dist

import com.nicta.scoobi.DList
import metascala.Nats.Nat
import com.nicta.scoobi.lib.DMatrix

class DistributedDenseMatrix[Rows <: Nat, Cols <: Nat](val rows: Long, val cols: Long, val items: DList[((Long, Long), Double)]){
  def add(b: DistributedDenseMatrix[Rows, Cols]): DistributedDenseMatrix[Rows, Cols] = {
    new DistributedDenseMatrix[Rows, Cols](
      rows,
      cols,
      (items ++ b.items).groupByKey.combine((a: Double, b: Double) => a + b))
  }
  def transpose: DistributedDenseMatrix[Cols, Rows] = {
    new DistributedDenseMatrix[Cols, Rows](
      cols,
      rows,
      items.map(v => (v._1._2, v._1._1) -> v._2)
    )
  }
  def multiply[B <: Nat](b: DistributedDenseMatrix[Cols, B]): DistributedDenseMatrix[Rows, B] = {
    null
  }
}
