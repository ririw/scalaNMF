package org.richardweiss.nmf

import no.uib.cipr.matrix.AbstractMatrix
import com.nicta.scoobi.DList
import com.nicta.scoobi.lib.DMatrix

trait NMF {
  val w: AbstractMatrix
  val h: AbstractMatrix
  val finalExitReason: NMF_ExitReason.EuclidianNMF_ExitReason
  val finalDistance: Double
}
trait DistNMF {
  val w: DMatrix[Long, Double]
  val h: DMatrix[Long, Double]
  val finalExitReason: NMF_ExitReason.EuclidianNMF_ExitReason
  val finalDistance: Double
}

object NMF_ExitReason extends Enumeration {
  type EuclidianNMF_ExitReason = Value
  val MaxIterations = Value
  val MinimizedDistance = Value
  val StableDistance = Value
}