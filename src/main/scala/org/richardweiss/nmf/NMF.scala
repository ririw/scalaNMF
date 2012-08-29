package org.richardweiss.nmf

import no.uib.cipr.matrix.AbstractMatrix

trait NMF {
  val w: AbstractMatrix
  val h: AbstractMatrix
  val finalExitReason: NMF_ExitReason.EuclidianNMF_ExitReason
  val finalDistance: Double
}

object NMF_ExitReason extends Enumeration {
  type EuclidianNMF_ExitReason = Value
  val MaxIterations = Value
  val MinimizedDistance = Value
  val StableDistance = Value
}