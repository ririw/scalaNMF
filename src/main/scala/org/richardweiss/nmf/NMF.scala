package org.richardweiss.nmf

import no.uib.cipr.matrix.AbstractMatrix

trait NMF {
  var w: AbstractMatrix
  var h: AbstractMatrix
  var finalExitReason: NMF_ExitReason.EuclidianNMF_ExitReason
  var finalDistance: Double
}

object NMF_ExitReason extends Enumeration {
  type EuclidianNMF_ExitReason = Value
  val MaxIterations = Value
  val MinimizedDistance = Value
  val StableDistance = Value
}