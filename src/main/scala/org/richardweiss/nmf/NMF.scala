package org.richardweiss.nmf

import no.uib.cipr.matrix.sparse.{FlexCompColMatrix, FlexCompRowMatrix}

trait NMF {
  var w: FlexCompRowMatrix
  var h: FlexCompColMatrix
  var finalExitReason: NMF_ExitReason.EuclidianNMF_ExitReason
  var finalDistance: Double
}

object NMF_ExitReason extends Enumeration {
  type EuclidianNMF_ExitReason = Value
  val MaxIterations = Value
  val MinimizedDistance = Value
  val StableDistance = Value
}