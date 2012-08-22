package org.richardweiss.nmf

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.prop.PropertyChecks
import org.scalatest.PropSpec
import no.uib.cipr.matrix.sparse.SparseVector
import scala.collection.JavaConversions._

class SparseSandbox extends PropSpec with PropertyChecks with ShouldMatchers {
  property("Test of vector getting"){
    pending
    val x = new SparseVector(10)
    x.set(3, 1)
    x.set(4, 2)
    for (e <- x)
      println(e.index())
  }
}
