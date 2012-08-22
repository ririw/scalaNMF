package org.richardweiss.nmf

import dense.EuclidianNMF
import org.scalatest.PropSpec
import org.scalatest.prop.PropertyChecks
import org.scalatest.matchers.ShouldMatchers
import sparse.SparseEuclidianNMF
import util.Random
import no.uib.cipr.matrix.sparse.FlexCompRowMatrix
import java.io.FileWriter

class SparseTest extends PropSpec with PropertyChecks with ShouldMatchers {
  val s = 10
  val r = 3
  property("Works"){
    pending
    val random = new Random()
    val v = new FlexCompRowMatrix(s,s)
    for (i <- 0 until s)
      for (j <- 0 until s){
        if (random.nextInt() % 20 == 0){
          val vij = math.pow(random.nextDouble()*10, 3)
          v.set(i, j, vij)
        }
      }
    val vFile = new FileWriter("./sv.dat")
    val wFile = new FileWriter("./sw.dat")
    val hFile = new FileWriter("./sh.dat")
    val startTime = java.util.Calendar.getInstance().getTimeInMillis
    val eNMF = new SparseEuclidianNMF(v, r, 20, 1000)
    val endTime = java.util.Calendar.getInstance().getTimeInMillis
    println("Sparse factorization took %d ms".format(endTime-startTime))
    vFile.write(v.toString.dropWhile(c => c != '\n'))
    wFile.write(eNMF.w.toString.dropWhile(c => c != '\n'))
    hFile.write(eNMF.h.toString.dropWhile(c => c != '\n'))
    vFile.close()
    wFile.close()
    hFile.close()
  }
}
