package org.richardweiss.nmf

import dense.{MultiEuclidianNMF, EuclidianNMF}
import org.scalatest.PropSpec
import org.scalatest.prop.PropertyChecks
import org.scalatest.matchers.ShouldMatchers
import no.uib.cipr.matrix.DenseMatrix
import scala.util.Random
import collection.JavaConversions._
import java.io.{FileWriter, File}
import no.uib.cipr.matrix.sparse.FlexCompRowMatrix
import scala.util.Random

class DenseTest extends PropSpec with PropertyChecks with ShouldMatchers {
  val s = 100
  val r = 5

  property("NNMF test"){
    val random = new Random()
    val v = new DenseMatrix(s,s)
    for (i <- 0 until s)
      for (j <- 0 until s){
        val vij = if (random.nextInt() % s/5 != 0) math.pow(random.nextDouble()*10, 3) else 0
        v.set(i, j, vij)
      }
    /*
    val v = new FlexCompRowMatrix(s,s)
    for (i <- 0 until s)
      for (j <- 0 until s){
        if (random.nextInt() % (s/2) == 0){
          val vij = math.pow(random.nextDouble()*10, 3)
          v.set(i, j, vij)
        }
      }
      */
    val vFile = new FileWriter("./v.dat")
    val wFile = new FileWriter("./w.dat")
    val hFile = new FileWriter("./h.dat")
    val startTime = java.util.Calendar.getInstance().getTimeInMillis
    val eNMF = EuclidianNMF(v, r, 20, 1000)
    val endTime = java.util.Calendar.getInstance().getTimeInMillis
    println("Dense factorization took %d ms".format(endTime-startTime))
    println(eNMF.finalDistance)
    println(eNMF.finalExitReason)
    vFile.write(v.toString.dropWhile(c => c != '\n'))
    wFile.write(eNMF.w.toString.dropWhile(c => c != '\n'))
    hFile.write(eNMF.h.toString.dropWhile(c => c != '\n'))
    vFile.close()
    wFile.close()
    hFile.close()
  }
  property("See what happens with sparsity"){
    pending
    val random = new Random()
    val v = new FlexCompRowMatrix(s,s)
    for (i <- 0 until s)
      for (j <- 0 until s){
        if (random.nextInt() % (s/5) == 0){
          val vij = math.pow(random.nextDouble()*10, 3)
          v.set(i, j, vij)
        }
      }

    val startTime = java.util.Calendar.getInstance().getTimeInMillis
    val eNMF = EuclidianNMF(v, r, 20, 1000)
    val endTime = java.util.Calendar.getInstance().getTimeInMillis
    println("Sparse factorization took %d ms".format(endTime-startTime))
    println(eNMF.finalDistance)
  }
}
