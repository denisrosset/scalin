package scalin

import spire.syntax.cfor._

object Util {

  def maskToIndices(mask: AbstractVec[Boolean]): Seq[Int] = {
    val mk = scala.collection.mutable.BitSet.empty
    cforRange(0 until mask.length) { k =>
      if (mask(k)) mk += k
    }
    mk.toSeq
  }

}
