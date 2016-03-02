package scalin

import org.scalatest.{FunSuite, Matchers}

class SyntaxGoal {

  Vec(2, 3)

  Mat.verbatim(3, 3)(
    1, 2, 3,
    4, 5, 6,
    7, 8, 9)

  Mat.zeros[Int](3, 3)
  Vec.zeros[Int](3)

  Mat.ones[Int](3, 3)
  Vec.ones[Int](3)

  Mat.fill(3,3)(3)
  Vec.fill(3)(3)

  //  Vec.range(start, stop, step)

  Mat.eye[Int](3)

  Mat.diag(Vec)

  Vec.tabulate(3)( i => 2 * i )
  Mat.tabulate(3, 2)( (i, j) => i + j )

}
