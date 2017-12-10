package fpinscala.monoids

import org.scalatest.{FlatSpec, Matchers}

class MonoidSpec extends FlatSpec with Matchers {

  "foldMapV" should "fold an IndexedSeq" in {
    val seq = IndexedSeq(2, 4, 7, 3)
    val fold = Monoid.foldMapV(seq, new Monoid[String]{
      override def op(s1: String, s2: String): String = s1 + s2
      override def zero: String = ""
    })(_.toString)
    fold shouldBe "2473"
  }

}
