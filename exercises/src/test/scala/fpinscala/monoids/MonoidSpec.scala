package fpinscala.monoids

import org.scalatest.{FlatSpec, Matchers}

class MonoidSpec extends FlatSpec with Matchers {

  import Monoid._

  "foldMapV" should "fold an IndexedSeq" in {
    val seq = IndexedSeq(2, 4, 7, 3)
    val fold = foldMapV(seq, new Monoid[String]{
      override def op(s1: String, s2: String): String = s1 + s2
      override def zero: String = ""
    })(_.toString)
    fold shouldBe "2473"
  }

  "ordered" should "detect whether an IndexedSeq is ordered" in {
    val orderedSeq = IndexedSeq(1, 2, 3, 4)
    val unorderedSeq = IndexedSeq(7, 8, 9, 6)
    ordered(orderedSeq) shouldBe true
    ordered(unorderedSeq) shouldBe false
  }

}
