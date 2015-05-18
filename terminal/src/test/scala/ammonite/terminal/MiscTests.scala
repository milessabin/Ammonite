package ammonite.terminal

import utest._

import scala.collection.{immutable => imm}

object HeightTests extends TestSuite{
  val tests = TestSuite{

    * - {
      val height = TermCore.calculateHeight(
        "abcde".toVector,
        cursor = 0,
        width = 2,
        promptLength = 0
      )
      assert(height == 3)
      //ab
      //cd
      //e
    }
    * - {
      val height = TermCore.calculateHeight(
        "abcd".toVector,
        cursor = 4,
        width = 2,
        promptLength = 0
      )
      assert(height == 3)
      //ab
      //cd
      //|
    }
    * - {
      val height = TermCore.calculateHeight(
        "abcd".toVector,
        cursor = 0,
        width = 2,
        promptLength = 0
      )
      assert(height == 2)
      //|b
      //cd
      //
    }

    * - {
      val height = TermCore.calculateHeight(
        "ab\ncd".toVector,
        cursor = 0,
        width = 2,
        promptLength = 0
      )
      assert(height == 2)
      //|b
      //cd
      //
    }

    * - {
      val height = TermCore.calculateHeight(
        "ab\ncd".toVector,
        cursor = 5,
        width = 2,
        promptLength = 0
      )
      assert(height == 3)
      //ab
      //cd
      //|
    }
    * - {
      val height = TermCore.calculateHeight(
        "ab\ncd".toVector,
        cursor = 2,
        width = 2,
        promptLength = 0
      )
      assert(height == 3)
      //ab
      //|
      //cd
    }
    * - {
      val height = TermCore.calculateHeight(
        "ab\ncd".toVector,
        cursor = 2,
        width = 2,
        promptLength = 1
      )
      assert(height == 3)
      //@a
      //|
      //cd
    }
    * - {
      val height = TermCore.calculateHeight(
        "a\ncd".toVector,
        cursor = 1,
        width = 2,
        promptLength = 1
      )
      assert(height == 3)
      //@a
      //|
      //cd
    }
    * - {
      val height = TermCore.calculateHeight(
        "a\ncd".toVector,
        cursor = 0,
        width = 2,
        promptLength = 1
      )
      assert(height == 2)
      //@|
      //cd
    }
    * - {
      val height = TermCore.calculateHeight(
        "ab\ncd".toVector,
        cursor = 1,
        width = 2,
        promptLength = 1
      )
      assert(height == 3)
      //@a
      //|
      //cd
    }


  }
}
