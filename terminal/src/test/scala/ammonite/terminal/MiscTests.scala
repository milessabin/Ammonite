package ammonite.terminal

import utest._

import scala.collection.{immutable => imm}

object HeightTests extends TestSuite{
  val tests = TestSuite{

    'a - {
      val height = TermCore.calculateHeight(
        "abcde".toVector,
        cursor = 0,
        width = 2,
        prompt = ""
      )
      assert(height == (3, 0, 0))
      //ab
      //cd
      //e
    }
    'b - {
      val height = TermCore.calculateHeight(
        "abcd".toVector,
        cursor = 4,
        width = 2,
        prompt = ""
      )
      assert(height == (3, 2, 0))
      //ab
      //cd
      //|
    }
    'c - {
      val height = TermCore.calculateHeight(
        "abcd".toVector,
        cursor = 0,
        width = 2,
        prompt = ""
      )
      assert(height == (2, 0, 0))
      //|b
      //cd
      //
    }

    'd - {
      val height = TermCore.calculateHeight(
        "ab\ncd".toVector,
        cursor = 0,
        width = 2,
        prompt = ""
      )
      assert(height == (2, 0, 0))
      //|b
      //cd
      //
    }

    'e - {
      val height = TermCore.calculateHeight(
        "ab\ncd".toVector,
        cursor = 5,
        width = 2,
        prompt = ""
      )
      assert(height == (3, 2, 0))
      //ab
      //cd
      //|
    }
    'f - {
      val height = TermCore.calculateHeight(
        "ab\ncd".toVector,
        cursor = 2,
        width = 2,
        prompt = ""
      )
      assert(height == (3, 1, 0))
      //ab
      //|
      //cd
    }
    'g - {
      val height = TermCore.calculateHeight(
        "ab\ncd".toVector,
        cursor = 2,
        width = 2,
        prompt = "@"
      )
      assert(height == (3, 1, 1))
      //@a
      //b|
      //cd
    }
    'h - {
      val height = TermCore.calculateHeight(
        "a\ncd".toVector,
        cursor = 1,
        width = 2,
        prompt = "@"
      )
      assert(height == (3, 1, 0))
      //@a
      //|d
    }
    'i - {
      val height = TermCore.calculateHeight(
        "a\ncd".toVector,
        cursor = 0,
        width = 2,
        prompt = "@"
      )
      assert(height == (2, 0, 1))
      //@|
      //cd
    }
    'j - {
      val height = TermCore.calculateHeight(
        "ab\ncd".toVector,
        cursor = 1,
        width = 2,
        prompt = "@"
      )
      assert(height == (3, 1, 0))
      //@a
      //|
      //cd
    }


  }
}
