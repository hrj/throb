import org.specs2._
import com.lavadip.throb._

object myParsers extends Parsers {
  val key1 = key("keyword1")
  val key2 = key("keyword2")
  val key3 = key("keyword3")
  val key4 = key("keyword4")
  val phrase = key("word1 word2")

  implicit def key2(k:String):Parser[String] = key(k)
}

class ParserSpec extends mutable.Specification {
  import myParsers._

  "key parser" should {
    "successfully parse a matching keyword" >> {
      key1("keyword1").success must beTrue
    }
    "give error for nonmatching keyword" >> {
      key1("xxx").success must beFalse
    }
  }
  
  "| combinator" should {
    "give error for nonmatching keyword" >> {
      val result = (key1|key2)("xxx")
      result.success must beFalse
      (result.status.rem(0).length == 3) must beTrue
    }
    "give error for nonmatching keyword" >> {
      val result = (key1|key2|phrase)("xxx")
      result.success must beFalse
      (result.status.rem(0).length == 3) must beTrue
    }
  }

  "~ combinator" should {
    "accept two keywords" >> {
      (key1 ~ key2) ("keyword1 keyword2").success must beTrue
    }
    "accept three keywords" >> {
      (key1 ~ key2 ~ key1) ("keyword1 keyword2 keyword1").success must beTrue
    }
    "give error for nonmatching keyword" >> {
      val result = (key1~key2)("xxx")
      result.success must beFalse
      (result.status.rem(0).length == 3) must beTrue
    }
    "complete with consideration to weight" >> {
      val c1 = (key1~key2).named("clause1",2)
      val c2 = (key3~key4).named("clause2",2)
      
      val result = (c1~c2)("keyword1")
      (result.completions contains ("keyword2 clause2")) must beTrue
    }
  }

  "? combinator" should {
    "accept the presence of optional part" >> {
      (key1 ~ (key2 ?)) ("keyword1 keyword2").success must beTrue
    }
    "accept the absence of optional part" >> {
      (key1 ~ (key2 ?) ~ key1) ("keyword1 keyword1").success must beTrue
    }
    "complete in the partial presence of keyword 1" >> {
      ((key1 ~ (key2 ?)) ("keyword1 k").completions contains "eyword2") must beTrue
    }
    "complete in the partial presence of keyword 2" >> {
      val completions = ((key1 ~ (key2 ?) ~ key3) ("keyword1 k").completions)
      ((completions contains "eyword2") && (completions contains "eyword3")) must beTrue
    }
    "complete in the presence of optional keyword" >> {
      ((key1 ~ (key2 ?) ~ key3) ("keyword1 keyword2").completions contains "keyword3") must beTrue
    }
    "complete in the presence of optional phrase" >> {
      ((key1 ~ (phrase ?) ~ key3) ("keyword1 word1 word2").completions contains "keyword3") must beTrue
    }
    "complete in the absence of optional keyword" >> {
      ((key1 ~ (key2 ?) ~ key3) ("keyword1 ").completions contains ("[keyword2] keyword3")) must beTrue
    }
    "complete in the absence of optional phrase" >> {
      val completions = (key1 ~ (phrase ?) ~ key3) ("keyword1 ").completions
      (completions contains ("[word1 word2] keyword3")) must beTrue
    }
    "complete in the absence of part of optional phrase 1" >> {
      ((key1 ~ (phrase ?) ~ key3) ("keyword1 word1").completions contains ("word2 keyword3")) must beTrue
    }
    "complete in the absence of part of optional phrase 2" >> {
      ((key1 ~ (phrase ?) ~ key3) ("keyword1 w").completions contains ("ord1 word2 keyword3")) must beTrue
    }
  }

  "combinations of ?, ~" should {
    "complete in the absence of optional keyword" >> {
      val completions = ((key1 ~ (((key2 ?) ~ key3 ~ key4) ) ) ("keyword1 keyword3").completions)
      (completions contains ("keyword4")) must beTrue
    }
    "complete in the absence of optional phrase" >> {
      val completions = ((key1 ~ (((phrase ?) ~ key3 ~ key4) ) ) ("keyword1 keyword3").completions)
      (completions contains ("keyword4")) must beTrue
    }
    "complete in the partial absence of optional phrase" >> {
      val completions = ((key1 ~ (((phrase ?) ~ key3 ~ key4) ) ) ("keyword1 word1").completions)
      (completions contains ("word2 keyword3 keyword4")) must beTrue
    }
  }

  "* parser" should {
    "parse correctly once" >> {
      val result = (key1 *) ("keyword1")
      (result.result == List("keyword1")) must beTrue
      result.success must beTrue
      (result.completions contains ("keyword1")) must beTrue
    }
    "parse correctly twice" >> {
      val result = (key1 *) ("keyword1 keyword1")
      (result.result == List("keyword1", "keyword1")) must beTrue
      result.success must beTrue
      (result.completions contains ("keyword1")) must beTrue
    }

    "parse a phrase correctly once" >> {
      val result = (phrase *) ("word1 word2")
      (result.result == List("word1 word2")) must beTrue
      result.success must beTrue
      (result.completions contains ("word1 word2")) must beTrue
    }
    "parse a phrase correctly twice" >> {
      val result = (phrase *) ("word1 word2 word1 word2")
      (result.result == List("word1 word2", "word1 word2")) must beTrue
      result.success must beTrue
      (result.completions contains ("word1 word2")) must beTrue
    }
    "parse a incomplete phrase correctly" >> {
      val result = (phrase *) ("word1 word2 xyz")
      (result.result == List("word1 word2")) must beTrue
      (result.status.rem == List("xyz")) must beTrue
      result.success must beTrue
      (result.completions contains ("word1 word2")) must beTrue
    }
  }
  "interleaved parser" should {
    "parse correctly " >> {
      val parser = (key1 * ("xy" ^^ {case sep => (s:String,t:String) => s + " z " + t}))
      val result = parser("keyword1")
      (result.result == "keyword1") must beTrue
      result.success must beTrue
      (result.completions contains ("xy keyword1")) must beTrue

      val result2 = parser("keyword1 xy")
      result2.success must beTrue
      result2.best.isDefined must beTrue
      if (result2.best.isDefined) {
        (result2.best.get.parsedChars == 12) must beTrue
      }
      (result2.completions contains ("keyword1")) must beTrue
    }
    "parse correctly with an optional separator" >> {
      val parser = (key1 * (("xy" ?) ^^ {case sep => (s:String,t:String) => s + " z " + t}))

      {
        val result = parser("keyword1")
        (result.result == "keyword1") must beTrue
        result.success must beTrue
        (result.completions contains ("[xy] keyword1")) must beTrue
      }

      {
        val result2 = parser("keyword1 xy")
        result2.success must beTrue
        result2.best.isDefined must beTrue
        if (result2.best.isDefined) {
          (result2.best.get.parsedChars == 12) must beTrue
        }
        (result2.completions contains ("keyword1")) must beTrue
      }

      {
        val result3 = parser("keyword1 xy keyword1")
        result3.success must beTrue
        result3.best.isDefined must beFalse
        (result3.completions contains ("[xy] keyword1")) must beTrue
      }
    }

    "parse a phrase correctly with an optional separator" >> {
      val parser = (phrase * (("xy" ?) ^^ {case sep => (s:String,t:String) => s + " z " + t}))

      {
        val result = parser("word1 word2")
        (result.result == "word1 word2") must beTrue
        result.success must beTrue
        (result.completions contains ("[xy] word1 word2")) must beTrue
      }

      {
        val result = parser("word1 word2 word1")
        (result.result == "word1 word2") must beTrue
        result.success must beTrue
        result.best.isDefined must beTrue
        if (result.best.isDefined) {
          (result.best.get.parsedChars == 18) must beTrue
        }
        (result.completions contains ("word2")) must beTrue
      }


      {
        val result = parser("word1 word2 xy")
        result.success must beTrue
        (result.result == "word1 word2") must beTrue
        result.best.isDefined must beTrue
        if (result.best.isDefined) {
          (result.best.get.parsedChars == 15) must beTrue
        }
        (result.completions contains ("word1 word2")) must beTrue
      }

      {
        val result = parser("word1 word2 xy word1 word2")
        (result.result == "word1 word2 z word1 word2") must beTrue
        result.success must beTrue
        result.best.isDefined must beFalse
        (result.completions contains ("[xy] word1 word2")) must beTrue
      }
    }

    "parse a phrase correctly with an optional alternatives separator" >> {
      val parser =  (phrase * ((alternativesParser( List(myParsers.key("xy"),myParsers.key("pq")) ) ?) ^^ {case sep => (s:String,t:String) => s + " z " + t}))

      {
        val result = parser("word1 word2")
        (result.result == "word1 word2") must beTrue
        result.success must beTrue
        (result.completions contains ("[xy/pq] word1 word2")) must beTrue
      }

      {
        val result = parser("word1 word2 word1")
        (result.result == "word1 word2") must beTrue
        result.success must beTrue
        result.best.isDefined must beTrue
        (result.best.get.rem.foldLeft(0)(_ + _.length) == 0) must beTrue
        if (result.best.isDefined) {
          (result.best.get.parsedChars == 18) must beTrue
        }
        (result.completions contains ("word2")) must beTrue
      }


      {
        val result = parser("word1 word2 xy")
        result.success must beTrue
        result.best.isDefined must beTrue
        (result.best.get.rem.foldLeft(0)(_ + _.length) == 0) must beTrue
        if (result.best.isDefined) {
          (result.best.get.parsedChars == 15) must beTrue
        }
        (result.completions contains ("word1 word2")) must beTrue
      }

      {
        val result = parser("word1 word2 xy word1 word2")
        result.success must beTrue
        result.best.isDefined must beFalse
        (result.completions contains ("[xy/pq] word1 word2")) must beTrue
      }
    }
  }

  "one-of-each parser" should {
    "parse and complete correctly " >> {
      val parser = oneOfEachParser( List((key1, 1), (key2, 1) ))

      val result = parser("keyword1")
      (result.result == List("keyword1")) must beTrue
      result.success must beTrue
      (result.completions contains ("keyword1")) must beFalse
      (result.completions contains ("keyword2")) must beTrue

      val result2 = parser("keyword2 key")
      result2.success must beTrue
      result2.best.isDefined must beTrue
      if (result2.best.isDefined) {
        (result2.best.get.parsedChars == 12) must beTrue
      }
      (result2.completions contains ("word1")) must beTrue
      (result2.completions contains ("word2")) must beFalse
    }
  }

  "end of input parser" should {
    "succeed with empty input for alwaysSuccess parser" >> {
      val parser = eoiParser(emptyParser)

      val result = parser("")
      result.success must beTrue
    }
    "fail with empty input for any other parser " >> {
      val parser = eoiParser(key1)

      val result = parser("")
      result.success must beFalse
    }
    "succeed for end of input " >> {
      val parser = eoiParser(key1)

      val result = parser("keyword1")
      result.success must beTrue
    }
    "fail for trailing words " >> {
      val parser = eoiParser(key1)

      val result = parser("keyword1 xyz")
      result.success must beFalse
      (result.completions isEmpty) must beTrue

      val result2 = parser("key")
      result2.success must beFalse
      (result2.completions contains ("word1")) must beTrue
    }
    "succeed for empty input for ? parser " >> {
      val parser = eoiParser(key1 ?)

      val result = parser("")
      result.success must beTrue
    }
  }
}
