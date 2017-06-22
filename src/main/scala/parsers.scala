package com.lavadip.throb

trait Parsers {

  type Input = List[String]

  var gatheredKeywords : Set[String] = Set.empty

  case class ParseStatus(parsedChars:Int, rem:Input)

  case class ParseResult[+OutT](
    success:Boolean,
    status:ParseStatus,
    best:Option[ParseStatus],
    completions:Set[String],
    result:OutT) {

    override def toString = {/*{{{*/
        var ret = "Result\n------\n"
        ret += "success:" + success + "\n"
        ret += "status:" + status + "\n"
        ret += "best:" + best + "\n"
        ret += "completions:" + completions + "\n"
/*
        val indent = (1 to status.parsedChars + "Parsing: ".length).foldLeft("")((x,y) => x+" ")
        if (success) {
            ret += ("\tSuccessfully parsed " + status.parsedChars + " chars.")
            ret += ("\n\tResult:" + result )
            if (status.rem.length > 0)
                ret += ("\n\tRemaining unparsed input: " + status.rem.reduceLeft(_ + " " + _))
            if (completions.size > 0) {
                ret +=  ("\n\tPossible completions:")
                completions.foreach(x => ret += ("\n" + indent + x))
            }
        } else {
            ret += (indent + "^")
            if (status.rem.length > 0)
                ret += ("\n\tCouldn't parse : " + status.rem.reduceLeft(_ + " " + _))
            completions.foreach(x => ret += ("\n" + indent + x))
        }
        if(best.isDefined) {
            ret += ("\n\tBest: " + best.get)
        }
*/
        ret
    }/*}}}*/

  }

  trait Parser[+OutT] extends ((Input,Int,Int) => ParseResult[OutT]) {
    def apply(i:Input,l:Int,depth:Int):ParseResult[OutT]

    def apply(i:List[String]):ParseResult[OutT] = apply(i,0,3)

    def apply(s:String):ParseResult[OutT] = apply(s.split(" ").filter(_.length > 0).toList)

    def ~[T](that: => Parser[T]) = andParser(this, that)
    def ~>[T](that: => Parser[T]) = andParser(this, that) ^^ {case _ and x => x}
    def <~[T](that: => Parser[T]) = andParser(this, that) ^^ {case x and _ => x}

    def |[U >: OutT](that: => Parser[U]) = orParser(this, that)
    def ? = optParser(this)
    def * = zeroOrMoreParser(this)
    def + = andParser(this, zeroOrMoreParser(this)) ^^ {case x and l => x :: l}

    def ^^[T](f: OutT=> T) = funcApplyParser (this, f)
    def *[U >: OutT](sep: Parser[(U,U) => U]) = interleavedParser(this, sep)

    var name:String = "..."
    var weight:Int = 1
    var nameIsSet = false

    def weighing(w:Int) = {weight = w; this}
    def named(n:String) = {name = n; nameIsSet = true; this}
    def named(n:String,w:Int) = {name = n; nameIsSet = true; weight = w; this}
  }


  def interleavedParser[T,U >: T](leaf: =>Parser[T], sep: =>Parser[(U,U) => T]):Parser[U] =  {
    (leaf ~ ((sep ~ leaf)*)) ^^ {case x and xs => xs.foldLeft(x){case (a, f and b) => f(b, a)}}
  }

  case class and[+X,+Y](x:X, y:Y)

  def countMatches(s:String, t:String) = {
    var i = 0;
    while ((i < s.length) && (i < t.length) && (s.charAt(i) == t.charAt(i))) i += 1
    i
  }

  def keyword(k:String,i:Input, l:Int, depth:Int) : ParseResult[String] = {
    if (i.length > 0) {
      val iword = i(0)
      if (iword.toLowerCase equals k.toLowerCase)
        ParseResult(true, ParseStatus(l + k.length + 1, i.tail), None, Set.empty, k)
      else {
        val cm = countMatches(iword.toLowerCase, k.toLowerCase)
        val bestStat =  if (cm > 0)
                          if (cm == iword.length)
                            Some(ParseStatus(l + cm, i.tail))
                          else
                            Some(ParseStatus(l + cm, iword.substring(cm, iword.length) :: i.tail))
                        else None
        ParseResult(false, ParseStatus(l, i), bestStat, Set(k.substring(cm, k.length)), null)
      }
    }
    else ParseResult(false, ParseStatus(l, i), None, Set(k), null)
    // else ParseResult(false, ParseStatus(l, i), Some(ParseStatus(l,i)), Set(k), null)
  }

  case class specialIdent(ignored:List[String]) extends Parser[String] {
    name = "(name)"
    def apply(i:Input, l:Int, depth:Int) = {
      if ((i.length > 0) && (! gatheredKeywords.contains(i(0))) && (!(i(0).charAt(0).isDigit))) {
        if ((i.length == 1) && (!ignored.forall(x => !(x startsWith i(0))))) {
            ParseResult(false, ParseStatus(l, i), None, Set(name), null)
        } else
          ParseResult(true, ParseStatus(l + i(0).length, i.tail), None, Set.empty, i(0))
      } else ParseResult(false, ParseStatus(l, i), None, Set(name), null)
    }
    override def toString = "specialident"
  }

  class Ident extends Parser[String] {
    name = "(ident)"
    def apply(i:Input, l:Int, depth:Int) = {
      if ((i.length > 0) && (! gatheredKeywords.contains(i(0))) && (!(i(0).charAt(0).isDigit))) {
        ParseResult(true, ParseStatus(l + i(0).length, i.tail), None, Set.empty, i(0))
      } else ParseResult(false, ParseStatus(l, i), None, Set(name), null)
    }
    override def toString = "ident"
  }

  def ident = new Ident

  implicit def key(k:String):Parser[String] = new Parser[String] {
    val words = k.split(" ").filter(_.length > 0).toList
    val keys = words.map(w => new Parser[String] {def apply(i:Input,l:Int,depth:Int) = keyword(w,i,l,depth)})
    gatheredKeywords ++= words
    val myParser:Parser[Any] = if (keys.length > 1) 
                                  keys.reduceRight[Parser[Any]](andParser(_, _))
                               else if (keys.length == 1)
                                  keys(0)
                               else emptyParser     // TODO Throw exception

    def apply(i:Input, l:Int, depth:Int) = {
      val res = myParser(i,l,depth)
      ParseResult(res.success, res.status, res.best, res.completions, k)
    }

    override def toString = "key("+k+")"
  }

  class alwaysSuccess[+X](x:X) extends Parser[X] {
    def apply(i:Input, l:Int, depth:Int) = ParseResult(true, ParseStatus(l, i), None, Set.empty, x)
  }
  val emptyParser = new alwaysSuccess("")
  // def empty = new Parser { def apply(i:Input, l:Int, depth:Int) = ParseResult(true, l, i, Nil) }

  def failParser[T](obj:T) : Parser[T] = new Parser[T] {
    def apply(i:Input, l:Int, depth:Int) = ParseResult[T](false, ParseStatus(l, i), None, Set.empty, obj)
  }

  /** The equivalent of ? in EBNF */
  def optParser[T](p: Parser[T]):Parser[Option[T]] = new Parser[Option[T]] {
    def apply(i:Input, l:Int, depth:Int) = {
      val pr = p(i, l, depth)
      if (pr.success) ParseResult(true, pr.status, pr.best, pr.completions, Some(pr.result))
      else {
        val completions:Set[String] = if (i.size > 0) {
          if (pr.best.isDefined) pr.completions
          else Set.empty
        } else pr.completions.map("["+_+"]")
        ParseResult(true, ParseStatus(l, i), pr.best, completions, None)
      }
    }
    override def toString = "optional("+p.toString+")"
  }

  /** A quick way to create | parsers from a list of parsers */
  def alternativesParser[T](l : Seq[Parser[T]]) = new Parser[T] {
    val parsers = l.reduceLeft(_ | _)
    def apply(i:Input, l:Int, depth:Int) = {
      val pr = parsers(i,l,depth)
      val completions = if(pr.completions.size > 0) {
        val innerStr = pr.completions.reduceLeft(_ + '/' + _)
        // val surroundedStr = if(pr.completions.size == 1) innerStr else ('{'+innerStr+'}')
        Set(innerStr)
      } else pr.completions
      ParseResult(pr.success, pr.status, pr.best, completions, pr.result)
    }
    override def toString = "alternatives("+l+")"
  }

  /** The equivalent of * in EBNF */
  def zeroOrMoreParser[T](p: => Parser[T]):Parser[List[T]] = new Parser[List[T]] {
    def apply(i:Input, l:Int, depth:Int) = {
      var pr = ParseResult(true, ParseStatus(l, i), None, Set.empty, null.asInstanceOf[T])
      var results:List[T] = Nil
      while (pr.success) {
        pr = p(pr.status.rem, pr.status.parsedChars, depth)
        if (pr.success)
          results ::= (pr.result)
      }
      val maxWeight = if (p.weight > weight) p.weight else weight
      val completions:Set[String] = if ((depth > maxWeight) && ((!pr.best.isDefined) || pr.best.get.rem.length == 0)) pr.completions else Set.empty
      ParseResult(true, pr.status, pr.best, completions, results.reverse)
    }
    override def toString = "zeroOrMore("+p.toString+")"
  }

  /* quicker version. Has been overriden by the longest matcher (see below)

  def orParser[X, Y >: X](p: => Parser[X], q: => Parser[Y]) : Parser[Y] = new Parser[Y]{
      def apply(i:Input, l:Int, depth:Int) = {
          val pr = p(i, l, depth)

          if (pr.success) pr
          else {
              val qr = q(i, l, depth)
              if ((qr.success) || (pr.parsedChars < qr.parsedChars))
                  qr
              else if (pr.parsedChars == qr.parsedChars) {
                  val completions = if (depth > weight) pr.completions ++ qr.completions else Set(name)
                  ParseResult(qr.success, qr.parsedChars, qr.rem, completions, qr.result)
              } else
                  pr
          }
      }
  }
*/
  // Longest matcher
  def orParser[X, Y >: X](p: => Parser[X], q: => Parser[Y]) : Parser[Y] = new Parser[Y]{
    def apply(i:Input, l:Int, depth:Int) = {
      val pr = p(i, l, depth)
      val qr = q(i, l, depth)

      if (pr.success && !qr.success) pr
      else if (qr.success && !pr.success) qr
      else if (pr.success && qr.success) {
        if (pr.status.parsedChars > qr.status.parsedChars) pr
        else qr
      }
      else {
        if (pr.best.isDefined && (!qr.best.isDefined)) pr
        else if ((!pr.best.isDefined) && qr.best.isDefined) qr
        else if (pr.best.isDefined && qr.best.isDefined) {
          if (pr.best.get.parsedChars > qr.best.get.parsedChars) pr
          else if (pr.best.get.parsedChars == qr.best.get.parsedChars) {
              ParseResult(false, pr.status, pr.best, pr.completions ++ qr.completions, pr.result)
          } else qr
        } else {
          val completions = if (depth > weight) pr.completions ++ qr.completions else Set(name)
          ParseResult(false, ParseStatus(l, i), None, completions, null.asInstanceOf[Y])
        }
      }
    }
    override def toString = "Or("+p.toString+","+q.toString+")"
  }

  private def joinCompletions(a:ParseResult[_], b:ParseResult[_]) = {
    if (a.completions.size > 0) {
      if(b.completions.size > 0) {
        a.completions.flatMap(ac => (b.completions).map(ac + " " + _))
      } else {
        a.completions
      }
    } else b.completions
  }

  def noDeepAndParser[X, Y](p: => Parser[X], q: => Parser[Y]) : Parser[and[X,Y]] = new Parser[and[X,Y]]{
    def apply(i:Input, l:Int, depth:Int) = {
      val pr = p(i, l, depth)

      if (pr.success) {
        val qr = q(pr.status.rem, pr.status.parsedChars, depth)
        if (qr.success)
          ParseResult(qr.success, qr.status, qr.best, qr.completions, and(pr.result, qr.result))
        else
          ParseResult(false, ParseStatus(l, i), if (pr.status.parsedChars > l) Some(pr.status) else None, joinCompletions(pr,qr), null)
      } else {
        val qr = q(Nil, 0, depth)
        ParseResult(pr.success, pr.status, pr.best, joinCompletions(pr,qr), and(pr.result, null.asInstanceOf[Y]))
      }
    }
  }

  def getBest(min:Int, x:Option[ParseStatus], y:Option[ParseStatus]) = {
    val temp =
      if (x.isDefined && !y.isDefined) x
      else if (!x.isDefined && y.isDefined) y
      else if (x.isDefined && y.isDefined) {
        if (x.get.parsedChars > y.get.parsedChars) x
        else y
      } else None
    if (temp.isDefined && (temp.get.parsedChars > min)) temp else None
  }

  def andParser[X, Y](p: => Parser[X], q: => Parser[Y]) : Parser[and[X,Y]] = new Parser[and[X,Y]]{
    def apply(i:Input, l:Int, depth:Int) = {
      val pr = p(i, l, depth)
      val maxWeight = if (weight < p.weight) p.weight else weight // max weight

      if (pr.success) {
        val qr = q(pr.status.rem, pr.status.parsedChars, depth)
        if (qr.success)
          ParseResult(
            qr.success,
            qr.status,
            getBest(l, pr.best, qr.best),
            (qr.completions),
            and(pr.result, qr.result))
        else {
          val best = getBest(l, Some(pr.status), getBest(l, pr.best, qr.best))
          val qrBestParsed = qr.best.map(_.parsedChars).getOrElse(0)
          val prBestParsed = pr.best.map(_.parsedChars).getOrElse(0)
          val completions =
            if (qrBestParsed > prBestParsed)
              qr.completions
            else if ((qrBestParsed == prBestParsed) && (qrBestParsed != 0))
              (pr.completions ++ qr.completions)
            else
              joinCompletions(pr,qr)
          ParseResult(false, ParseStatus(l, i), best, completions, null)
        }
      } else if (depth > maxWeight) {
        val qr = q(Nil, pr.status.parsedChars, depth - maxWeight)
        ParseResult(false, pr.status, pr.best, joinCompletions(pr, qr), null)
      } else {
        if (nameIsSet || (pr.completions.size == 0))
          ParseResult(false, pr.status, pr.best, Set(name), null)
        else
          ParseResult(pr.success, pr.status, pr.best, pr.completions, and(pr.result, null.asInstanceOf[Y]))
      }
    }
    override def toString = "And("+p.toString+","+q.toString+")"
  }

  def funcApplyParser[X,Y](p: => Parser[X], f : X=>Y) : Parser[Y] = new Parser[Y] {
    def apply(in:Input, l:Int, depth:Int) = {
      val presult = p.apply(in, l, depth)
      val applyResult = if(presult.success) f(presult.result) else (null.asInstanceOf[Y])
      ParseResult(presult.success, presult.status, presult.best, presult.completions, applyResult)
    }
    override def toString = "applyF()"
  }

  def numParser: Parser[Int] = new Parser[Int] {
    def apply(in:Input, l:Int, depth:Int) = {
      if(in.length > 0) {
        var i = 0
        val s = in(0)
        while ((i < s.length) && (s.charAt(i).isDigit)) i += 1

        if (i == s.length)
          ParseResult(true, ParseStatus(l + i, in.tail), None, Set.empty, s.toInt)
        else
          ParseResult(false, ParseStatus(l, in), None, Set("(number)"), 0)
      } else ParseResult(false, ParseStatus(l, in), None, Set("(number)"), 0)
    }
    override def toString = "numParser"
  }

  def oneOfEachParser[T](parserSeq:Seq[(Parser[T], Int)]) = new Parser[List[T]] {
    val parserList = parserSeq.toList
    val lifeOfParser = Map[Int,Int]() ++ parserList.zipWithIndex.map(x => x._2 -> x._1._2)
    val parserCount = parserList.map(_._1)
    val parsers = parserList.map(_._1)
    val (alwaysParsers, restrictedParsers) = parsers.zipWithIndex.partition(x => lifeOfParser(x._2) == 0)
    val countAlwaysParsers = alwaysParsers.length

    def apply(i:Input, l:Int, depth:Int) = {
      val lifeOfRestricted =
        scala.collection.mutable.Map[Int,Int]() ++
          restrictedParsers.map(r => r._2 -> lifeOfParser(r._2))

      var activeRestrictedParsers = restrictedParsers
      var lastSuccessfulResult:Option[(ParseResult[T],Int)] = None

      var noMatch = false
      var remainingInput = i
      var parsedChars = l

      var results = List[ParseResult[T]]()
      var completions = Set[String]()
      var best : Option[ParseStatus] = None

      do {
        val currentParsers = alwaysParsers ::: activeRestrictedParsers
        val matchResults = (alwaysParsers ::: activeRestrictedParsers).view map {
            case (p,i) => (p(remainingInput, parsedChars, depth), i)
        }
        val matchFound = matchResults find (r => r._1.success)

        if (matchFound.isDefined) {
          val parserNum = matchFound.get._2
          if (parserNum >= countAlwaysParsers) {
            // this was one of the restricted parsers
            val previousLife = lifeOfRestricted(parserNum)
            lifeOfRestricted(parserNum) = previousLife - 1
            activeRestrictedParsers = activeRestrictedParsers filter (x => lifeOfRestricted(x._2) > 0)
          }
          parsedChars = matchFound.get._1.status.parsedChars
          remainingInput = matchFound.get._1.status.rem
          results ::= (matchFound.get._1)
          lastSuccessfulResult = matchFound
        } else {
          var mostParsed = 0
          val (haveBest, noBest) = (lastSuccessfulResult.toList ++ matchResults).map(_._1).partition(_.best.isDefined)
          haveBest foreach {r =>
            val parsedCharsInR = r.best.get.parsedChars
            if (parsedCharsInR > mostParsed) {
              mostParsed = parsedCharsInR
              completions = r.completions
              best = r.best
            } else if (parsedCharsInR == mostParsed) {
              completions ++= r.completions
            }
          }
          if (haveBest.isEmpty) {
            completions ++= noBest.flatMap(_.completions)
          }

          noMatch = true
        }
      } while (noMatch == false)

      ParseResult(parsedChars != l,
        ParseStatus(parsedChars,remainingInput),
        best, completions,
        results.map(_.result))

    }

    override def toString = "oneOf("+parserSeq+")"
  }

  def eoiParser[T](parser:Parser[T]) = new Parser[T] {
    def apply(i:Input, l:Int, depth:Int) = {
      val result = parser(i,l,depth)
      if (result.success) {
        if (result.status.rem.length == 0) {
          result
        } else {
          if (result.best.isDefined) {
            ParseResult(false, result.status, result.best,
              if(result.best.get.rem.length == 0) result.completions else Set.empty,
              result.result)
          } else {
            ParseResult(false, result.status, result.best,
              Set.empty,
              result.result)
          }
        }
      } else {
        result
      }
    }
  }
}

/*
object Main extends Parsers {
  // val myParser = (numParser * ("+" ^^ {(s) => {(p:Int,q:Int) => (p + q)}}))
  val monthParser = ("jan" | "feb" | "march") ~ ((numParser) ?)
  val periodParser = "last" ~ numParser ~ "week"
  val dateclause = monthParser | periodParser
  val cmpCondParser = ("in" ~ ident).named("cmp cond")
  val cmpParser = ("company" ~ (cmpCondParser +).named("[cmp cond]*")).named("<cmp>")
  val cmpExpr = (ident+) | cmpParser
  val payCondParser = (("in" ~ dateclause).named("in month") | (("made by" ~ cmpExpr).named("made by c"))).named("pay cond")
  val payParser = ("payment" ~ (payCondParser +).named("[pay cond]*")).named("<pay>")
  val myParser = payParser

  val totalP = "total" ~ ("amount of" ?) ~ payParser
}

object MyApp extends App {
  println(Main.payParser("payment made by company"))
}
*/
