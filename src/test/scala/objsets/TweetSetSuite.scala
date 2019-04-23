package objsets

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TweetSetSuite extends FunSuite {
  trait TestSets {
    val set1 = new Empty
    val set2 = set1.incl(new Tweet("a", "a body", 20))
    val set3 = set2.incl(new Tweet("b", "b body", 20))
    val c = new Tweet("c", "c body", 7)
    val d = new Tweet("d", "d body", 9)
    val set4c = set3.incl(c)
    val set4d = set3.incl(d)
    val set5 = set4c.incl(d)
  }

  def toTweetList(toConvert:List[Tweet]): TweetList = {
    if(toConvert == null || toConvert == List())
      Nil
    else
      new Cons(toConvert.head, toTweetList(toConvert.tail))
  }

  def mostRetweeted(tweet1: Tweet, tweet2: Tweet):Tweet =
    if(tweet1 == null)
      if(tweet2==null) null else tweet2
    else
    if(tweet2==null)
      tweet1
    else
    if(tweet1.retweets>tweet2.retweets) tweet1 else tweet2

  def asSet(tweets: TweetSet): Set[Tweet] = {
    var res = Set[Tweet]()
    tweets.foreach(res += _)
    res
  }

  def size(set: TweetSet): Int = asSet(set).size

  test("filter: on empty set") {
    new TestSets {
      assert(size(set1.filter(tw => tw.user == "a")) === 0)
    }
  }

  test("filter: a on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.user == "a")) === 1)
    }
  }

  test("filter: 20 on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.retweets == 20)) === 2)
    }
  }

  test("union: set4c and set4d") {
    new TestSets {
      assert(size(set4c.union(set4d)) === 4)
    }
  }

  test("union: with empty set (1)") {
    new TestSets {
      assert(size(set5.union(set1)) === 4)
    }
  }

  test("union: with empty set (2)") {
    new TestSets {
      assert(size(set1.union(set5)) === 4)
    }
  }

  test("descending: set5") {
    new TestSets {
      val trends = set5.descendingByRetweet
      assert(!(trends.isEmpty))
      assert((trends.head).user == "a" || (trends.head).user == "b")
    }
  }

  test("filter accumulated:") {
    new TestSets {
      val acc = new Tweet("mariadasilva", "accumulator", 75)
      val trends = set5.filterAcc(x => x.retweets >= 20, new NonEmpty(acc, new Empty(), new Empty()))
      assert(!(trends.isInstanceOf[Empty]))
      assert(size(trends) == 2)
      assert((trends.mostRetweeted).retweets == 75)
    }
  }

  test("binary mostRetwweted:") {

    new TestSets {
      val acc = new Tweet("mariadasilva", "accumulator", 75)
      val my_tweet = new Tweet("joaodasilva", "iwiuhfiw", 1000)
      val mostRetweetedTweet = mostRetweeted(acc, my_tweet)
      assert(mostRetweetedTweet.retweets == 1000)
    }
  }

  test("include includes:") {
    new TestSets {
      val my_tweet1 = new Tweet("mariadasilva", "accumulator", 75)
      val my_tweet2 = new Tweet("joaodasilva", "iwiuhfiw", 1000)
      val newTweetSetIncluded = set4c.incl(my_tweet2)
      assert(size(newTweetSetIncluded) == size(set4c)+1)
    }
  }


  test("remove removes:") {
    new TestSets {
      val my_tweet1 = new Tweet("mariadasilva", "accumulator", 75)
      val my_tweet2 = new Tweet("joaodasilva", "iwiuhfiw", 1000)
      val newTweetSetRemoved = set4c.remove(c)
      assert(newTweetSetRemoved.contains(c)==false)
    }
  }

  test("contain checks whether the set contains the tweet:") {
    new TestSets {
      val my_tweet1 = new Tweet("mariadasilva", "accumulator", 75)
      assert(set1.contains(my_tweet1)==false)
    }
  }

  test("head returns the head of the list:") {
    new TestSets {
      assert((set2.head).user == "a")
    }
  }

  test("toTweetList converts") {
    new TestSets {
      val a = new Tweet("a", "a body", 20)
      val b = new Tweet("b", "b body", 20)
      assert((set4d.toTweetList).equals(toTweetList(List(d, b, a))))
    }
  }

  test("getLeft gets the left side") {
    new TestSets {
      val b = new Tweet("b", "b body", 20)
      assert(set4d.isInstanceOf[NonEmpty])
      val asNonEmpty = set4d.asInstanceOf[NonEmpty]
      val expected_result = new Empty()
      //println("tipo getLeft esperado é "+expected_result.getClass)
      val my_left = asNonEmpty.getLeft
      //println("tipo de my_left é "+my_left.getClass)
      //println("my_left é "+my_left.foreach(x=>println(x)))
      assert(my_left.equals(expected_result.asInstanceOf[Empty]))
    }
  }

  test("getRight gets the right side") {
    new TestSets {
      val a = new Tweet("a", "a body", 20)
      val b = new Tweet("b", "b body", 20)
      assert(set4d.isInstanceOf[NonEmpty])
      val asNonEmpty = set4d.asInstanceOf[NonEmpty]
      val expected_result = new NonEmpty(b, new Empty , new NonEmpty(d, new Empty, new Empty))//toTweetList(List(b, d)).toTweetSet
      //println("tipo getRight esperado é "+expected_result.getClass)
      val my_right = asNonEmpty.getRight
      println("tipo de my_right é "+my_right.getClass)

      println("my_right é "+my_right.foreach(x=>println(x)))
      assert(my_right.equals(expected_result.asInstanceOf[NonEmpty]))
    }
  }

  /*
  filter and union: tweets with 321 and 205 retweets
  [Observed Error] Set() had size 0 instead of expected size 1 exactly one such tweet
  [Lost Points] 10
   */
  test("filter and union: tweets with 321 and 205 retweets"){
    new TestSets {

    }
  }
  /*
  [Test Description] filter and union: tweets with 321 and 205 retweets
  [Observed Error] Set() had size 0 instead of expected size 1 exactly one such tweet
  [Lost Points] 10
   */
  test("filter and union: tweets with 321 and 205 retweets"){
    new TestSets {

    }
  }
  /*
  [Test Description] trending: google and apple tweets
  [Observed Error] A fatal exception has been thrown: java.lang.StackOverflowError
  [Lost Points] 10
   */
  test("trending: google and apple tweets"){
    new TestSets {


    }
  }
}
