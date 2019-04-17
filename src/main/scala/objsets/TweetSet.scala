package objsets

import java.security.KeyStore.TrustedCertificateEntry
import java.util.NoSuchElementException

import TweetReader._
import com.sun.javafx.image.BytePixelSetter

import scala.Boolean
import scala.List
import org.w3c.dom.ranges.RangeException

/**
 * A class to represent tweets.
 */
class Tweet(val user: String, val text: String, val retweets: Int) {
  override def toString: String =
    "myTweet"

  override def equals(obj: Any): Boolean = {
    println("entrou Tweet.equals("+obj+")")
    if(this==null) {
      println("saiu Tweet.equals("+obj+")")
      true
    }
    else if(obj == null) {
      println("saiu Tweet.equals("+obj+")")
      false
    }
    else if(!obj.isInstanceOf[Tweet]) {
      println("saiu Tweet.equals("+obj+")")
      false
    }
    else{
      val objAsTweet = obj.asInstanceOf[Tweet]
      val retorno = (((objAsTweet.user).equals(this.user)) && (objAsTweet.text).equals(this.text)) && objAsTweet.retweets == this.retweets
      println("saiu Tweet.equals("+obj+")")
      retorno
    }
  }
}

/**
 * This represents a set of objects of type `Tweet` in the form of a binary search
 * tree. Every branch in the tree has two children (two `TweetSet`s). There is an
 * invariant which always holds: for every branch `b`, all elements in the left
 * subtree are smaller than the tweet at `b`. The elements in the right subtree are
 * larger.
 *
 * Note that the above structure requires us to be able to compare two tweets (we
 * need to be able to say which of two tweets is larger, or if they are equal). In
 * this implementation, the equality / order of tweets is based on the tweet's text
 * (see `def incl`). Hence, a `TweetSet` could not contain two tweets with the same
 * text from different users.
 *
 *
 * The advantage of representing sets as binary search trees is that the elements
 * of the set can be found quickly. If you want to learn more you can take a look
 * at the Wikipedia page [1], but this is not necessary in order to solve this
 * assignment.
 *
 * [1] http://en.wikipedia.org/wiki/Binary_search_tree
 */
abstract class TweetSet {

  /**
   * This method takes a predicate and returns a subset of all the elements
   * in the original set for which the predicate is true.
   *
   * Question: Can we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
    def filter(p: Tweet => Boolean): TweetSet //the implementation need to be in the subclasses

  /**
   * This is a helper method for `filter` that propagetes the accumulated tweets.
   */
  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet

  /**
   * Returns a new `TweetSet` that is the union of `TweetSet`s `this` and `that`.
   *
   * Question: Should we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
    def union(that: TweetSet): TweetSet //the implementation need to be in the subclasses

  /**
   * Returns the tweet from this set which has the greatest retweet count.
   *
   * Calling `mostRetweeted` on an empty set should throw an exception of
   * type `java.util.NoSuchElementException`.
   *
   * Question: Should we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
    def mostRetweeted: Tweet //the implementation need to be in the subclasses

  /**
   * Returns a list containing all tweets of this set, sorted by retweet count
   * in descending order. In other words, the head of the resulting list should
   * have the highest retweet count.
   *
   * Hint: the method `remove` on TweetSet will be very useful.
   * Question: Should we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
    def descendingByRetweet: TweetList //the implementation need to be in the subclasses

  /**
   * The following methods are already implemented
   */

  /**
   * Returns a new `TweetSet` which contains all elements of this set, and the
   * the new element `tweet` in case it does not already exist in this set.
   *
   * If `this.contains(tweet)`, the current set is returned.
   */
  def incl(tweet: Tweet): TweetSet

  /**
   * Returns a new `TweetSet` which excludes `tweet`.
   */
  def remove(tweet: Tweet): TweetSet

  /**
   * Tests if `tweet` exists in this `TweetSet`.
   */
  def contains(tweet: Tweet): Boolean

  /**
   * This method takes a function and applies it to every element in the set.
   */
  def foreach(f: Tweet => Unit): Unit

  def head: Tweet

  def toTweetList: TweetList

  override def equals(other: Any):Boolean

  override def toString:String
}

class Empty extends TweetSet {


  /**
   * The following methods are already implemented
   */

  def contains(tweet: Tweet): Boolean = false

  def incl(tweet: Tweet): TweetSet = new NonEmpty(tweet, new Empty, new Empty)

  def remove(tweet: Tweet): TweetSet = this

  def foreach(f: Tweet => Unit): Unit = ()

  def filter(p: Tweet => Boolean): TweetSet = new Empty

  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = acc

  def union(that: TweetSet):TweetSet = that

  def descendingByRetweet: TweetList = Nil

  def mostRetweeted: Tweet = null

  def head: Tweet = null

  def toTweetList: TweetList = Nil

  override def equals(other: Any):Boolean = {
    println("entrou Empty.equals")
    if(other==null) {
      println("saiu Empty.equals")
      false
    }
    else if(!(other.isInstanceOf[Empty])) {
      println("saiu Empty.equals")
      false
    }
    else {
      println("saiu Empty.equals")
      true
    }
  }

  override def toString: String = "{Empty}"
}

class NonEmpty(elem: Tweet, left: TweetSet, right: TweetSet) extends TweetSet {

    
  /**
   * The following methods are already implemented
   */

  def contains(x: Tweet): Boolean = {
    println("entrou NonEmpty.contains")
    if (x.text < elem.text) {
      val retorno = left.contains(x)
      println("saiu NonEmpty.contains")
      retorno
    }
    else if (elem.text < x.text) {
     val retorno = right.contains(x)
     println("saiu NonEmpty.contains")
     retorno
    }
    else{
      println("saiu NonEmpty.contains")
      true
    }
  }

  def incl(x: Tweet): TweetSet = {
    println("entrou NonEmpty.incl("+x+"), this="+this+")")
    if (x.text < elem.text) {
      if(left.isInstanceOf[Empty]|| left.head == null ){
        println("saiu NonEmpty.incl("+x+"), this="+this+")")
        new NonEmpty(elem, new NonEmpty(x, new Empty, new Empty), right)
      }
      else {
        println("left.incl(myTweet), left="+left)
        val retorno= new NonEmpty(elem, left.incl(x), right)
        println("saiu NonEmpty.incl("+x+"), this="+this+")")
        retorno
      }
    }
    else if (elem.text < x.text) {
      if(right.isInstanceOf[Empty]|| right.head == null ){
        println("saiu NonEmpty.incl("+x+"), this="+this+")")
        new NonEmpty(elem, right,  new NonEmpty(x, new Empty, new Empty))
      }
      else {
        println("right.incl(myTweet), right="+right)
        val retorno = new NonEmpty(elem, left, right.incl(x))
        println("saiu NonEmpty.incl("+x+"), this="+this+")")
        retorno
      }
    }
    else {
      println("saiu NonEmpty.incl("+x+"), this="+this+")")
      this
    }
  }

  def remove(tw: Tweet): TweetSet = {
    println("entrou NonEmpty.remove("+tw+")")
    if (tw.text < elem.text) {

     val retorno = new NonEmpty(elem, left.remove(tw), right)
     println("saiu NonEmpty.remove("+tw+")")
     retorno
    }
    else if (elem.text < tw.text){
      val retorno = new NonEmpty(elem, left, right.remove(tw))
      println("saiu NonEmpty.remove("+tw+")")
      retorno
    }
    else {
      val retorno = left.union(right)
      println("saiu NonEmpty.remove("+tw+")")
      retorno
    }
  }

  def foreach(f: Tweet => Unit): Unit = {
    println("entrou NonEmpty.foreach("+f+")")
    f(elem)
    if(left != null && (!(left.isInstanceOf[Empty])))
      left.foreach(f)

    if(right != null && (!(right.isInstanceOf[Empty])))
      right.foreach(f)
    println("saiu NonEmpty.foreach("+f+")")

  }

  def filter(p: Tweet => Boolean): TweetSet = {
    println("entrou NonEmpty.filter("+p+")")
    val retorno = filterAcc(p, new Empty)
    println("saiu NonEmpty.filter("+p+")")
    retorno
  }

  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = {
    println("entrou NonEmpty.filterAcc("+p+", "+acc+")")
    if (p(elem)) {
      val retorno = right.filterAcc(p, left.filterAcc(p, acc.incl(elem)))
      println("saiu NonEmpty.filterAcc("+p+", "+acc+")")
      retorno
    }
    else {
      val retorno = right.filterAcc(p, left.filterAcc(p, acc))
      println("saiu NonEmpty.filterAcc("+p+", "+acc+")")
      retorno
    }
  }

  def head: Tweet =
    this.elem

  def union(that: TweetSet):TweetSet = {
    println("entrou NonEmpty.union("+that+")")
    if (that == null || that.head == null) {
      val retorno = this
      println("saiu NonEmpty.union("+that+")")
      retorno
    }
    else {
      val retorno = (this.incl(that.head)).union((that.remove(that.head)))
      println("saiu NonEmpty.union("+that+")")
      retorno
    }
  }



  def descendingByRetweet: TweetList = {

    def auxDescendingByRetweet(base: TweetSet, partialResult: TweetList): TweetList = {
      println("entrou auxDescendingByRetweet("+base+","+partialResult+")")
      if (base.mostRetweeted == null) {
        val retorno = Nil
        println("saiu auxDescendingByRetweet("+base+","+partialResult+")")
        retorno
      }
      else {
        //val mostRetweetedSet: TweetSet = new NonEmpty(mostRetweeted, new Empty, new Empty) //TweetSet
        //val listWithoutMostRetweeted: TweetList = (this.remove(mostRetweeted)).descendingByRetweet //TweetList

        //val setWithoutMostRetweeted: TweetSet = listWithoutMostRetweeted.toTweetSet
        //(mostRetweetedSet.union(setWithoutMostRetweeted)).toTweetList

        val theMostRetweeted = base.mostRetweeted
        val removedList = base.remove(theMostRetweeted)
          if(removedList.isInstanceOf[NonEmpty]) {
            val asNonEmpty = removedList.asInstanceOf[NonEmpty]
            val retorno = auxDescendingByRetweet(asNonEmpty, partialResult.include(mostRetweeted))
            println("saiu auxDescendingByRetweet("+base+","+partialResult+")")
            retorno
          }
          else{
            val retorno = partialResult.include(mostRetweeted)
            println("saiu auxDescendingByRetweet("+base+","+partialResult+")")
            retorno
          }
      }

      }
      println("entrou NonEmpty.descendingByRetweet")
      val retorno=auxDescendingByRetweet(this, Nil)
      println("saiu NonEmpty.descendingByRetweet")
      retorno
  }
//                                              TweetSet.union(TweetList
  def mostRetweeted: Tweet = {
    println("entrou NonEmpty.mostRetweeted")
    val retorno = mostRetweeted(mostRetweeted(left.mostRetweeted, elem), right.mostRetweeted)
    println("saiu NonEmpty.mostRetweeted")
    retorno
  }

  def mostRetweeted(tweet1: Tweet, tweet2: Tweet):Tweet = {

    println("entrou NonEmpty.mostRetweeted("+tweet1+", "+tweet2+")")
    if (tweet1 == null) {
      val retorno = tweet2
      println("saiu NonEmpty.mostRetweeted("+tweet1+", "+tweet2+")")
      retorno
    }
    else if (tweet2 == null) {
      val retorno = tweet1
      println("saiu NonEmpty.mostRetweeted("+tweet1+", "+tweet2+")")
      retorno
    }
    else if (tweet1.retweets > tweet2.retweets) {
      val retorno = tweet1
      println("saiu NonEmpty.mostRetweeted("+tweet1+", "+tweet2+")")
      retorno
    }
    else {
      val retorno = tweet2
      println("saiu NonEmpty.mostRetweeted("+tweet1+", "+tweet2+")")
      retorno
    }
  }
  def getLeft(): TweetSet = {

    println("entrou NonEmpty.getLeft")
    if(left.isInstanceOf[NonEmpty]) {
      val nonEmptyLeft = left.asInstanceOf[NonEmpty]
      val retorno = new NonEmpty(nonEmptyLeft.head, nonEmptyLeft.getLeft(), nonEmptyLeft.getRight())
      println("saiu NonEmpty.getLeft")
      retorno
    }
    else{
      val retorno = new Empty()
      println("saiu NonEmpty.getLeft")
      retorno
    }
  }
  def getRight():TweetSet = {

    println("entrou NonEmpty.getRight")
    if(right.isInstanceOf[NonEmpty]) {
      val nonEmptyRight = right.asInstanceOf[NonEmpty]
      val retorno = new NonEmpty(nonEmptyRight.head, nonEmptyRight.getLeft(), nonEmptyRight.getRight())
      println("saiu NonEmpty.getRight")
      retorno
    }
    else {
      val retorno =  new Empty()
      println("saiu NonEmpty.getRight")
      retorno
    }
  }
  def toTweetList: TweetList = {

    println("entrou NonEmpty.toTweetList")
    def auxToTweetList(currentSubSet: TweetSet):List[Tweet] = {
      println("entrou NonEmpty.auxToTweetList")
      if(((currentSubSet == null) || (!(currentSubSet.isInstanceOf[NonEmpty])))|| (currentSubSet.head == null)) {
        val retorno =  List()
        println("saiu NonEmpty.auxToTweetList")
        retorno
      }
      else {
        val nonEmptySet = currentSubSet.asInstanceOf[NonEmpty]
        val retorno = (auxToTweetList(nonEmptySet.getLeft()) ::: List(currentSubSet.head)) ::: auxToTweetList(nonEmptySet.getRight)
        println("saiu NonEmpty.auxToTweetList")
        retorno
      }
    }
    val retorno = toTweetList(auxToTweetList(this))
    println("saiu NonEmpty.toTweetList")
    retorno
  }
  def toTweetList(toConvert:List[Tweet]): TweetList = {

    println("entrou NonEmpty.toTweetList(List[Tweet])")
    if(toConvert == null || toConvert == List()) {
      val retorno = Nil
      println("saiu NonEmpty.toTweetList(List[Tweet])")
      retorno
    }
    else {
      val retorno = new Cons(toConvert.head, toTweetList(toConvert.tail))
      println("saiu NonEmpty.toTweetList(List[Tweet])")
      retorno
    }
  }

  override def equals(other: Any):Boolean = {

    println("entrou NonEmpty.equals")
    if(other==null) {
      val retorno = false
      println("saiu NonEmpty.equals")
      retorno
    }
    else if(!(other.isInstanceOf[NonEmpty])) {
      val retorno = false
      println("saiu NonEmpty.equals")
      retorno
    }
    else {
      val objAsNonEmpty = other.asInstanceOf[NonEmpty]
      if (!(objAsNonEmpty.head.equals(this.head))) {
        val retorno = false
        println("saiu NonEmpty.equals")
        retorno
      }
      else {

        val my_left = this.left
        val my_right = this.right
        val that_left = objAsNonEmpty.getLeft
        val that_right = objAsNonEmpty.getRight
        if(my_left.isInstanceOf[Empty] != that_left.isInstanceOf[Empty]) {
          val retorno = false
          println("saiu NonEmpty.equals")
          retorno
        }
        if(my_right.isInstanceOf[Empty] != that_right.isInstanceOf[Empty]) {
          val retorno = false
          println("saiu NonEmpty.equals")
          retorno
        }

        if (my_left.isInstanceOf[NonEmpty])
            if (!(my_left.equals(that_left))) {
              val retorno = false
              println("saiu NonEmpty.equals")
              retorno
            }
        if (my_right.isInstanceOf[NonEmpty])
          if (!(my_right.equals(that_right))) {
            val retorno = false
            println("saiu NonEmpty.equals")
            retorno
          }

        val retorno = true
        println("saiu NonEmpty.equals")
        retorno
      }
    }
  }

  override def toString:String = "{NonEmpty{"+left+", "+elem+", "+right+"}}"
}

trait TweetList {
  def head: Tweet
  def tail: TweetList
  def isEmpty: Boolean
  def foreach(f: Tweet => Unit): Unit = {
    println("entrou TweetList.foreach")
    if (!isEmpty) {
      f(head)
      tail.foreach(f)
    }
    println("saiu TweetList.foreach")

  }
  def toTweetSet:TweetSet
  def include(toInsert: Tweet): TweetList
  def remove(toRemove: Tweet): TweetList
  override def equals(obj: Any): Boolean
}

object Nil extends TweetList {
  def head = throw new java.util.NoSuchElementException("head of EmptyList")
  def tail = throw new java.util.NoSuchElementException("tail of EmptyList")
  def isEmpty = true
  def toTweetSet = new Empty
  override def equals(obj: Any): Boolean = {

    println("entrou Nil.equals")
    if(obj.isInstanceOf[TweetList]){
      val asTweetList = obj.asInstanceOf[TweetList]
      val retorno = asTweetList.isEmpty
      println("saiu Nil.equals")
      retorno
    }
    else {
      val retorno = true
      println("saiu Nil.equals")
      retorno
    }
  }
  def include(toInsert: Tweet) = new Cons(toInsert, Nil)
  def remove(toRemove: Tweet) = throw new java.util.NoSuchElementException("tail of EmptyList")

}

class Cons(val head: Tweet, val tail: TweetList) extends TweetList {
  def isEmpty = false
  def toTweetSet = new NonEmpty(head, new Empty, tail.toTweetSet)
  override def equals(obj: Any): Boolean = {

    println("entrou Cons.equals")
    if(obj.isInstanceOf[TweetList]){
      val asTweetList = obj.asInstanceOf[TweetList]
      val retorno = !(asTweetList.isEmpty)
      println("saiu Cons.equals")
      retorno
    }
    else{
      val retorno = false
      println("saiu Cons.equals")
      retorno
    }

  }
  def include(toInsert: Tweet) = {

    println("entrou Cons.include("+toInsert+")")
    if(toInsert==this.head) {
      val retorno = this
      println("saiu Cons.include("+toInsert+")")
      retorno
    }
    else if(this.tail == Nil) {
      val retorno = new Cons(head, new Cons(toInsert, Nil))
      println("saiu Cons.include("+toInsert+")")
      retorno
    }
    else {
      val retorno = new Cons(this.head, this.tail.include(toInsert))
      println("saiu Cons.include("+toInsert+")")
      retorno
    }
  }
  def remove(toRemove: Tweet) = {
    println("entrou Cons.remove("+toRemove+")")
    if(toRemove==this.head) {

      val retorno = this.tail
      println("saiu Cons.remove("+toRemove+")")
      retorno
    }
    else if(this.tail == Nil) {
      val retorno = new Cons(head, Nil)
      println("saiu Cons.remove("+toRemove+")")
      retorno
    }
    else {
      val retorno = new Cons(this.head, this.tail.remove(toRemove))
      println("saiu Cons.remove("+toRemove+")")
      retorno
    }
  }
}



object GoogleVsApple {
  val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")
  val apple = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")
  def toTweetList(toConvert:List[Tweet]): TweetList = {
    println("entrou GoogleVSApple.toTweetList")
    if(toConvert == null || toConvert == List()) {
      val retorno = Nil
      println("saiu GoogleVSApple.toTweetList")
      retorno
    }
    else {
      val retorno = new Cons(toConvert.head, toTweetList(toConvert.tail))
      println("saiu GoogleVSApple.toTweetList")
      retorno
    }
  }
  lazy val googleTweets: TweetSet = (toTweetList(List(
    new Tweet("joaodasilva", "nexus Galaxy wow", 34),
    new Tweet("mariagertrudes", "Android nexus wow", 52)))).toTweetSet
  lazy val appleTweets: TweetSet = (toTweetList(List(
    new Tweet("anadasilva", "iOS iPad wow", 51),
    new Tweet("antoniosantos", "iPhone iOS wow", 45)))).toTweetSet


  
  /**
   * A list of all tweets mentioning a keyword from either apple or google,
   * sorted by the number of retweets.
   */

     lazy val trending: TweetList = ((googleTweets.union(appleTweets)).filter(x => true ==((google ::: apple).foreach(y => (x.text).contains(y))))).descendingByRetweet
  }

object Main extends App {
  // Print the trending tweets
  GoogleVsApple.trending foreach println
}
