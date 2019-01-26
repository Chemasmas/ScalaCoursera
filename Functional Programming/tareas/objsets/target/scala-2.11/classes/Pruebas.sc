import objsets.Empty
import objsets.NonEmpty
import objsets.TweetSet
import objsets.Tweet

def asSet(tweets: TweetSet): Set[Tweet] = {
  var res = Set[Tweet]()
  tweets.foreach(res += _)
  res
}
def size(set: TweetSet): Int = asSet(set).size


val set1 = new Empty
/*
val set2 = set1.incl(new Tweet("b", "a body", 20))
val set3 = set2.incl(new Tweet("a", "b body", 20))
val c = new Tweet("c", "c body", 7)
val d = new Tweet("d", "d body", 9)
val set4c = set3.incl(c)
val set4d = set3.incl(d)
val set5 = set4c.incl(d)
*/
//size(set5.filter(tw => tw.user == "b"))
//size(set5.filter(tw => tw.retweets == 20))
//set5.filter(tw => tw.user == "a")
//set5.contains( new Tweet("a", "a body", 20)  )
//set5.contains( new Tweet("b", "a body", 20)  )
//set5.contains( new Tweet("c", "a bodi", 20)  )