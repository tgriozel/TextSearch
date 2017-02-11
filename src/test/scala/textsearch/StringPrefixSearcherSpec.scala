package textsearch

import org.specs2.mutable.Specification

class StringPrefixSearcherSpec extends Specification {
  val dictionary = Seq("Pandora", "Pinterest", "Paypal", "Pg&e", "Project free tv", "Press democrat", "Progressive",
    "Project runway", "Proactive", "Programming", "Progeria", "Progesterone", "Progenex", "Procurable", "Processor",
    "Proud", "Print", "Prank", "Bowl", "Owl", "River", "Phone", "Kayak", "Stamps", "Reprobe")

  val searcher = new StringPrefixSearcher(dictionary)

  "The string prefix searcher" should {
    "Return correct results" in {
      searcher.getSomeWordsWithPrefix("p", 4) mustEqual Seq("pandora", "paypal", "pg&e", "phone")
      searcher.getSomeWordsWithPrefix("pr", 4) mustEqual Seq("prank", "press democrat", "print", "proactive")
      searcher.getSomeWordsWithPrefix("prog", 4) mustEqual Seq("progenex", "progeria", "progesterone", "programming")
    }

    "Return only the required amount of suggestions" in {
      searcher.getSomeWordsWithPrefix("p", 2) mustEqual Seq("pandora", "paypal")
    }

    "Return the same results with a less complex structure, for a simple enough research" in {
      val searcherTwo = new StringPrefixSearcher(dictionary, maxDepth = 2)
      searcherTwo.getSomeWordsWithPrefix("p", 4) mustEqual Seq("pandora", "paypal", "pg&e", "phone")
    }
  }
}
