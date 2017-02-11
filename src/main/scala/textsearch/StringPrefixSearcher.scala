package textsearch

import scala.collection.immutable.TreeMap

class StringPrefixSearcher(words: Seq[String], maxDepth: Int = 6) {
  // This is the central data structure of the searcher. Each level (or node) corresponds to a character in a
  // string and has a list of corresponding words (if the end of the word or the structure depth is reached)
  // and a sorted map pointing to some potential following nodes (the keys being the next characters).
  // The maxDepth of the structure represents the maximum number of characters usable for research.
  private class valuesAndMap(dictionary: Seq[String], currentDepth: Int, maxDepth: Int) {
    val (words, subValuesAndMap) = maxDepth - currentDepth match {
      case 0 =>
        (dictionary, TreeMap.empty[Char, valuesAndMap])
      case _ =>
        val terminatedWords = dictionary.filter(_.length == currentDepth)
        val groupedWords = dictionary.filter(_.length != currentDepth).groupBy(_.substring(0, currentDepth + 1))
        val mapEntries = groupedWords.keySet.map { key =>
          key.last -> new valuesAndMap(groupedWords.getOrElse(key, Seq.empty), currentDepth + 1, maxDepth)
        }
        (terminatedWords, TreeMap(mapEntries.toArray:_*))
    }
  }

  private val root = new valuesAndMap(words.map(_.toLowerCase).sorted, 0, maxDepth)

  private def getNodeWithPrefix(currentNode: valuesAndMap, prefix: String): valuesAndMap = prefix.length match {
    case 0 =>
      currentNode
    case _ =>
      currentNode.subValuesAndMap.get(prefix.head) match {
        case None => new valuesAndMap(Seq.empty, 0, 0)
        case Some(next) => getNodeWithPrefix(next, prefix.tail)
      }
  }

  private def getSomeValuesFromValuesAndMap(from: valuesAndMap, count: Int): Seq[String] = count match {
    case c if c <= 0 =>
      Nil
    case _ =>
      from.subValuesAndMap.values.foldLeft(from.words.slice(0, count)) { (words, vam) =>
        words ++ getSomeValuesFromValuesAndMap(vam, count - words.size)
      }
  }

  def getSomeWordsWithPrefix(prefix: String, count: Int): Seq[String] = {
    getSomeValuesFromValuesAndMap(getNodeWithPrefix(root, prefix.toLowerCase), count)
  }
}
