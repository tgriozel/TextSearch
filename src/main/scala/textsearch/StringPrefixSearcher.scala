package textsearch

import scala.collection.immutable.{SortedSet, TreeMap}

class StringPrefixSearcher(words: Set[String], maxDepth: Int = 6) {
  // This is the central data structure of the searcher. Each level (or node) corresponds to a character in a
  // string and has a list of corresponding words (if the end of the word or the structure depth is reached)
  // and a sorted map pointing to some potential following nodes (the keys being the next characters).
  // The maxDepth of the structure represents the maximum number of characters usable for research.
  private class valuesAndMap(dictionary: Set[String], currentDepth: Int, maxDepth: Int) {
    val (words, subValuesAndMap) = maxDepth - currentDepth match {
      case 0 =>
        (SortedSet(dictionary.toList:_*), TreeMap.empty[Char, valuesAndMap])
      case _ =>
        val terminatedWords = dictionary.filter(_.length == currentDepth)
        val groupedWords = dictionary.filter(_.length != currentDepth).groupBy(_.substring(0, currentDepth + 1))
        val mapEntries = groupedWords.keySet.map { key =>
          key.last -> new valuesAndMap(groupedWords.getOrElse(key, Set.empty), currentDepth + 1, maxDepth)
        }
        (SortedSet(terminatedWords.toList:_*), TreeMap(mapEntries.toList:_*))
    }
  }

  private val root = new valuesAndMap(words.map(_.toLowerCase), 0, maxDepth)

  private def getNodeWithPrefix(currentNode: valuesAndMap, prefix: String): valuesAndMap = prefix.length match {
    case 0 =>
      currentNode
    case _ =>
      currentNode.subValuesAndMap.get(prefix.head) match {
        case None => new valuesAndMap(Set.empty, 0, 0)
        case Some(next) => getNodeWithPrefix(next, prefix.tail)
      }
  }

  private def getSomeValuesFromValuesAndMap(from: valuesAndMap, count: Int): Set[String] = count match {
    case c if c <= 0 =>
      Set.empty
    case _ =>
      from.subValuesAndMap.values.foldLeft(from.words.slice(0, count)) { (words, vam) =>
        words ++ getSomeValuesFromValuesAndMap(vam, count - words.size)
      }
  }

  def getSomeWordsWithPrefix(prefix: String, count: Int = 4): Set[String] = {
    getSomeValuesFromValuesAndMap(getNodeWithPrefix(root, prefix.toLowerCase), count)
  }
}
