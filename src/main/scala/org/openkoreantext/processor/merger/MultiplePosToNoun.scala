package org.openkoreantext.processor.merger

import org.openkoreantext.processor.tokenizer.KoreanTokenizer.KoreanToken
import org.openkoreantext.processor.util.{CharArraySet, KoreanDictionaryProvider, KoreanPos}
import org.openkoreantext.processor.util.KoreanPos.Noun

import scala.collection.mutable

object MultiplePosToNoun {
  def merge(tokens: Seq[KoreanToken]): Seq[KoreanToken] = {
    val nouns = KoreanDictionaryProvider.koreanDictionary.get(Noun)
    val maxWords = 4
    val words = mutable.ListBuffer.empty[KoreanToken]
    val mergedWords = tokens.flatMap { token =>
      if (token.pos == KoreanPos.Space) {
        mergeMultipleWords(words, nouns, noMore = true) ++ Seq(token)
      } else {
        words += token
        if (words.length > maxWords) {
          mergeMultipleWords(words, nouns)
        } else Seq.empty
      }
    }

    mergedWords ++ mergeMultipleWords(words, nouns, noMore = true)
  }

  private def mergeMultipleWords(words: mutable.ListBuffer[KoreanToken], nouns: CharArraySet, noMore: Boolean = false): Seq[KoreanToken] = {
    if (words.isEmpty)
      return Seq.empty

    for (l <- words.length to 2 by -1) {
      val merged = words.slice(0, l).map(_.text).mkString("")
      if (nouns.contains(merged.toCharArray)) {
        val mergedNoun = KoreanToken(merged, Noun, words.head.offset, merged.length, Option.empty)
        words.remove(0, l)

        if (noMore)
          return mergedNoun +: mergeMultipleWords(words, nouns, noMore = true)
        else
          return Seq(mergedNoun)
      }
    }

    if (noMore)
      Seq(words.remove(0)) ++ mergeMultipleWords(words, nouns, noMore = true)
    else Seq(words.remove(0))
  }

}
