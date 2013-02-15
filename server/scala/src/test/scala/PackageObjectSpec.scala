package com.equalexperts.battleships.server

import org.specs2.Specification
import org.specs2.ScalaCheck
import org.scalacheck._

class PackageObjectSpec extends Specification with ScalaCheck { def is =

  "Specification for the Server package object"                       ^
                                                                      endp^
  "The indexed item replacement lens should"                          ^
    "given a valid index into the sequence"                           ^
      "replace the item at the given index"                           ! replaceChangesItem^
      "keep the length of the sequence the same"                      ! replaceMaintainsLength^
      "not change any of the preceeding items"                        ! replaceDoesNotChangePreceeding^
      "not change any of the succeeding items"                        ! replaceDoesNotChangeSucceeding^
                                                                      p^
    "given an invalid index into the sequence"                        ^  
      "report an error if the sequence is empty"                      ! replaceInEmptySequence^
      "report an error if index is before the start of sequence"      ! replaceBeforeStartOfSequence^
      "report an error if index is after the end of sequence"         ! replaceAfterEndOfSequence^
                                                                      end

  import Prop.forAll
  import Arbitrary.arbitrary

  private val indexes = Gen.choose(0, 50)
  private val intList = Gen.containerOf[List, Int](arbitrary[Int])

  def replaceChangesItem = forAll(intList, indexes) { (items: List[Int], index: Int) => items.length > 0 ==> {
    val (result, actualIndex) = replaceAtIndex(items, index)
    result(actualIndex) must_== 42
  }}

  def replaceMaintainsLength = forAll(intList, indexes)  { (items: List[Int], index: Int) => items.length > 0 ==> {
    val (result, actualIndex) = replaceAtIndex(items, index)
    result must haveLength(items.length)
  }}

  def replaceDoesNotChangePreceeding = forAll(intList, indexes)  { (items: List[Int], index: Int) => items.length > 0 ==> {
    val (result, actualIndex) = replaceAtIndex(items, index)
    result.startsWith(items take actualIndex)
  }}

  def replaceDoesNotChangeSucceeding = forAll(intList, indexes) { (items: List[Int], index: Int) => items.length > 0 ==> {
    val (result, actualIndex) = replaceAtIndex(items, index)
    result.endsWith(items drop (actualIndex + 1))
  }}

  private def replaceAtIndex(items: List[Int], index: Int) = {
    val actualIndex = if (index < 0 ) 0 else if (index >= items.length) items.length - 1 else index
    (indexedItemReplacementL[Int](actualIndex).mod(_ => 42, items), actualIndex)
  }

  def replaceInEmptySequence = {
    indexedItemReplacementL[Int](0).mod(_ => 42, Nil) must throwAn[IndexOutOfBoundsException]
  }

  def replaceBeforeStartOfSequence = {
    indexedItemReplacementL[Int](-1).mod(_ => 42, List(42)) must throwAn[IndexOutOfBoundsException]
  }

  def replaceAfterEndOfSequence = {
    indexedItemReplacementL[Int](1).mod(_ => 42, List(42)) must throwAn[IndexOutOfBoundsException]
  }
}