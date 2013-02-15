package com.equalexperts.battleships

import scalaz._
import Scalaz._

package object server {

  private[server] def indexedItemReplacementL[A](index: Int): Lens[Seq[A], A] = Lens.lensu((sequence, newItem) => replaceAt(index, sequence, newItem), _.apply(index))

  private def replaceAt[A](index: Int, v: Seq[A], newValue: A): Seq[A] = {
    val (before, after) = v splitAt index
    (before :+ newValue) ++ (after drop 1)
  }

}