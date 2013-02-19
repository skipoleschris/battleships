package com.equalexperts.battleships.server

import org.scalacheck._

trait Generators {
  import Arbitrary.arbitrary
  import GridModel._
  import GameModel._

  protected val widths = Gen.choose(1, 25)
  protected val heights = Gen.choose(1, 25)

  protected val widths1 = Gen.choose(1, 250)
  protected val heights1 = Gen.choose(1, 250)

  protected val widths2 = Gen.choose(1, 250)
  protected val heights2 = Gen.choose(1, 250)

  protected val widths3 = Gen.choose(1, 250)
  protected val heights3 = Gen.choose(1, 250)


  protected val positions = Gen.oneOf(allRefs(20, 20))
  protected val nonEdgePositions = Gen.oneOf(allNonEdgeRefs(20, 20))
  protected val shipIds = arbitrary[ShipId]
  protected val players = Gen.oneOf(Player1, Player2)

  private def allRefs(width: Int, height: Int) = for {
    column <- 1 to width
    row <- 1 to height
  } yield Ref(column, row)

  private def allNonEdgeRefs(width: Int, height: Int) = for {
    column <- 2 until width 
    row <- 2 until height
  } yield Ref(column, row)
}
