package com.equalexperts.battleships.server

import org.specs2.Specification
import org.specs2.ScalaCheck
import org.specs2.matcher.MatchResult
import org.scalacheck._

class GameSpec extends Specification with ScalaCheck with GameActions with GridActions with Generators with FailureHandling { def is =

  "Specification for the Actions that manipulate a Battleships game"  ^
                                                                      endp^
  "Creating a new game should"                                        ^
    "have both players grids be of equal size"                        ! newGameCreatesEqualSizeGrids^ 
    "start with player 1's grid being empty"                          ! newGameEmptyPlayer1Grid^                                                                   
    "start with player 2's grid being empty"                          ! newGameEmptyPlayer2Grid^  
                                                                      endp^
  "Placing pieces onto the game should"                               ^
    "add all the ship positions to the player's grid"                 ! placeShipInPlayersGrid^
    "not modify the other player's grid when placing a ship"          ! placeShipDoesNotChangeOtherPlayersGrid^
    "allow player 2 to add ships once player 1 is ready"              ! placeShipWhenPlayer1Ready^
    "allow player 1 to add ships once player 2 is ready"              ! placeShipWhenPlayer2Ready^
                                                                      endp^
  "Both players becoming ready should"                                ^
    "put the game into a playing state"                               ! bothPlayersReady^                                                                                                
                                                                      end

  import Prop.forAll
  import GridModel._
  import GameModel._

  def newGameCreatesEqualSizeGrids = forAll(widths, heights) { (width: Int, height: Int) => handleFailureOf {
    newGame(width, height) map { game =>
      (game.player1Grid.width must_== game.player2Grid.width) and (game.player1Grid.height must_== game.player2Grid.height)
    }
  }}

  def newGameEmptyPlayer1Grid = forAll(widths, heights)(newGameEmptyGrid(_.player1Grid))

  def newGameEmptyPlayer2Grid = forAll(widths, heights)(newGameEmptyGrid(_.player2Grid))

  private def newGameEmptyGrid(gridFrom: Game[_] => Grid)(width: Int, height: Int) = handleFailureOf {
    newGame(width, height) map { game =>
      gridFrom(game).columns.flatten forall (_ == EmptyCell) must beTrue
    }
  }



  private val startingGame = newGame(20, 20) fold (failWithException, identity)

  def placeShipInPlayersGrid = forAll(nonEdgePositions, shipIds, players) { (position: Ref, shipId: ShipId, player: Player) => handleFailureOf {
    val shipPositions = aSquareAround(position)
    placeShip(player, shipId, shipPositions, startingGame) map { game =>
      shipOnlyAtPositions(shipPositions, (if (player == Player1) game.player1Grid else game.player2Grid))
    }
  }}

  def placeShipDoesNotChangeOtherPlayersGrid = forAll(nonEdgePositions, shipIds, players) { (position: Ref, shipId: ShipId, player: Player) => handleFailureOf {
    val shipPositions = aSquareAround(position)
    placeShip(player, shipId, shipPositions, startingGame) map { game =>
      allCellsEmpty(if (player == Player1) game.player2Grid else game.player1Grid)
    }
  }}

  def placeShipWhenPlayer1Ready = forAll(nonEdgePositions, shipIds) { (position: Ref, shipId: ShipId) => handleFailureOf {
    val shipPositions = aSquareAround(position)
    placeShipPlayer2(shipId, shipPositions, player1Ready(startingGame)) map { game =>
      shipOnlyAtPositions(shipPositions, game.player2Grid) and allCellsEmpty(game.player1Grid)
    }
  }}

  def placeShipWhenPlayer2Ready = forAll(nonEdgePositions, shipIds) { (position: Ref, shipId: ShipId) => handleFailureOf {
    val shipPositions = aSquareAround(position)
    placeShipPlayer1(shipId, shipPositions, player2Ready(startingGame)) map { game =>
      shipOnlyAtPositions(shipPositions, game.player1Grid) and allCellsEmpty(game.player2Grid)
    }
  }}

  private def shipOnlyAtPositions(positions: Set[Ref], grid: Grid) = {
    (positions forall (p => (probe(p, grid) map (_._1.status)) == scalaz.Success(Hit)) must beTrue) and
    (grid.columns.flatten filter (_ == EmptyCell) must haveSize(20 * 20 - 9))
  }

  private def allCellsEmpty(grid: Grid) = grid.columns.flatten forall (_ == EmptyCell) must beTrue



  private def aSquareAround(position: Ref) = Set(
    Ref(position.column - 1, position.row - 1),
    Ref(position.column,     position.row - 1),
    Ref(position.column + 1, position.row - 1),
    Ref(position.column - 1, position.row),
    position,
    Ref(position.column + 1, position.row),
    Ref(position.column - 1, position.row + 1),
    Ref(position.column,     position.row + 1),
    Ref(position.column + 1, position.row + 1)
  )



  def bothPlayersReady = {
    val playing1: Game[Player1Turn.type] = player2AlsoReady(player1Ready(startingGame))
    val playing2: Game[Player1Turn.type] = player1AlsoReady(player2Ready(startingGame))
    playing1 must_== playing2
  }
}
