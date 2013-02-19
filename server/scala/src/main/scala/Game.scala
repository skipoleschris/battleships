package com.equalexperts.battleships.server

import scalaz._
import Scalaz._

object GameModel {
  import GridModel._

  sealed trait GameStatus
  sealed trait Setup extends GameStatus
  case object NeitherPlayerReady extends Setup
  case object Player1Ready extends Setup
  case object Player2Ready extends Setup
  sealed trait Playing extends GameStatus
  case object Player1Turn extends Playing
  case object Player2Turn extends Playing
  case object Finished extends Playing

  sealed trait Player
  case object Player1 extends Player
  case object Player2 extends Player

  case class Game[T <: GameStatus](player1Grid: Grid, player2Grid: Grid)
}

trait GameActions {
  this: GridActions =>

  import GridModel._
  import GameModel._

  type ActionResult[T <: GameStatus] = Validation[ErrorMessage, Game[T]]
  type GuessStatus[T <: GameStatus] = Either[Game[Finished.type], (ProbeResult, Game[T])]
  type GuessResult[T <: GameStatus] = Validation[ErrorMessage, GuessStatus[T]]

  protected def newGame(width: Int, height: Int): ActionResult[NeitherPlayerReady.type] = 
    createGrid(width, height) map (grid => Game(grid, grid))

  protected def placeShip(player: Player, shipId: ShipId, 
                          positions: Set[Ref], game: Game[NeitherPlayerReady.type]): ActionResult[NeitherPlayerReady.type] =
    placeShipIntoGame(player, shipId, positions, game)

  protected def placeShipPlayer1(shipId: ShipId, positions: Set[Ref], 
                                 game: Game[Player2Ready.type]): ActionResult[Player2Ready.type] =
    placeShipIntoGame(Player1, shipId, positions, game)

  protected def placeShipPlayer2(shipId: ShipId, positions: Set[Ref], 
                                 game: Game[Player1Ready.type]): ActionResult[Player1Ready.type] =
    placeShipIntoGame(Player2, shipId, positions, game)

  private def placeShipIntoGame[T <: Setup](player: Player, 
                                            shipId: ShipId, 
                                            positions: Set[Ref], 
                                            game: Game[T]): ActionResult[T] = {
    val lens = if (player == Player1) player1GridL[T] else player2GridL[T]
    val updatedGrid = positions.foldLeft(lens.get(game).success[ErrorMessage]) { (acc, position) => acc flatMap (placeShipPart(position, shipId, _)) }
    updatedGrid map { grid => lens.set(game, grid) }
  } 


  
  protected def player1Ready(game: Game[NeitherPlayerReady.type]) = changeState[Player1Ready.type](game)
  protected def player2Ready(game: Game[NeitherPlayerReady.type]) = changeState[Player2Ready.type](game)
  protected def player1AlsoReady(game: Game[Player2Ready.type]) = changeState[Player1Turn.type](game)
  protected def player2AlsoReady(game: Game[Player1Ready.type]) = changeState[Player1Turn.type](game)



  protected def player1Guess(position: Ref, game: Game[Player1Turn.type]): GuessResult[Player2Turn.type] = 
    guess(Player1, position, game)

  protected def player2Guess(position: Ref, game: Game[Player2Turn.type]): GuessResult[Player1Turn.type] =
    guess(Player2, position, game)

  private def guess[T1 <: Playing, T2 <: Playing](player: Player,
                                                  position: Ref,
                                                  game: Game[T1]): GuessResult[T2] = {
    val lens = if (player == Player1) player2GridL[T1] else player1GridL[T1]
    probe(position, lens.get(game)) map { result =>
      val (probeResult, grid) = result
      val updatedGame = lens.set(game, grid)
      if ( isDefeated(grid) ) Left(changeState[Finished.type](updatedGame))
      else Right((probeResult, changeState[T2](updatedGame)))
    } 
  }


  private def changeState[T <: GameStatus](game: Game[_]) = Game[T](game.player1Grid, game.player2Grid)
  private def player1GridL[T <: GameStatus]: Lens[Game[T], Grid] = Lens.lensu((game, newGrid) => game.copy(player1Grid = newGrid), _.player1Grid)
  private def player2GridL[T <: GameStatus]: Lens[Game[T], Grid] = Lens.lensu((game, newGrid) => game.copy(player2Grid = newGrid), _.player2Grid)
}
