package com.equalexperts.battleships.server

object Battleships {

	type ShipId = String

	sealed trait CellContent
	case object Empty extends CellContent
	case class Occupied(shipId: ShipId) extends CellContent

	sealed trait ProbeStatus
	case object Unprobed extends ProbeStatus
	case object Miss extends ProbeStatus
	case object Hit extends ProbeStatus

	case class Cell(content: CellContent, status: ProbeStatus)

	type Column = Seq[Cell]

	case class Grid(width: Int, height: Int, columns: Seq[Column])

  case class Ref(column: Int, row: Int) {
    def columnIndex = column - 1
    def rowIndex = row - 1
  }

  type ErrorMessage = String

  import scalaz._
  import Scalaz._

  def createGrid(width: Int, height: Int): Validation[ErrorMessage, Grid] = {
    if (width <= 1 || height <= 1) "The grid must be at least 1 x 1 in size".fail
    else {
      val columns = for (col <- 0 until width) yield {
        (for (row <- 0 until height) yield Cell(Empty, Unprobed))
      } 

      Grid(width, height, columns).success
    }
  }

  def placeShipPart(at: Ref, shipId: ShipId, grid: Grid): Validation[ErrorMessage, Grid] = againstValidRef(at, grid) {
    val lens = columnsL andThen indexedItemReplacementL(at.columnIndex) andThen indexedItemReplacementL(at.rowIndex) andThen contentL    
    lens.mod(_ => Occupied(shipId), grid).success
  }


  case class ProbeResult(status: ProbeStatus, sunk: Option[ShipId] = None)

  def probe(at: Ref, grid: Grid): Validation[ErrorMessage, (ProbeResult, Grid)] = againstValidRef(at, grid) {
    val statusLens = columnsL andThen indexedItemReplacementL(at.columnIndex) andThen indexedItemReplacementL(at.rowIndex) andThen statusL
    val contentLens = columnsL andThen indexedItemReplacementL(at.columnIndex) andThen indexedItemReplacementL(at.rowIndex) andThen contentL    

    if (statusLens.get(grid) != Unprobed) "The location (${at.column}, ${at.row}) has already been probed".fail
    else {
      contentLens.get(grid) match {
        case Occupied(shipId) => 
          val newGrid = statusLens.mod(_ => Hit, grid)
          (ProbeResult(Hit, if (isSunk(shipId, newGrid)) Some(shipId) else None), newGrid).success
        case _ => (ProbeResult(Miss), statusLens.mod(_ => Miss, grid)).success
      }
    }
  }
 
  def isSunk(shipId: ShipId, grid: Grid): Boolean = {
    (for {
      column <- grid.columns
      cell <- column if ( cell.content == Occupied(shipId) && cell.status != Hit)
    } yield cell).isEmpty
  }

  def againstValidRef[A](ref: Ref, grid: Grid)(f: => Validation[ErrorMessage, A]): Validation[ErrorMessage, A] = {
    if (isValidRef(ref, grid)) s"A position reference must be between (1 and ${grid.width}, 1 and ${grid.height}".fail
    else f
  }

  def isValidRef(ref: Ref, grid: Grid): Boolean =
    (ref.column > 0 && ref.column <= grid.width && ref.row > 0 && ref.row <= grid.height)


	val contentL: Lens[Cell, CellContent] = Lens.lensu((cell, newContent) => cell.copy(content = newContent), _.content)
	val statusL: Lens[Cell, ProbeStatus] = Lens.lensu((cell, newStatus) => cell.copy(status = newStatus), _.status)
  val columnsL: Lens[Grid, Seq[Column]] = Lens.lensu((grid, newColumns) => grid.copy(columns = newColumns), _.columns)

  def indexedItemReplacementL[A](index: Int): Lens[Seq[A], A] = Lens.lensu((sequence, newItem) => insertAt(index, sequence, newItem), _.apply(index))

  def insertAt[A](index: Int, v: Seq[A], newValue: A): Seq[A] = {
    val (before, after) = v splitAt index
    (before :+ newValue) ++ (after drop 1)
  }
}
