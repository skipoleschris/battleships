package com.equalexperts.battleships.server

object Domain {

	type ShipId = String

	sealed trait CellContent
	case object Empty extends CellContent
	case class Occupied(shipId: ShipId) extends CellContent

	sealed trait ProbeStatus
	case object Unprobed extends ProbeStatus
	case object Miss extends ProbeStatus
	case object Hit extends ProbeStatus

	case class Cell(content: CellContent, status: ProbeStatus) {
    def isAfloat = content != Empty && status == Unprobed
  }

	type Column = Seq[Cell]

	case class Grid(width: Int, height: Int, columns: Seq[Column])

  case class Ref(column: Int, row: Int) {
    def columnIndex = column - 1
    def rowIndex = row - 1
  }

  type ErrorMessage = String

  case class ProbeResult(status: ProbeStatus, sunk: Option[ShipId] = None)
}
