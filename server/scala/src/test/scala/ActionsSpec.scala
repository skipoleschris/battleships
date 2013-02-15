package com.equalexperts.battleships.server

import org.specs2.Specification
import org.specs2.ScalaCheck
import org.specs2.matcher.MatchResult
import org.scalacheck._

class ActionsSpec extends Specification with ScalaCheck with Actions { def is =

  "Specification for the Actions that manipulate a Battleships grid"  ^
                                                                      endp^
  "Creating a new grid should"                                        ^
    "yield a grid of the specified width"                             ! createGridOfSpecifiedWidth^                                                                      
    "yield a grid of the specified height"                            ! createGridOfSpecifiedHeight^              
    "yield a grid with all empty and unprobed cells"                  ! createGridOfEmptyUnprobedCells^    
    "error if trying to create a grid less than width 1"              ! invalidWidthCreateGrid^                                  
    "error if trying to create a grid less than height 1"             ! invalidHeightCreateGrid^                                  
                                                                      endp^
  "Placing ship parts into a grid should"                             ^
    "update the specified position on the grid with the ship name"    ! placeShipPartUpdatesCell^           
    "not modify any cells in the grid other than the one specified"   ! placeShipPartDoesNotChangeOtherCells^     
    "error if trying to place a ship part outside the grid"           ! invalidReferencePlaceShipPart^                                                      
                                                                      endp^
  "Probing a location in a grid should"                               ^
    "return a new grid with the position marked as probed"            ! probeMarksCellAsProbed^
    "not modify any cells in the grid other than the one probed"      ! probeDoesNotChangeOtherCells^
    "report a miss if the location does not contain a ship"           ! probeEmptyPositionReportsMiss^
    "report a hit if the location contains part of a ship"            ! probeReportsHitIfShipPresent^
    "report the name of the ship that has been fully sunk"            ! probeReportsShipHasBeenSunk^
    "error if probing an already probed location"                     ! duplicateReferenceProbe^
    "error if trying to probe outside the grid"                       ! invalidReferenceProbe^                                                      
                                                                      end

  import Prop.forAll
  import Arbitrary.arbitrary
  import Domain._

  private val widths1 = Gen.choose(1, 250)
  private val heights1 = Gen.choose(1, 250)

  def createGridOfSpecifiedWidth = forAll(widths1, heights1) { (width: Int, height: Int) => handleFailureOf {
    createGrid(width, height) map { grid =>
      (grid.width must_== width) and (grid.columns.size must_== width)
    }
  }}

  private val widths2 = Gen.choose(1, 250)
  private val heights2 = Gen.choose(1, 250)

  def createGridOfSpecifiedHeight = forAll(widths2, heights2) { (width: Int, height: Int) => handleFailureOf {
    createGrid(width, height) map { grid =>
      (grid.height must_== height) and ((grid.columns forall (_.size == height)) must beTrue)
    }
  }}

  private val widths3 = Gen.choose(1, 250)
  private val heights3 = Gen.choose(1, 250)

  def createGridOfEmptyUnprobedCells = forAll(widths3, heights3) { (width: Int, height: Int) => handleFailureOf {
    createGrid(width, height) map { grid =>
      grid.columns forall (_ forall (_ == Cell(Empty, Unprobed))) must beTrue
    }
  }}

  def invalidWidthCreateGrid = createGrid(0, 42) must_== scalaz.Failure("The grid must be at least 1 x 1 in size")

  def invalidHeightCreateGrid = createGrid(42, 0) must_== scalaz.Failure("The grid must be at least 1 x 1 in size")


  private val startingGrid = createGrid(20, 20) fold (failWithException, identity)
  private val positions = Gen.oneOf(allRefs(20, 20))
  private val shipNames = arbitrary[String]

  def placeShipPartUpdatesCell = forAll(positions, shipNames) { (position: Ref, shipName: String) => handleFailureOf {
    placeShipPart(position, shipName, startingGrid) map { grid => 
      grid.columns(position.columnIndex).apply(position.rowIndex) must_== Cell(Occupied(shipName), Unprobed)
    }
  }}

  def placeShipPartDoesNotChangeOtherCells = forAll(positions) { (position: Ref) => handleFailureOf {
    placeShipPart(position, "USS Enterprise", startingGrid) map { grid => 
      grid.columns.flatten count (_ != Cell(Empty, Unprobed)) must_== 1
    }
  }}
  
  def invalidReferencePlaceShipPart = check { (column: Int, row: Int) => (column < 1 || column > 20 || row < 1 || row > 20) ==> {
    placeShipPart(Ref(column, row), "USS Enterprise", startingGrid) must_== scalaz.Failure("A position reference must be between (1 and 20, 1 and 20)")
  }}




  def probeMarksCellAsProbed = forAll(positions) { (position: Ref) => handleFailureOf {
    probe(position, startingGrid) map { probeResult =>
      probeResult._2.columns(position.columnIndex).apply(position.rowIndex) must_== Cell(Empty, Miss) 
    }
  }}

  def probeDoesNotChangeOtherCells = forAll(positions) { (position: Ref) => handleFailureOf {
    probe(position, startingGrid) map { probeResult =>
      probeResult._2.columns.flatten count (_ != Cell(Empty, Unprobed)) must_== 1
    }
  }}

  def probeEmptyPositionReportsMiss = forAll(positions) { (position: Ref) => handleFailureOf {
    probe(position, startingGrid) map { probeResult =>
      probeResult._1 must_== ProbeResult(Miss, None)
    }
  }}

  def probeReportsHitIfShipPresent = forAll(positions) { (position: Ref) => handleFailureOf {
    val adjacentPosition = position.copy(column = if (position.column == 20) 19 else 20) 
    for {
      placedOne <- placeShipPart(position, "USS Enterprise", startingGrid)
      placedTwo <- placeShipPart(adjacentPosition, "USS Enterprise", placedOne)
      probeResult <- probe(position, placedTwo)
    } yield probeResult._1 must_== ProbeResult(Hit, None)
  }}

  def probeReportsShipHasBeenSunk = forAll(positions) { (position: Ref) => handleFailureOf {
    val adjacentPosition = position.copy(column = if (position.column == 20) 19 else 20) 
    for {
      placed <- placeShipPart(position, "USS Enterprise", startingGrid)
      probeResult <- probe(position, placed)
    } yield probeResult._1 must_== ProbeResult(Hit, Some("USS Enterprise"))
  }}

  def duplicateReferenceProbe = forAll(positions) { (position: Ref) => handleFailureOf {
    probe(position, startingGrid) map { probeResult =>
      probe(position, probeResult._2) must_== scalaz.Failure(s"The location (${position.column}, ${position.row}) has already been probed")
    }
  }}

  def invalidReferenceProbe = check { (column: Int, row: Int) => (column < 1 || column > 20 || row < 1 || row > 20) ==> {
    probe(Ref(column, row), startingGrid) must_== scalaz.Failure("A position reference must be between (1 and 20, 1 and 20)")
  }}



  private def allRefs(width: Int, height: Int) = for {
    column <- 1 to width
    row <- 1 to height
  } yield Ref(column, row)

  private def handleFailureOf(f: => scalaz.Validation[ErrorMessage, MatchResult[Any]]): MatchResult[Any] = f fold (failWithMessage, identity)
  private def failWithMessage(message: ErrorMessage) = message must_== ""
  private def failWithException(message: ErrorMessage) = throw new IllegalStateException(message)
}
