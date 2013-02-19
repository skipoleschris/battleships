package com.equalexperts.battleships.server

import org.specs2.Specification
import org.specs2.matcher.MatchResult
import scalaz.Validation
import GridModel.ErrorMessage

trait FailureHandling {
  this: Specification =>
  
  protected def handleFailureOf(f: => scalaz.Validation[ErrorMessage, MatchResult[Any]]): MatchResult[Any] = f fold (failWithMessage, identity)
  protected def failWithMessage(message: ErrorMessage) = message must_== ""
  protected def failWithException(message: ErrorMessage) = throw new IllegalStateException(message)

}
