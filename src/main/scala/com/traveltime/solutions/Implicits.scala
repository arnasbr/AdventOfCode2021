package com.traveltime.solutions

object Implicits {
  implicit class OptionTraversable[A](input: List[Option[A]]) {
    def traverseOptions: Option[List[A]] =
      input.foldRight[Option[List[A]]](Some(Nil))((oa, acc) =>
        for {
          a <- oa
          list <- acc
        } yield a :: list
      )
  }
}
