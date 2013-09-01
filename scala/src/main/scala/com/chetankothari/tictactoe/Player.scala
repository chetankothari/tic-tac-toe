/*
  author: Chetan Kothari
  email: mail@chetankothari.in
  date: 01/09/2013
*/

package com.chetankothari.tictactoe

sealed abstract class Player(marker: String) {
  override def toString = marker
}

case class O(marker: String = "O") extends Player(marker)

case class X(marker: String = "X") extends Player(marker)

case class Empty(marker: String = " ") extends Player(marker)

