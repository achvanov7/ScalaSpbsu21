package org.spbsu.mkn.scala

import java.lang.invoke.WrongMethodTypeException
import java.nio.channels.ReadPendingException
import java.security.KeyStore.TrustedCertificateEntry
import scala.io.StdIn.readLine
import scala.util.Random

object TheGame {

  sealed trait GuessResult

  case class Correct(numTries: Int) extends GuessResult

  case class Incorrect(bulls: Int, cows: Int) extends GuessResult

  class RepeatingDigitsException extends RuntimeException

  class WrongNumberLengthException(expected: Int, got: Int) extends RuntimeException

  def generateNumberString(length: Int): String = {
    var NumbersAndLetters = ""
    for (i <- '0' to '9') NumbersAndLetters += i
    for (i <- 'A' to 'Z') NumbersAndLetters += i
    Random.shuffle(NumbersAndLetters).take(length).toString()
  }

  def isUnique(str: String): Boolean = str.length == str.toSet.size

  def validate(secret: String, userInput: String, numTries: Int = 1): GuessResult = {
    if (userInput.length != secret.length)
      throw new WrongNumberLengthException(secret.length, userInput.length)
    if (!isUnique(userInput))
      throw new RepeatingDigitsException
    if (secret == userInput)
      return Correct(numTries)
    var cows: Int = 0
    var bulls: Int = 0
    for (i <- 0 until userInput.length) {
      if (secret(i) == userInput(i))
        bulls += 1
      else if (secret.contains(userInput(i)))
        cows += 1
    }
    Incorrect(bulls, cows)
  }

  def main(args: Array[String]): Unit = {
    print("Enter your name: ")
    val name = readLine()
    println(s"Hello, $name!")

    val lengthOfString = Random.nextInt(6) + 1
    val guessedString = generateNumberString(lengthOfString)
    println(s"The string of length $lengthOfString has been guessed.")
    var guessResult: GuessResult = Incorrect(0, 0)
    var numTries = 0
    while (guessResult.isInstanceOf[Incorrect]) {
      print("Enter your next guess: ")
      val guess = readLine()
      guessResult = validate(guessedString, guess, numTries)
      numTries += 1
      guessResult match {
        case x: Incorrect => {
          val bulls = x.bulls
          val cows = x.cows
          println(s"Your guess is incorrect! Bulls: $bulls, Cows: $cows")
        }
        case _ => {}
      }
    }
    println(s"Correct! You've guessed the string in $numTries tries.")
  }
}
