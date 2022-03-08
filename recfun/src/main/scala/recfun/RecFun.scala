package recfun

import scala.annotation.tailrec

object RecFun extends RecFunInterface:

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do
        print(s"${pascal(col, row)} ")
      println()

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = (c, r) match
    case (_, 0) => 1
    case (0, _) => 1
    case (`r`, _) => 1
    case (c, r) => pascal(c-1, r-1) + pascal(c, r-1)


  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean =
    balanceInner(List.empty[Char], chars)

  @tailrec
  def balanceInner(acc: List[Char], chars: List[Char]): Boolean = (acc, chars) match
    case (_, List()) => acc.isEmpty
    case _ =>
      chars.head match
        case '(' => balanceInner('(' :: acc , chars.drop(1))
        case ')' => balanceInner(if (!acc.isEmpty && acc.head == '(') acc.drop(1) else ')' :: acc, chars.drop(1))
        case _ => balanceInner(acc , chars.drop(1))


  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int =
    (money, coins) match
      case (0, _) => 0
      case (_, List()) => 0
      case _ => countChangeInner(money, coins.sortWith(_ > _))

  def countChangeInner(money: Int, coins: List[Int]): Int =
    (money, coins) match
      case (0, _) => 1
      case (money, _) if money < 0 => 0
      case (_, List()) => 0
      case _ => countChangeInner(money - coins.head, coins) + countChangeInner(money, coins.drop(1))
