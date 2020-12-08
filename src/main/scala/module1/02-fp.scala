package module1

import module1.list.List.{::, Cons, Nil}

import scala.annotation.tailrec

/**
 * Реализуем тип Option
 */


object opt {

  /**
   *
   * Реализовать тип Option, который будет указывать на присутствие либо отсутсвие результата
   */

  sealed trait Option[+A] {

    import Option.{Some, None}

    def isEmpty: Boolean = this match {
      case Some(_) => false
      case None => true
    }

    def get[A]: A = this match {
      case Some(v: A) => v
      case None => throw new Exception("get on empty Option")
    }

    /**
     *
     * Реализовать метод printIfAny, который будет печатать значение, если оно есть
     */
    def printIfAny: Unit = this match {
      case Some(v) => println(v)
      case None => println(None)
    }

    /**
     *
     * реализовать метод orElse который будет возвращать другой Option, если данный пустой
     */
    def orElse[A](altV: A): A = this match {
      case Some(v: A) => v
      case None => altV
    }

    /**
     *
     * Реализовать метод zip, который будет создавать Option от пары значений из 2-х Option
     */
    def zip[A](optV: Option[A]): Option[(A, A)] = this match {
      case None => None
      case Some(v: A) => {
        optV match {
          case Some(optV: A) => Option[(A, A)]((v, optV))
          case None => None
        }
      }
    }

    /**
     *
     * Реализовать метод filter, который будет возвращать не пустой Option
     * в случае если исходный не пуст и предикат от значения = true
     */
    def filter(pred: A => Boolean): Option[A] = {
      // тут сделаю без pattern matching
      if (this.isEmpty) None
      else if (pred(get)) this else None
    }

  }

  object Option {

    case class Some[A](v: A) extends Option[A]

    case object None extends Option[Nothing]

    def apply[A](v: A): Option[A] = v match {
      case None => None
      case _ => Some(v)
    }

  }

}

object recursion {

  /**
   * Реализовать метод вычисления n!
   * n! = 1 * 2 * ... n
   */

  def fact(n: Int): Long = {
    var _n = 1L
    var i = 2
    while (i <= n) {
      _n *= i
      i += 1
    }
    _n
  }

  def !!(n: Int): Long = {
    if (n <= 1) 1
    else n * !!(n - 1)
  }

  def !(n: Int): Long = {
    @tailrec
    def loop(n1: Int, acc: Long): Long = {
      if (n <= 1) acc
      else loop(n1 - 1, n1 * acc)
    }

    loop(n, 1)
  }

}

object list {

  /**
   *
   * Реализовать односвязанный имутабельный список List
   */

  sealed trait List[+A] {

    import List._

    def ::[AA >: A](head: AA): List[AA] = Cons(head, this)

    def mkString: String = mkString(", ")

    def mkString(sep: String): String = {

      def loop(l: List[A], acc: StringBuilder): StringBuilder = {
        l match {
          case List.Nil => acc
          case h :: Nil => acc.append(s"$h")
          case h :: t => loop(t, acc.append(s"$h$sep"))
        }
      }

      loop(this, new StringBuilder()).toString()
    }

    /**
     *
     * Реализовать метод reverse который позволит заменить порядок элементов в списке на противоположный
     */
    def reverse: List[A] = {
      def loop(l: List[A], reverseList: List[A] = Nil): List[A] = {
        l match {
          case Nil => reverseList
          case head :: tail => loop(tail, head :: reverseList)
        }
      }

      loop(this, List.Nil)
    }

   /**
     *
     * Написать функцию incList котрая будет принимать список Int и возвращать список,
     * где каждый элемент будет увеличен на 1
     */
    def incList:List[Int] = incList(1)

    def incList(add: Int): List[Int] = {
      def loop(l: List[A], incL: List[Int]): List[Int] = {
        l match {
          case Nil => incL
          case head :: tail => loop(tail, (head.asInstanceOf[Int]+add) :: incL)
        }
      }
      try {
        loop(this, Nil).reverse
      } catch {
        case e: ClassCastException => {
          println("Not cast type A to Int")
          Nil
        }
      }
    }

    /**
     *
     * Написать функцию shoutString котрая будет принимать список String и возвращать список,
     * где к каждому элементу будет добавлен префикс в виде '!'
     */

    def shoutString: List[String] = shoutString("!")
    def shoutString(prefix:String): List[String] = {
      def loop(l: List[A], shoutList: List[String]): List[String] = {
        l match {
          case Nil => shoutList
          case head :: tail => loop(tail, s"$prefix$head":: shoutList)
        }
      }

      loop(this, Nil).reverse
    }

    /**
     *
     * Реализовать метод для списка который будет применять некую ф-цию к элементам данного списка
     */
    def mapF[AA >: A](f: AA => AA): List[AA] = {
      def loop(l: List[AA], mapList: List[AA]): List[AA] = {
        l match {
          case Nil => mapList
          case head :: tail => loop(tail, f(head):: mapList)
        }
      }

      loop(this, Nil).reverse
    }

  }

  object List {

    case object Nil extends List[Nothing]

    case class ::[A](head: A, tail: List[A]) extends List[A]

    val Cons = ::

    def apply[T](args: T*): List[T] = {
      var l: List[T] = List.Nil
      args.foreach(el => l = el :: l)
      l.reverse
    }
  }


  val list = 1 :: List.Nil







}