package module1

import module1.list.List




object App {


  def main(args: Array[String]): Unit = {

    val list = 1 :: 2 :: 3 :: List.Nil
    val list2 = List(1, 2, 3)
    println(list.mkString)
    println(list2.mkString(" "))
  }
}
