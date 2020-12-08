package module1

import module1.list.List
import module1.opt.Option


object App {


  def main(args: Array[String]): Unit = {
    println("//--------------------LIST---------------//")
    val list: List[Int] = 1 :: 2 :: 3 :: List.Nil
    val list2 = List(1, 2, 3)
    val list3 = List("a", "b", "c")
    println("mkString= " + list.mkString)
    println("mkString with sep = " + list2.mkString(" "))

    println("reverse= " + list.reverse.mkString)

    println("incList on Int= " + list.incList.mkString)
    println("incList on String= " + list3.incList.mkString)
    println("incList on Int with param = " + list.incList(100).mkString)

    println("shoutString on Int List with param= " + list.shoutString("el=").mkString)
    println("shoutString on String with default= " + list3.shoutString.mkString)


    println("mapF on Int List= " + list.mapF(x => x * x).mkString)
    println("mapF on String List= " + list3.mapF(x => x.repeat(2)).mkString)
    println("//---------------------------------------//")
    println()

    //--------------------OPTION-------------//
    val withValue: Option[Int] = Option(3)
    val emptyOption: Option[Int] = Option.None

    print("withValue.printIfAny = ")
    withValue.printIfAny
    print("emptyOption.printIfAny = ")
    emptyOption.printIfAny

    println("withValue.orElse = " + withValue.orElse(-1))
    println("emptyOption.orElse = " + emptyOption.orElse(-1))

    println("withValue.zip = " + withValue.zip(Option(2)))
    println("withValue.zip(Option(2).zip()) = " + withValue.zip(Option(2).zip(Option(1))))
    println("emptyOption.zip = " + emptyOption.zip(Option(1)))

    println("withValue.filter true = " + withValue.filter(x => x > 2))
    println("withValue.filter false = " + withValue.filter(x => x < 2))
    println("emptyOption.filter = " + emptyOption.filter(x => x < 2))


  }
}
