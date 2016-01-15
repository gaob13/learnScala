
/**
  *
  * Created by gaobin on 16-1-13.
  */

class Main1785 {
  def main = {
    val n = readInt()
    val u = n match {
      case x if x < 5 => "few"
      case x if x < 10 => "several"
      case x if x < 20 => "pack"
      case x if x < 50 => "lots"
      case x if x < 100 => "horde"
      case x if x < 250 => "throng"
      case x if x < 500 => "swarm"
      case x if x < 1000 => "zounds"
      case _ => "legion"
    }
    println(u)
  }
}

class Main1877 {
  def main (args: Array[String]) {
    val firstLock = readInt()
    val secondLock = readInt()
    if (firstLock % 2 == 1 && secondLock % 2 == 0)
      println("no")
    else
      println("yes")
  }
}

class Main1820 {
  def main (args: Array[String]) {
    val lin = io.StdIn.readLine().split(' ').map(_.toInt)
    println(math.max((lin(0) * 2.0 / lin(1)).ceil.toInt, 2))
  }
}

class Main1787 {
  def main (args: Array[String]): Unit = {
    //val (n:Int, k:Int) = io.StdIn.readf2("{0,number,integer} {1,number,integer}")
    val (n, k) = io.StdIn.readf2("{0,number,integer} {1,number,integer}")
    val N = n.toString.toInt
    val K = k.toString.toInt
    var s:Int = 0
    val a = io.StdIn.readLine().split(' ').map(_.toInt)
    for (p <- a) {
      s = math.max(0, s - N + p)
    }
    println(s)
  }
}

class Main1002 {
  def parse (key:String): String = {
    val ans = new StringBuffer()
    for (c:Char <- key.toCharArray) {
      ans.append(Main1002.keyboard(c))
    }
    ans.toString
  }
  def main (args: Array[String]): Unit = {
    import scala.collection.mutable.HashMap
    var target = ""
    while ({target = io.StdIn.readLine(); target != "-1"}) {
      val k = io.StdIn.readInt()
      val dict = HashMap[String,String]()
      for (i <- 1 to k) {
        val key = io.StdIn.readLine()
        dict(parse(key)) = key
      }
      val dp = new Array[Int](101)
      val dpr = new Array[Int](101)
//      for (i <- 0 until dp.length)
//        dp(i) = new Array[Int](101)
      for (i <- 1 until dp.length)
        dp(i) = -1
      dp(0) = 0
      for (i <- 1 to target.length) {
        for (j <- 0 until i) {
          if(dp(j) >= 0 && dict.contains(target.substring(j,i))) {
//            if (dp(i) == -1) dp(i) = dp(j) + 1
            //println(dict(target.substring(j,i)))
            if (dp(i) == -1 || dp(i) >= dp(j) + 1) {
              dp(i) = dp(j) + 1
              dpr(i) = j
            }
          }
        }
      }
      if (dp(target.length) >= 0) {
        import scala.collection.mutable.Stack
        val stack = new Stack[String]
        var t = target.length
        var s = dpr(t)
        while (t > 0) {
          stack.push(dict(target.substring(s,t)))
          t = s
          s = dpr(t)
        }
        while (stack.nonEmpty)
          print(stack.pop() + " ")
//        println(dp(target.length), dpr(target.length))
        println()
      } else {
        println("No solution.")
      }

//      for (i <- 0 to 100)
//        print (dp(i) + " ")
//
//      println()
//      for (i <- 0 to 100)
//        print (dpr(i) + " ")


    }
  }
}

object Main1002 {
  import scala.collection.immutable.HashMap
  val keyboard = HashMap[Char, Char](
    ('i','1'), ('j','1'), ('a','2'), ('b','2'), ('c','2'), ('d','3'), ('e','3'), ('f','3'),
    ('g','4'), ('h','4'), ('k','5'), ('l','5'), ('m','6'), ('n','6'), ('p','7'), ('r','7'),
    ('s','7'), ('t','8'), ('u','8'), ('v','8'), ('w','9'), ('x','9'), ('y','9'), ('o','0'),
    ('q','0'), ('z','0')
  )

}

class Main2073 {
  def main(args: Array[String]) = {
    val (width1, width2, width3) = (30, 8, 13)
    println("+" + "-" * width1 + "+" + "-" * width2 + "+" + "-" * width3 + "+")
    println("|" + Main2073.stringWithWidth("Contest name", width1) + "|" + Main2073.stringWithWidth("Date",width2) + "|ABCDEFGHIJKLM|")
    println("+" + "-" * width1 + "+" + "-" * width2 + "+" + "-" * width3 + "+")
    val n = scala.io.StdIn.readInt()
    for (r <- 1 to n) {
      val name = scala.io.StdIn.readLine()
      val date = scala.io.StdIn.readLine()
      val num = scala.io.StdIn.readLine().split(" ").map(_.toInt)
      val m = num(0)
      val n = num(1)
      val str = new StringBuilder(m)
      for (i <- 0 until m) str.append('.')
      for (i <- 1 to n) {
        val (p, o) = scala.io.StdIn.readf2("{0} {1}")
        val index = p.toString.charAt(0).toInt - 'A'.toInt
        str(index) = o.toString.charAt(0) match {
          case 'A' => 'o'
          case _ if str(index) != 'o' => 'x'
          case _ => 'o'
        }
      }
      println("|" + Main2073.stringWithWidth(name, width1) + "|" + Main2073.stringWithWidth(date,width2) + "|" + Main2073.stringWithWidth(str.toString(),width3) + "|")
      println("+" + "-" * width1 + "+" + "-" * width2 + "+" + "-" * width3 + "+")
    }
  }
}

object Main2073 {
  def stringWithWidth(str: String, width: Int): String = (str + " " * width).substring(0, width)
}

class Main2033 {
  def main(Args: Array[String]): Unit = {
    import scala.collection.mutable.HashMap
    val count = HashMap[String, Int]()
    val price = HashMap[String, Int]()
    for (i <- 1 to 6) {
      scala.io.StdIn.readLine()
      val name = scala.io.StdIn.readLine()
      val pri = scala.io.StdIn.readInt()
      if (! count.contains(name)) {
        count(name) = 0
        price(name) = Integer.MAX_VALUE
      }
      count(name) += 1
      price(name) = math.min(price(name), pri)
    }
    var m = 0
    var n = Integer.MAX_VALUE
    var ans = ""
    for (name <- count.keySet if m < count(name))  m = count(name)
    for (name <- price.keySet if count(name) == m) if (n > price(name)) {
      n = price(name)
      ans = name
    }
    println(ans)
  }
}

object Main extends App {
  new Main2033().main(args)
}

