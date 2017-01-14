package chapter2

/**
  * Created by zhuan on 2017/1/14.
  */
object Simple {
    
    def abs(n: Int): Int =
        if (n < 0)
            -n
        else
            n
    
    private def formatAbs(x: Int): String = {
        val msg ="The absolute value of %d is %d"
        msg.format(x, abs(x))
    }
    
    private def formatFactorial(n: Int): String ={
        val msg = "The factorial value of %d is %d"
        msg.format(n, factorial(n))
    }
    
    private def formatResult(name: String, n: Int, f: Int => Int): String ={
        val msg = "The %s of %d is %d."
        msg.format(name, n, f(n))
    }
    
    def factorial(n: Int): Int = {
        def go(n: Int, acc: Int): Int =
            if(n <= 0) acc
            else go(n-1, acc * n)
        go(n, 1)
    }
    
    // Ex:2.1
    def fib(n: Int): Int =  {
        def go(n0: Int, n1: Int, n: Int): Int ={
            if(n <= 0)
                n0
            else
                go(n1, n0 + n1, n-1)
        }
        go(0, 1, n)
    }
    
    def findFirst(ss: Array[String], key: String): Int= {
        @annotation.tailrec
        def loop(n: Int): Int =
            if (n >= ss.length) -1
            else if(ss(n) == key) n
            else loop(n+1)
        loop(0)
    }
    
    def findFirst[A](as: Array[A], p: A=>Boolean): Int= {
        @annotation.tailrec
        def loop(n: Int): Int =
            if (n >= as.length) -1
            else if(p(as(n))) n
            else loop(n+1)
        loop(0)
    }
    
    //Ex: 2.2
    def isSorted[A](as: Array[A], ordered: (A, A)=>Boolean): Boolean = {
        @annotation.tailrec
        def loop(n: Int): Boolean =
            if(n >= as.length-1) true
            else if(!ordered(as(n - 1, n))) false
            else loop(n+1)
        if(as.length < 2)
            true
        else
            loop(0)
    }
    
    //Ex: 2.3
    def curry[A, B, C](f: (A, B) => C): A => (B=>C) = {
        (a: A) => (b: B) => f(a, b)
    }
    
    //Ex: 2.4
    def uncurry[A, B, C](f: A=>B=>C): (A, B) => C = {
        (a: A, b: B) => f(a)(b)
    }
    
    //Ex: 2.5
    def compose[A, B, C](f: B=>C, g: A=>B): A => C = {
        (a:A) => f(g(a))
    }
    
    def main(args: Array[String]): Unit = {
//        println(formatAbs(-42))
        for(i <- 0 to 10){
            println(fib(i))
        }
    }
}
