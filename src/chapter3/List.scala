package chapter3

/**
  * Created by zhuan on 2017/1/14.
  */

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
    def sum(ints: List[Int]): Int = ints match {
        case Nil => 0
        case Cons(x, xs) => x + sum(xs)
    }
    
    def product(ds: List[Double]): Double = ds match {
        case Nil => 1.0
        case Cons(0.0, _) => 0.0
        case Cons(x, xs)  => x * product(xs)
    }
    
    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
        as match {
            case Nil => z
            case Cons(x, xs) => f(x, foldRight(xs, z)(f))
        }
    
    //Ex: 3.7
    //Answer: 不可以, 因为程序没有判断
    
    //Ex: 3.8
    //Anser: List数据原样输出
    
    //Ex: 3.9
    def length[A](as: List[A]): Int =
        foldRight(as, 0) ((x, acc) => acc + 1)
    
    //Ex: 3.10
    def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
        def go(as: List[A], acc: B): B = as match {
            case Nil => acc
            case Cons(x, xs) => go(xs, f(acc, x))
        }
        go(as, z)
    }
    
    //Ex: 3.11 -> sum
    def sum3(ns: List[Int]) =
        foldLeft(ns, 0)(_ + _)
    
    //Ex: 3.11 -> product
    def product3(ds: List[Double]) =
        foldLeft(ds, 1.0)(_ * _)
    
    //Ex: 3.11 -> length
    def length3[A](as: List[A]): Int =
        foldLeft(as, 0)((acc, _) => acc+1)
    
    //Ex: 3.12
    def reverse[A](as: List[A]): List[A] =
        foldLeft(as, Nil: List[A])((acc, a) => Cons(a, acc))
    
    //Ex: 3.13
    //todo
    
    //Ex: 3.14
    def append[A](xs: List[A], ys: List[A]): List[A] =
        foldRight(xs, ys)((x, acc) => Cons(x, acc))
    
    //Ex: 3.15
    //todo
    
    //Ex: 3.16
    def add1(ns: List[Int]): List[Int] =
        foldRight(ns, Nil: List[Int])((a, acc) => Cons(a+1, acc))
    
    //Ex: 3.17
    def doubleToString(ds: List[Double]): List[String] =
        foldRight(ds, Nil: List[String])((a, acc) => Cons(a.toString, acc))
    
    //Ex: 3.18
    def map[A, B](as: List[A])(f: A => B): List[B] =
        foldRight(as, Nil: List[B])((a, acc) => Cons(f(a), acc))
    
    //Ex: 3.19
    def filter[A](as: List[A])(f: A => Boolean): List[A] =
        foldRight(as, Nil: List[A])((a, acc) => if(f(a)) Cons(a, acc) else acc)
    
    //Ex: 3.20
    def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
        foldRight(as, Nil: List[B])((a, acc) => append(f(a),acc))
    
    //Ex: 3.21
    def filter2[A](as: List[A])(f: A=> Boolean): List[A] =
        flatMap(as)((a: A) => if(f(a)) List(a) else Nil)
    
    //Ex: 3.22
    def zipPlus(xl: List[Int], yl: List[Int]): List[Int] = (xl, yl) match {
        case (Nil, _) => Nil
        case (_, Nil) => Nil
        case (Cons(x, xs), Cons(y, ys)) => Cons(x+y, zipPlus(xs, ys))
    }
    
    //Ex: 3.23
    def zipWith[A, B, C](al: List[A], bl: List[B])(f: (A, B)=>C) : List[C] = (al, bl) match {
        case (Nil, _) => Nil: List[C]
        case (_, Nil) => Nil: List[C]
        case (Cons(a, as), Cons(b, bs)) => Cons(f(a, b), zipWith(as, bs)(f))
    }
    
    //Ex: 3.24
    def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean =
        true
    
    def sum2(ns: List[Int]) =
        foldRight(ns, 0)((x, y) => x + y)
    
    def product2(ns: List[Double]) =
        foldRight(ns, 1.0)(_ * _)
    
    
    //Ex: 3.2
    def tail[A](ls: List[A]): List[A] = ls match {
        case Nil => Nil // hack
        case Cons(_, xs) => xs
    }
    
    //Ex: 3.3
    def setHead[A](ls: List[A], head: A):List[A] = ls match {
        case Nil => List(head) // hack
        case Cons(_, xs) => Cons(head, xs)
    }
    
    //Ex: 3.4
    def drop[A](l: List[A], n: Int): List[A] = (l, n) match {
        case (xs, 0) => xs
        case (Nil, _) => Nil
        case (Cons(_, xs), v) => drop(xs, v-1)
    }
    
    //Ex: 3.5
    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
        def go(l: List[A]): List[A] = l match {
            case Nil => Nil
            case Cons(x, xs) => if(f(x)) go(xs) else xs
        }
        go(l)
    }
    
    //Ex: 3.6
    def init[A](l: List[A]): List[A] = l match {
        case Nil  => Nil // hack
        case Cons(_, Nil) => Nil
        case Cons(x, Cons(_, Nil)) => List(x)
        case Cons(x, Cons(y, ys))  => Cons(x, init(Cons(y, ys)))
    }
    
    def apply[A](as: A*): List[A] = {
        if(as.isEmpty)
            Nil
        else
            Cons(as.head, apply(as.tail: _*))
    }
}

object TestList {
    
    //Ex: 3.1
    val x =List(1,2,3,4,5) match {
        case Cons(x, Cons(2, Cons(4, _))) => x
        case Nil => 42
        case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y // <= match this
        case Cons(h, t) => h + List.sum(t)
        case _ => 101
    }
    
    def main(args: Array[String]): Unit = {
//        println(List.init(List(1,2,3)))
//        println(List.init(List(1,2)))
//        println(List.init(List(1)))
        
        //Ex: 3.8
        val r_3_8 =  List.foldRight(List(1,2,3,4), Nil: List[Int])(Cons(_, _))
        println(r_3_8)
        
        println(List.length(List(1,2,3)))
    }
}
