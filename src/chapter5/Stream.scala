package chapter5

/**
  * Created by gujd on 2017/1/16.
  */

sealed trait Stream[+A] {
    def headOption: Option[A] = this match {
        case Empty => None
        case Cons(h, t) => Some(h())
    }

    //Ex: 5.1
    def toList: List[A] = {
        def go(s: Stream[A], acc: List[A]): List[A] = s match {
            case Empty => acc.reverse
            case Cons(h, t) => go(t(), h()::acc)
        }
        go(this, Nil)
    }

    //Ex: 5.2
    def take(n:Int): List[A] = {
        def go(s: Stream[A], remain: Int, acc: List[A]): List[A] = (s, remain) match {
            case (Empty, _) | (_, 0) => acc.reverse
            case (Cons(h, t), n) => go(t(), n-1, h()::acc)
        }
        go(this, n, Nil)
    }

    def drop(n: Int): Stream[A] = {
        def go(s: Stream[A], remain: Int): Stream[A] = (s, remain) match {
            case (Empty, _) => Empty
            case (t, _)     => t
            case (Cons(h, t), n) => go(t(), n-1)
        }
        go(this, n)
    }

    //Ex: 5.3
    def takeWhile(p: A => Boolean): Stream[A] = {
        def go(s:Stream[A]):Stream[A] = s match {
            case Empty => Empty
            case Cons(h, t) => if(p(h())) t() else go(t())
        }
        go(this)
    }

    def foldRight[B](z: => B)(f: (A, =>B) => B): B = this match {
        case Cons(h, t) => f(h(), t().foldRight(z)(f))
        case _ => z
    }

    //Ex: 5.4
    def forAll(p: A=>Boolean): Boolean = {
        def go(s: Stream[A]): Boolean = s match {
            case Empty => true
            case Cons(h, t) => if(p(h())) go(t()) else false
        }
        go(this)
    }

    //Ex: 5.5
    def forAll2(p: A => Boolean): Boolean =
        _
//        this.foldRight(true)((a, b) => if(p(a)) true else )

    //Ex: 5.6

    //Ex: 5.7

    //Ex: 5.8

    //Ex: 5.9

    //Ex: 5.10

    //Ex: 5.11

    //Ex: 5.12

    //Ex: 5.13

    //Ex: 5.14

    //Ex: 5.15

    //Ex: 5.16
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
        lazy val head = hd
        lazy val tail = tl
        Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
        if(as.isEmpty)
            empty
        else
            cons(as.head, apply(as.tail: _*))
}


object TestStream {
    def main(args: Array[String]) {

    }
}
