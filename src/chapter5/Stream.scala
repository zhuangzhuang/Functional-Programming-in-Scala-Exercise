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


//    def append(s: => Stream[A]): Stream[A] =
//        foldRight(s)((h, t) => Cons(h(), t))

    //Ex: 5.4
    def forAll(p: A=>Boolean): Boolean = {
        def go(s: Stream[A]): Boolean = s match {
            case Empty => true
            case Cons(h, t) => if(p(h())) go(t()) else false
        }
        go(this)
    }

    //Ex: 5.4
    def forAll2(p: A => Boolean): Boolean =
        this.foldRight(true)((a, b) => p(a) && b)

    //Ex: 5.5
    def takeWhile_use_flodRight(p: A => Boolean): Stream[A] = {
//        foldRight()((a, b) => p(a) )
        ???
    }

    //Ex: 5.6
    // todo


    //Ex: 5.7
    // todo

    //Ex: 5.8
    def constant[A](a: A): Stream[A] =
        Stream.cons(a, constant(a))

    //Ex: 5.9
    def from(n: Int): Stream[Int] =
        Stream.cons(n, from(n+1))

    //Ex: 5.10
    val fibs = ???

    //Ex: 5.11
    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
        case Some((a, s)) => Stream.cons(a, unfold(s)(f))
        case None       => Stream.empty
    }


    //Ex: 5.12
    val fibs_use_unfold = ???

    def from_use_unfold(n: Int): Stream[Int] =
        unfold(n)(s => Some(n+1, s+1))

    def constant_use_unfold[A](a: A): Stream[A] =
        unfold(a)(s => Some(s, s))

    val ones_use_unfold = unfold(1)(s => Some(1, 1))

    //Ex: 5.13
    def map_use_unfold[B](f: A => B): Stream[B] =
        unfold(this) {
            case Cons(h, t) => Some(f(h()), t())
            case _ => None
        }

    def take_use_unfold(n: Int): Stream[A] =
        unfold((this, n)) {
            case (Cons(h, t), 1) => Some(h(), (Empty, 0))
            case (Cons(h, t), n) if n > 1 => Some(h(), (t(), n-1))
            case _ => None
        }

    def takeWhile_use_unfold(p: A => Boolean): Stream[A] =
        unfold((this)) {
            case Cons(h, t) => if(p(h())) None else Some(h(), (t()))
            case _ => None
        }

    def zipWith[B, C](s2: Stream[B])(f: (A, B)=>C): Stream[C] =
        unfold((this, s2)) {
            case (Cons(h1, t1), Cons(h2, t2)) =>
                    Some(f(h1(), h2()), (t1(), t2()))
            case _ => None
        }
    def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
        unfold((this, s2)) {
            case (Cons(h1, t1), Cons(h2, t2)) =>
                Some((Some(h1()), Some(h2())), (t1(), t2()))
            case (_, Cons(h2, t2)) =>
                Some((None, Some(h2())), (Empty, t2()))
            case (Cons(h1, t1), _) =>
                Some((Some(h1()), None), (t1(), Empty))
            case _ => None
        }

    //Ex: 5.14

    //Ex: 5.15
    //    def tails: Stream[Stream[A]] =
    //        unfold(this) {
    //            case Empty => None
    //            case s => Some(s, s.drop(1))
    //        } append Empty


    //Ex: 5.16
    // todo scanRight
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
