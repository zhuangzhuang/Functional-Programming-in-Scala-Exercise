package chapter4

import scala.{Option => _, Either => _}

/**
  * Created by zhuan on 2017/1/14.
  */

//Ex: 4.1
sealed trait Option[+A] {
    def map[B](f: A => B): Option[B] = this match {
        case None => None
        case Some(x) => Some(f(x))
    }
    def flatMap[B](f: A => Option[B]): Option[B] = this match {
        case Some(x) => f(x)
        case None => None
    }
    def getOrElse[B >: A](default: => B): B = this match {
        case Some(x) => x
        case None    => default
    }
    def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
        case Some(_) => this
        case None    => ob
    }
    def filter(f: A=> Boolean): Option[A] = this match {
        case Some(x) => if(f(x)) this else None
        case None    => None
    }
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]


object Option {
    def mean(xs: Seq[Double]): Option[Double] =
        if(xs.isEmpty)
            None
        else
            Some(xs.sum / xs.length)
    
    //Ex: 4.2
    def variance(xs: Seq[Double]): Option[Double] =
        None
    
    //Ex: 4.3
    def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
        a.flatMap(va => b.map(vb => f(va, vb)))
    
    //Ex: 4.4
    def sequence[A](a: List[Option[A]]): Option[List[A]] =
        a.foldRight[Option[List[A]]](Some(Nil))((n, acc) => (n, acc) match {
            case (None, _) => None
            case (_, None) => None
            case (Some(v), Some(lv)) => Some(v::lv)
        })
    
    //Ex: 4.5
    def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
        a.foldRight[Option[List[B]]](Some(Nil))((n, acc)=> {
            for(v <- f(n);
                lst <- acc) yield v::lst
        })
}


object TestOption{
    def main(args: Array[String]): Unit = {
        println(Some(1).get)
    }
}
