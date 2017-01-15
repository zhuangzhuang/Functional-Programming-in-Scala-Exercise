package chapter4

import scala.{Either => _, Option => _}
/**
  * Created by zhuan on 2017/1/15.
  */

//Ex: 4.6
sealed trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B] = this match {
        case Left(e) =>  Left(e)
        case Right(v) => Right(f(v))
    }
    
    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
        case Left(e)  => Left(e)
        case Right(v) => f(v)
    }
    
    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
        case Left(e) => b
        case Right(v) => this
    }
    
    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = this match {
        case Left(e) => Left(e)
        case Right(v1) => b match {
            case Left(eb) => Left(eb)
            case Right(v2) => Right(f(v1, v2))
        }
    }
}
case class Left[+E](value: E) extends  Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {
    //Ex: 4.7
    
    //Ex: 4.8
}
