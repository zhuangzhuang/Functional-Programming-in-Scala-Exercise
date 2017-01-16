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
    def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
        es.foldRight[Either[E, List[A]]](Right(Nil))((e, acc)=>e.map2(acc)(_::_))

    def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
        as.foldRight[Either[E, List[B]]](Right(Nil))((e, acc)=>f(e).map2(acc)(_::_))


    sealed class Name(val value: String)
    sealed class Age(val value: Int)
    case class Person(name: Name, age: Age)

    def mkName(name: String):Either[String, Name] =
        if(name == "" || name == null)
            Left("Name is empty")
        else
            Right(new Name(name))

    def mkAge(age: Int): Either[String, Age] =
        if(age < 0)
            Left("Age is out of range.")
        else Right(new Age(age))

    def mkPerson(name: String, age: Int): Either[String, Person] =
        mkName(name).map2(mkAge(age))(Person(_, _))
    //Ex: 4.8
    // change map2
    // 使用集合错误
    // 参考: c#中的 AggregateException (https://msdn.microsoft.com/en-us/library/system.aggregateexception(v=vs.110).aspx)

}
