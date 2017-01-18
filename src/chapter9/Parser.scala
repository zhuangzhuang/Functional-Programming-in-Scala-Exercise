package chapter9

/**
  * Created by gujd on 2017/1/17.
  */

trait Parser[T] {
    def map[A, B](a: Parser[A])(f: A => B): Parser[B] = ???
}

class ParserError


trait Parsers[ParseError, Parser[+_]] { self =>

    def run[A](p: Parser[A])(input: String): Either[ParserError, A] = ???

//    def char(c: Char): Parser[Char] = ???

//    def string(s: String): Parser[String] = ???
//    def orString(s1: String , s2: String): Parser[String] = ???

    def or[A](s1: Parser[A], s2: Parser[A]): Parser[A] = ???


    implicit def string(s: String): Parser[String] = ???
    implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
    implicit def asStringParser[A](a: A)(implicit f: A=>Parser[String]): ParserOps[String] =
        ParserOps(f(a))

    def product[A, B](p: Parser[A], p2: Parser[B]): Parser[(A, B)] = ???
    def map[A, B](p: Parser[A])(f: A => B): Parser[B] = ???

    case class ParserOps[A](p: Parser[A]) {
        def |[B>:A](p2: Parser[B]): Parser[B] = self.or(p, p2)
        def or[B>:A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

        def product[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)
        def **[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)


        def map[B](f: A => B): Parser[B] = self.map(p)(f)
    }

    def slice[A](p: Parser[A]): Parser[String] = ???
    def many[A](p: Parser[A]): Parser[List[A]] = ???


    //Ex: 9.1
    def map2[A, B, C](p1: Parser[A], p2: Parser[B])(f: (A, B) => C): Parser[C] =
        p1.product(p2).map(r => f(r._1, r._2))

    def many1[A](p: Parser[A]): Parser[List[A]] = (p ** many(p)).map(x => x._1 :: x._2)


    //Ex: 9.2
    // Todo

    //Ex: 9.3
    def many_use_or_map2_succed[A](p: Parser[A]):Parser[List[A]] =
        succeed(Nil).or((p ** many_use_or_map2_succed(p)).map(x => x._1 :: x._2))

    //Ex: 9.4
    def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = {
        List.range(0, n).foldRight[Parser[List[A]]](succeed(Nil))((_, acc) => (p ** acc).map(x => x._1 :: x._2))
    }

    //Ex: 9.5
    //Todo


    def char(c: Char): Parser[Char] =
        string(c.toString).map(_.charAt(0))

    def succeed[A](a: A): Parser[A] =
        string("").map(_ => a)





}

object Parsers {
    import Parsers._





}
