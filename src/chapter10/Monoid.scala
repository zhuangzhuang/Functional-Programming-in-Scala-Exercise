package chapter10

/**
  * Created by gujd on 2017/1/17.
  */
trait Monoid [A]{
    def op(a1: A, a2: A): A
    def zero: A
}

object Monoid {
    val stringMonoid = new Monoid[String] {
        def op(a1: String, a2: String) = a1 + a2

        def zero = ""
    }

    def listMonoid[A] = new Monoid[List[A]] {
        override def op(a1: List[A], a2: List[A]): List[A] = a1 ::: a2

        override def zero: List[A] = Nil
    }

    // Ex: 10.1
    val intAddition = new Monoid[Int] {
        override def op(a1: Int, a2: Int): Int = a1 + a2
        override def zero: Int = 0
    }

    val intMultiplication = new Monoid[Int] {
        override def op(a1: Int, a2: Int): Int = a1 * a2
        override def zero: Int = 1
    }

    val booleanOr = new Monoid[Boolean] {
        override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
        override def zero: Boolean = false
    }

    val booleanAnd = new Monoid[Boolean] {
        override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
        override def zero: Boolean = true
    }

    //Ex: 10.2
    def optionMonoid[A] = new Monoid[Option[A]] {
        override def op(a1: Option[A], a2: Option[A]): Option[A] = a1.orElse(a2)
        override def zero: Option[A] = None
    }

    //Ex: 10.3
    def endoMonoid[A] =  new Monoid[A => A]{
        override def op(a1: (A) => A, a2: (A) => A): (A) => A =
            a => a1(a2(a))
        override def zero: (A) => A = a => a
    }

    //Ex: 10.4
    // Todo

    //Ex: 10.5
    def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = {
        ???
    }
}
