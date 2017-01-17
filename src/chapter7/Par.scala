package chapter7

/**
  * Created by gujd on 2017/1/17.
  */


trait Par[A]


object Par1 {
    def unit[A](a: =>A): Par[A] = ???
    def get[A](a: Par[A]): A = ???
    def fork[A](a: => Par[A]): Par[A] = ???


    //Ex: 7.1
    def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B)=> C): Par[C] = {
        lazy val a_v = get(a) // ??
        lazy val b_v = get(b) // ??
        unit(f(a_v, b_v))
    }

}

object Par {
    //Ex: 7.2
    // Todo
    def unit[A](a: A): Par[A] = ???
    def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B)=>C): Par[C] = ???
    def fork[A](a: => Par[A]): Par[A] = ???
    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))
    def run[A](a: Par[A]): A = ???



}

object TestPar {
    def sum(ints: Seq[Int]): Int =
        ints.foldLeft(0)(_ + _ )

    def sum2(ints: IndexedSeq[Int]): Int =
        if(ints.size <= 1){
            ints.headOption.getOrElse(0)
        }else{
            val (l, r) = ints.splitAt(ints.length/2)
            sum2(l) + sum2(r)
        }

    def sum3(ints: IndexedSeq[Int]): Par[Int] =
        if(ints.size <= 1)
            Par.unit(ints.headOption.getOrElse(0))
        else {
            val (l, r) = ints.splitAt(ints.length/2)
            Par.map2(sum(l), sum(r))(_ + _)
        }




}
