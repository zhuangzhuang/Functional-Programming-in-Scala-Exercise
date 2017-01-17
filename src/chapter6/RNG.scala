package chapter6

/**
  * Created by gujd on 2017/1/17.
  */

trait RNG {
    def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
    override def nextInt: (Int, RNG) = {
        val newSeed = (seed * 0x5deece66dl + 0xbl) & 0xffffffff
        val nextRNG = SimpleRNG(newSeed)
        val n = (newSeed >>> 16).toInt
        (n, nextRNG)
    }
}

object RNG {
    def randomPair(rng: RNG): ((Int, Int), RNG) = {
        val (i1, rng2) = rng.nextInt
        val (i2, rng3) = rng2.nextInt
        ((i1, i2), rng3)
    }

    //Ex: 6.1
    def nonNegativeInt(rng: RNG): (Int, RNG) = {
        def go(rng: RNG): (Int, RNG) = {
            val (x, next_rng) = rng.nextInt
            if(x >= 0)
                (x, next_rng)
            else
                go(next_rng)
        }
        go(rng)
    }

    //Ex: 6.2
    def double(rng: RNG): (Double, RNG) = {
        val (i, next_rng) = nonNegativeInt(rng)
        (i/ (Int.MaxValue.toDouble + 1), next_rng)
    }

    //Ex: 6.3
    def intDouble(rng: RNG): ((Int, Double), RNG) = {
        val (i, rng2) = rng.nextInt
        val (d, rng3) = double(rng)
        ((i, d), rng3)
    }

    def doubleInt(rng: RNG): ((Double, Int), RNG) = {
        val (d, rng2) = double(rng)
        val (i, rng3) = rng2.nextInt
        ((d, i), rng3)
    }

    def double3(rng: RNG): ((Double, Double, Double), RNG) = {
        val (d1, rng1) = double(rng)
        val (d2, rng2) = double(rng1)
        val (d3, rng3) = double(rng2)
        ((d1, d2, d3), rng3)
    }

    //Ex: 6.4
    def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
        List.range(0, count).foldRight[(List[Int], RNG)]((Nil, rng))((_, acc) => {
            val rng = acc._2
            val (i, next_rng) = rng.nextInt
            (i:: acc._1, next_rng)
        })
    }

    type Rand[+A] = RNG => (A, RNG)

    val int: Rand[Int] = _.nextInt

    def unit[A](a: A):Rand[A] =
        rng => (a, rng)

    def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
        rng => {
            val (a, rng2) = s(rng)
            (f(a), rng2)
        }

    def nonNegativeEven: Rand[Int] =
        map(nonNegativeInt)(i => i - i % 2)

    //Ex: 6.5
    def double2: Rand[Double] =
        map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

    //Ex: 6.6
    def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B)=>C): Rand[C] =
        rng => {
            val (a, rng1) = ra(rng)
            val (b, rng2) = rb(rng1)
            (f(a, b), rng2)
        }

    //Ex: 6.7
    // Todo

    def nonNegativeLessThan(n: Int): Rand[Int] = {
        rng =>
            val (i, rng2) = nonNegativeInt(rng)
            val mod = i % n
            if (i + (n-1) - mod >=0)
                (mod, rng2)
            else
                nonNegativeLessThan(n)(rng)
    }

    //Ex: 6.8
    def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
        rng => {
            val (a, rng2) = f(rng)
            g(a)(rng2)
        }

    //Ex: 6.9
    def map_use_flatMap[A, B](s: Rand[A])(f: A => B): Rand[B] =
        flatMap(s)(a => unit(f(a)))

    def map2_use_flatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B)=>C): Rand[C] =
        flatMap(ra)(a => flatMap(rb)(b => unit(f(a, b))))
}


// Ex: 6.10
case class State[S, +A](run: S => (A, S)) {

    def map[B](f: A => B ): State[S, B] =
        State{s => {
            val (a, s1) = run(s)
            (f(a), s1)
        }}

    def map2[B, C](that: State[S, B])(f: (A, B)=> C) : State[S, C] =
        State {
            s => {
                val (a, s1) = run(s)
                val (b, s2) = that.run(s1)
                (f(a, b), s2)
            }
        }

    def flatMap[B](f: A => State[S, B]): State[S, B] =
        State {
            s => {
                val (a, s1) = run(s)
                f(a).run(s1)
            }
        }
}

object State {
    def unit[S, A](a: A) =
        State[S, A](s => (a, s))


    //def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]]
    // Todo


    def get[S]: State[S, S] = State(s => (s, s))
    def set[S](s: S): State[S, Unit] = State(_ => ((), s))

    def modify[S](f: S => S): State[S, Unit] = for{
        s <- get
        _ <- set(f(s))
    } yield ()


    //Ex: 6.11
    // Todo

}





