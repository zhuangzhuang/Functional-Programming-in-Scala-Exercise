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
        (i/ (Int.MaxValue + 1).toDouble, next_rng)
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
}





