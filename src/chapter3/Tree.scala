package chapter3

/**
  * Created by zhuan on 2017/1/14.
  */

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
    //Ex: 3.25
    def size[A](tree: Tree[A]): Int = tree match {
        case Leaf(_) => 1
        case Branch(left, right) => 1 + size(left) + size(right)
    }
    
    //Ex: 3.26
    def maximum(tree: Tree[Int]): Int = tree match {
        case Leaf(x) => x
        case Branch(left, right) => maximum(left) max maximum(right)
    }
    
    //Ex: 3.27
    def depth(tree: Tree[Int]): Int = tree match {
        case Leaf(_) => 1
        case Branch(left, right) => depth(left).max(depth(right)) + 1
    }
    
    //Ex: 3.28
    def map[A, B](tree: Tree[A])(f: A=>B): Tree[B] = tree match {
        case Leaf(x) => Leaf(f(x))
        case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    }
    
    //Ex: 3.29
    def fold[A, B](tree: Tree[A])(f1: (A, B)=>B)(f2: (B, B)=>B): B = tree match {
        case Leaf(x) =>f1(x)
        case Branch(left, right) => f2(fold(left)(f1)(f2),
                                       fold(right)(f1)(f2))
    }
}

object TestTree {
    def main(args: Array[String]): Unit = {
        print(Tree.size(Leaf(1)))
    }
}