import fpinscala.datastructures.List._
import fpinscala.datastructures._


def filter[A](as: List[A])(f: A => Boolean): List[A] =
  foldRight(as, Nil: List[A])((h, t) => if (f(h)) Cons(h, t) else t)

filter(List(1, 2, 3))(_ == 1)