package u05lab.ex1

import u05lab.ex1.List

// Ex 1. implement the missing methods both with recursion or with using fold, map, flatMap, and filters
// List as a pure interface
enum List[A]:
  case ::(h: A, t: List[A])
  case Nil()

  def ::(h: A): List[A] = List.::(h, this)

  def head: Option[A] = this match
    case h :: t => Some(h)
    case _ => None

  def tail: Option[List[A]] = this match
    case h :: t => Some(t)
    case _ => None

  def append(list: List[A]): List[A] = this match
    case h :: t => h :: t.append(list)
    case _ => list

  def foreach(consumer: A => Unit): Unit = this match
    case h :: t => consumer(h); t.foreach(consumer)
    case _ =>

  def get(pos: Int): Option[A] = this match
    case h :: t if pos == 0 => Some(h)
    case h :: t if pos > 0 => t.get(pos - 1)
    case _ => None

  def filter(predicate: A => Boolean): List[A] = this match
    case h :: t if predicate(h) => h :: t.filter(predicate)
    case _ :: t => t.filter(predicate)
    case _ => Nil()

  def map[B](fun: A => B): List[B] = this match
    case h :: t => fun(h) :: t.map(fun)
    case _ => Nil()

  def flatMap[B](f: A => List[B]): List[B] =
    foldRight[List[B]](Nil())(f(_) append _)

  def foldLeft[B](z: B)(op: (B, A) => B): B = this match
    case h :: t => t.foldLeft(op(z, h))(op)
    case Nil() => z

  def foldRight[B](z: B)(f: (A, B) => B): B = this match
    case h :: t => f(h, t.foldRight(z)(f))
    case _ => z

  def length: Int = foldLeft(0)((l, _) => l + 1)

  def isEmpty: Boolean = this match
    case Nil() => true
    case _ => false

  def reverse(): List[A] = foldLeft[List[A]](Nil())((l, e) => e :: l)

  /** EXERCISES */
  def zipRight: List[(A, Int)] =
    val size = length - 1
    foldRight(Nil())((i, s) => (i, s.head.fold(size)(_._2 - 1)) :: s)

  def foldLeftRight[B, C](z: B)(f: B => C)(leftOp: (B, A) => B)(rightOp: (A, B, C) => C): C = this match
    case h :: t => rightOp(h, z, t.foldLeftRight(leftOp(z, h))(f)(leftOp)(rightOp))
    case _ => f(z)

  def zipRightDoubleFolding: List[(A, Int)] =
    foldLeftRight(0)(_ => Nil())((s, _) => s + 1)((e, c, s) => (e, c) :: s)

  def partition(pred: A => Boolean): (List[A], List[A]) =
    foldRight(Nil(), Nil())((i, s) => if pred(i) then (i :: s._1, s._2) else (s._1, i :: s._2))

  def foldRightFrom[B](z: B)(f: (A, B) => B)(pred: A => Boolean): (B, List[A]) = this match
    case h :: _ if pred(h) => (z, this)
    case h :: t =>
      val folded = t.foldRightFrom(z)(f)(pred)
      (f(h, folded._1), folded._2)
    case _ => (z, Nil())

  def span(pred: A => Boolean): (List[A], List[A]) =
    foldRightFrom(Nil[A]())((e, s) => e :: s)(pred)

  /** @throws UnsupportedOperationException if the list is empty */
  def reduce(op: (A, A) => A): A = this match
    case Nil() => throw UnsupportedOperationException()
    case h :: t => t.foldLeft(h)(op)

  def takeRight(n: Int): List[A] =
    foldRight((Nil[A](), 0))((e, s) => if s._2 < n then (e :: s._1, s._2 + 1) else s)._1

  def collect[B](pf: PartialFunction[A, B]): List[B] =
    filter(pf.isDefinedAt).map(pf)

// Factories
object List:

  def apply[A](elems: A*): List[A] =
    var list: List[A] = Nil()
    for e <- elems.reverse do list = e :: list
    list

  def of[A](elem: A, n: Int): List[A] =
    if n == 0 then Nil() else elem :: of(elem, n - 1)

@main def checkBehaviour(): Unit =
  val reference = List(1, 2, 3, 4)
  println(reference.zipRight) // List((1, 0), (2, 1), (3, 2), (4, 3))
  println(reference.partition(_ % 2 == 0)) // (List(2, 4), List(1, 3))
  println(reference.span(_ % 2 != 0)) // (List(1), List(2, 3, 4))
  println(reference.span(_ < 3)) // (List(1, 2), List(3, 4))
  println(reference.reduce(_ + _)) // 10
  try Nil.reduce[Int](_ + _)
  catch case ex: Exception => println(ex) // prints exception
  println(List(10).reduce(_ + _)) // 10
  println(reference.takeRight(3)) // List(2, 3, 4)
