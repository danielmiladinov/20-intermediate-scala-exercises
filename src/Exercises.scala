// The source file below compiles, but is missing certain function implementations (where you see ???)).
// It's your job to fill in the missing implementations!

trait PartialType[T[_, _], A] {
  type Apply[B] = T[A, B]
  type Flip[B] = T[B, A]
}

trait Fluffy[F[_]] {
  def furry[A, B](f: A => B, fa: F[A]): F[B]
}

object Fluffy {
  // Exercise 1
  // Relative Difficulty: 1
  def ListFluffy: Fluffy[List] = new Fluffy[List] {
    def furry[A, B](f: (A) => B, fa: List[A]): List[B] = fa map f
  }

  // Exercise 2
  // Relative Difficulty: 1
  def OptionFluffy: Fluffy[Option] = new Fluffy[Option] {
    def furry[A, B](f: (A) => B, fa: Option[A]): Option[B] = fa map f
  }

  // Exercise 3
  // Relative Difficulty: 1
  def StreamFluffy: Fluffy[Stream] = new Fluffy[Stream] {
    def furry[A, B](f: (A) => B, fa: Stream[A]): Stream[B] = fa map f
  }

  // Exercise 4
  // Relative Difficulty: 1
  import scala.collection.mutable
  def ArrayFluffy: Fluffy[mutable.ArraySeq] = new Fluffy[mutable.ArraySeq] {
    def furry[A, B](f: (A) => B, fa: mutable.ArraySeq[A]): mutable.ArraySeq[B] = fa map f
  }

  // Exercise 5
  // Relative Difficulty: 5
  def Function1Fluffy[X]: Fluffy[PartialType[Function1, X]#Apply] = new Fluffy[PartialType[Function1, X]#Apply] {
    def furry[A, B](f: (A) => B, fa: PartialType[Function1, X]#Apply[A]): PartialType[Function1, X]#Apply[B] = f compose fa
  }

  // Exercise 6
  // Relative Difficulty: 6
  def EitherLeftFluffy[X]: Fluffy[PartialType[Either.LeftProjection, X]#Flip] = new Fluffy[PartialType[Either.LeftProjection, X]#Flip] {
    def furry[A, B](f: (A) => B, fa: PartialType[Either.LeftProjection, X]#Flip[A]): PartialType[Either.LeftProjection, X]#Flip[B] = Either.LeftProjection(Left[B, X](f(fa.get)))
  }

  // Exercise 7
  // Relative Difficulty: 4
  def EitherRightFluffy[X]: Fluffy[PartialType[Either.RightProjection, X]#Apply] = new Fluffy[PartialType[Either.RightProjection, X]#Apply] {
    def furry[A, B](f: (A) => B, fa: PartialType[Either.RightProjection, X]#Apply[A]): PartialType[Either.RightProjection, X]#Apply[B] = Either.RightProjection(Right[X, B](f(fa.get)))
  }
}

trait Misty[M[_]] extends Fluffy[M] {
  def banana[A, B](f: A => M[B], ma: M[A]): M[B]

  def unicorn[A](a: A): M[A]

  // Exercise 8
  // Relative Difficulty: 3
  // (use banana and/or unicorn)
  def furry[A, B](f: A => B, ma: M[A]): M[B] = banana[A, B]((x: A) => unicorn[B](f(x)), ma)
}

object Misty {
  // Exercise 9
  // Relative Difficulty: 2
  def ListMisty: Misty[List] = new Misty[List] {
    def banana[A, B](f: (A) => List[B], ma: List[A]): List[B] = ma flatMap f

    def unicorn[A](a: A): List[A] = List(a)
  }

  // Exercise 10
  // Relative Difficulty: 2
  def OptionMisty: Misty[Option] = new Misty[Option] {
    def banana[A, B](f: (A) => Option[B], ma: Option[A]): Option[B] = ma flatMap f

    def unicorn[A](a: A): Option[A] = Option(a)
  }

  // Exercise 11
  // Relative Difficulty: 2
  def StreamMisty: Misty[Stream] = new Misty[Stream] {
    def banana[A, B](f: (A) => Stream[B], ma: Stream[A]): Stream[B] = ma flatMap f

    def unicorn[A](a: A): Stream[A] = Stream(a)
  }

  // Exercise 12
  // Relative Difficulty: 2
  import collection.mutable
  def ArrayMisty: Misty[mutable.ArraySeq] = new Misty[mutable.ArraySeq] {
    def banana[A, B](f: (A) => mutable.ArraySeq[B], ma: mutable.ArraySeq[A]): mutable.ArraySeq[B] = ma flatMap f

    def unicorn[A](a: A): mutable.ArraySeq[A] = mutable.ArraySeq(a)
  }

  // Exercise 13
  // Relative Difficulty: 6
  def Function1Misty[X]: Misty[PartialType[Function1, X]#Apply] = new Misty[PartialType[Function1, X]#Apply] {
    def banana[A, B](f: (A) => PartialType[Function1, X]#Apply[B], ma: PartialType[Function1, X]#Apply[A]): PartialType[Function1, X]#Apply[B] = (x: X) => f(ma(x))(x)

    def unicorn[A](a: A): PartialType[Function1, X]#Apply[A] = (x: X) => a
  }

  // Exercise 14
  // Relative Difficulty: 7
  def EitherLeftMisty[X]: Misty[PartialType[Either.LeftProjection, X]#Flip] = new Misty[PartialType[Either.LeftProjection, X]#Flip] {
    def banana[A, B](f: (A) => PartialType[Either.LeftProjection, X]#Flip[B], ma: PartialType[Either.LeftProjection, X]#Flip[A]): PartialType[Either.LeftProjection, X]#Flip[B] = f(ma.get)

    def unicorn[A](a: A): PartialType[Either.LeftProjection, X]#Flip[A] = Either.LeftProjection(Left(a))
  }

  // Exercise 15
  // Relative Difficulty: 5
  def EitherRightMisty[X]: Misty[PartialType[Either.RightProjection, X]#Apply] = new Misty[PartialType[Either.RightProjection, X]#Apply] {
    def banana[A, B](f: (A) => PartialType[Either.RightProjection, X]#Apply[B], ma: PartialType[Either.RightProjection, X]#Apply[A]): PartialType[Either.RightProjection, X]#Apply[B] = f(ma.get)

    def unicorn[A](a: A): PartialType[Either.RightProjection, X]#Apply[A] = Either.RightProjection(Right(a))
  }

  // Exercise 16
  // Relative Difficulty: 3
  def jellybean[M[_], A](ma: M[M[A]], m: Misty[M]): M[A] = m.banana[M[A], A]((m: M[A]) => m, ma)

  // Exercise 17
  // Relative Difficulty: 6
  def apple[M[_], A, B](ma: M[A], mf: M[A => B], m: Misty[M]): M[B] = m.banana[A => B, B]((f: A => B) => m.furry[A, B](f, ma), mf)

  // Exercise 18
  // Relative Difficulty: 6
  def moppy[M[_], A, B](as: List[A], f: A => M[B], m: Misty[M]): M[List[B]] = m.furry[B, List[B]]((b: B) => List(b), f(as.head))
}

object AdvancedFun {
  case class State[S, A](f: S => (S, A))

  // Exercise 19
  // Relative Difficulty: 9
  def StateFluffy[S]: Fluffy[PartialType[State, S]#Apply] = new Fluffy[PartialType[State, S]#Apply] {
    def furry[A, B](f: (A) => B, fa: PartialType[State, S]#Apply[A]): PartialType[State, S]#Apply[B] = State[S, B]((s: S) => (s, f(fa.f(s)._2)))
  }

  // Exercise 20
  // Relative Difficulty: 10
  def StateMisty[S]: Misty[PartialType[State, S]#Apply] = ???
}
