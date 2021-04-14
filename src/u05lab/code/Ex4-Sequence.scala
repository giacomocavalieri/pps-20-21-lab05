package u05lab.code

object Ex4 {
  def map2[A, B, C](optA: Option[A], optB: Option[B])(f: (A, B) => C): Option[C] =
    for (a <- optA; b <- optB) yield f(a, b)

  def sequence[A](l: List[Option[A]]): Option[List[A]] =
    l.foldRight(Some(List.nil): Option[List[A]])(map2(_, _)(_ :: _))
}
