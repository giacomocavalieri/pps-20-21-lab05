package u05lab.code

object Ex4 {
  def sequence[A](l: List[Option[A]]): Option[List[A]] = l.foldRight(Some(List.nil): Option[List[A]])(
    (elem, acc) => elem match {
      case Some(x) => acc.map(list => x :: list)
      case None => None
    })
}
