package object bucharestfp {
  /**
   * Provides a symmetrical `->` construct for use in tuple matching, companion
   * to the built-in `->` used for building tuples.
   *
   * ```
   * scala> val a -> b = 1 -> 2
   * a: Int = 1
   * b: Int = 2
   * ```
   *
   * Instead of the more verbose:
   *
   * ```
   * scala> val (a, b) = 1 -> 2
   * a: Int = 1
   * b: Int = 2
   * ```
   */
  object -> {
    def unapply[A,B](t: (A, B)): Option[(A, B)] = Some(t)
  }
}
