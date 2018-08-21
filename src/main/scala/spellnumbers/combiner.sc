

def combine(ao: Option[String], bo: Option[String]): Option[String] = {
  for (a <- ao; b <- bo) yield (a + " frullalla " + b)
}


val x = combine(Some("a"), None)


None.map(a => Some("b"))