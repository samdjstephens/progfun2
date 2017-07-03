case class Book(title: String, authors: List[String])

val books: List[Book] = List(
  Book(title = "Structure and Interpretation of Computer Programs",
    authors = List("Abelson, Harald", "Sussman, Gerald J.")),
  Book(title = "Introduction to Functional Programming",
    authors = List("Bird, Richard", "Wadler, Phil")),
  Book(title = "Effective Java",
    authors = List("Bloch, Joshua")),
  Book(title = "Java Puzzlers",
    authors = List("Bloch, Joshua", "Gafter, Neal")),
  Book(title = "Programming in Scala",
    authors = List("Odersky, Martin", "Spoon, Lex", "Venners, Bill")))



for (b <- books; a <- b.authors if a startsWith "Bird")
  yield b.title


// Stage 1 - remove filter
for (b <- books; a <- b.authors.withFilter(x => x.startsWith("Bird")))
  yield b.title

// Stage 2 - remove second generator
books.flatMap(b => for (a <- b.authors.withFilter(x => x.startsWith("Bird"))) yield b.title)

// Stage 3 - remove last generator
books.flatMap(b =>
  b.authors.withFilter(x =>
    x.startsWith("Bird")
  ).map(_ => b.title)
)
