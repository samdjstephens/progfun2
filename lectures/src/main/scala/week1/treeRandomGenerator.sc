trait Tree
case class Inner(left: Tree, right: Tree) extends Tree
case class Leaf(x: Int) extends Tree


trait Generator[+T] {
  self => // an alias for ”this”.
  def generate: T
  def map[S](f: T => S): Generator[S] = new Generator[S] {
    def generate = f(self.generate)
  }
  def flatMap[S](f: T => Generator[S]): Generator[S] = new Generator[S] {
    def generate = f(self.generate).generate
  }
}


def single[T](x: T): Generator[T] = new Generator[T] {
  def generate = x
}


val integers = new Generator[Int] {
  val rand = new java.util.Random
  override def generate: Int = rand.nextInt()
}

val booleans = for (x <- integers) yield x > 0


def trees: Generator[Tree] = for {
  isLeaf <- booleans
  value <- integers
  tree <- if (isLeaf) single(Leaf(value)) else treeRecurseGenerator
} yield tree

def treeRecurseGenerator = for {
  left <- trees
  right <- trees
} yield Inner(left, right)

trees.generate
trees.generate
trees.generate
trees.generate