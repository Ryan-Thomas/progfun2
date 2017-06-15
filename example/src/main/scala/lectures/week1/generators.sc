import lectures.week1.Generator

trait Tree
case class Inner(left: Tree, right: Tree) extends Tree
case class Leaf(x: Int) extends Tree

object generators {
    val integers = new Generator[Int] {
        def generate = scala.util.Random.nextInt();
    }
    
    val booleans = integers.map(_ >= 0) 
    
    def leafs: Generator[Leaf] = for {
        x <- integers
    } yield Leaf(x)
    
    def trees: Generator[Tree] = for {
        isLeaf <- booleans
        tree <- if (isLeaf) leafs else inners
    } yield tree
    
    def inners: Generator[Inner] = for {
        l <- trees
        r <- trees
    } yield Inner(l, r)
    
}


// Using generators we can write a random test function:

def test[T](g: Generator[T], numTimes: Int = 100) (test: T => Boolean): Unit = {
    for (i <- 0 until numTimes) {
        val value = g.generate
        assert(test(value), s"test failed for $value")
    }
    println(s"passed $numTimes tests")
}

// Example usage:
