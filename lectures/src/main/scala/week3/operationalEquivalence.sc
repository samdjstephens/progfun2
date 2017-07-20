val x = Array(1, 2, 3)
val y = x

def func(a: Array[Int], b: Array[Int]) = {
  a.update(1, 4)
  assert(b(1) == 2)
}

func(x, y)
func(x, x)