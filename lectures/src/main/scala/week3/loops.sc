class REPEAT(command: => Unit) {

  def UNTIL(condition: => Boolean): Unit = {
    command
    if (condition) ()
    else UNTIL(condition)
  }
}



def WHILE(condition: => Boolean, command: => Unit): Unit = {
  if (condition) {
    command
    WHILE(condition, command)
  } else ()
}

var x = 0
WHILE(x < 10, {
  println(x)
  x = x + 1
})


def REPEAT_(command: => Unit)(condition: => Boolean): Unit = {
  command
  if (condition) ()
  else REPEAT_(command)(condition)
}

REPEAT_({
  x = x - 1
  println(x)
})(x == 0)



var y = 10
new REPEAT({
  y = y - 1
  println(y)
}) UNTIL (y == 0)

y