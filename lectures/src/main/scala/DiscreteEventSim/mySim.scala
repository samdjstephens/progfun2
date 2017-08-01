package DiscreteEventSim

object mySim extends Circuits with Parameters {
  val in1, in2, sum, carry = new Wire

  halfAdder(in1, in2, sum, carry)

  probe("sum", sum)
  probe("carry", carry)

  in1 setSignal true

  probe("sum", sum)
  probe("carry", carry)

  in2 setSignal true

  probe("sum", sum)
  probe("carry", carry)
}
