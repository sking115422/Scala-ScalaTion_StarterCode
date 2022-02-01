
package my_scalation

import scala.math.sqrt

import scalation._
import scalation.mathstat._
import scalation.random._

// > runMain my_scalation.ConfidenceIntervalTest

object linReg1 extends App:

    val x0 = VectorD (1, 2, 3, 4)
    val y = VectorD (1, 3, 3, 4)
    val b0 = VectorD.range (0, 50) / 25.0
    val sse = new VectorD (b0.dim)
    
    for i <- b0.indices do
        val e = y - x0 * b0(i)
        sse(i) = e dot e
    end for

    new Plot (b0, sse, lines = true)

end linReg1

