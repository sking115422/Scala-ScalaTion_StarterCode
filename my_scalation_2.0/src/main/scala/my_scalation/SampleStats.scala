
package my_scalation

import scalation.mathstat._
import scalation.random._

// > runMain my_scalation.SampleStats

object SampleStats extends App:

    val (mu, sig) = (70.0, 8.0)                               // population mean and standard deviation
    val m      = 100                                          // sample size
    val rvg    = Normal (mu, sig * sig)                       // Normal random variate generator
    val sample = VectorD (for i <- 0 until m yield rvg.gen)   // sample from Normal distribution
    val (mu_, sig_) = (sample.mean, sample.stdev)             // sample mean and standard deviation
    println (s"(mu_, sig_) = ($mu_, $sig_)")
    new Plot (null, sample)
    new Histogram (sample)

end SampleStats

