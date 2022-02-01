
package my_scalation

import scala.math.sqrt

import scalation._
import scalation.mathstat._
import scalation.random._

// > runMain my_scalation.ConfidenceIntervalTest

object ConfidenceIntervalTest extends App:

    val (mu, sig) = (70.0, 8.0)                               // population mean and standard deviation
    val m   = 100                                             // sample size
    val rm  = sqrt (m)
    val rvg = Normal (mu, sig * sig)                          // Normal random variate generator
    var count_z, count_t = 0

    for it <- 1 to 100 do                                     // test several datasets
        val y = VectorD (for i <- 0 until m yield rvg.gen)    // sample from Normal distribution
        val (mu_, sig_) = (y.mean, y.stdev)                   // sample mean and standard deviation

        val ihw_z = z_sigma (sig_) / rm                       // interval half width: z-distribution
        val ci_z  = (mu_ - ihw_z, mu_ + ihw_z)                // z-confidence interval
        println (s"mu = $mu in ci_z = $ci_z?")
        if mu in ci_z then count_z += 1

        val ihw_t = t_sigma (sig_, m-1) / rm                  // interval half width: t-distribution
        val ci_t  = (mu_ - ihw_t, mu_ + ihw_t)                // z-confidence interval
        println (s"mu = $mu in ci_t = $ci_t?")
        if mu in ci_t then count_t += 1
    end for

    println (s"mu inside $count_z % z-confidence intervals")
    println (s"mu inside $count_t % t-confidence intervals")

end ConfidenceIntervalTest

