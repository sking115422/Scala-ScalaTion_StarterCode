

///////////////////////////////////////////// 
//INCLUDING SCALATION ON A NEW PROJECT
/////////////////////////////////////////////


/*

Method 1: Use a copy of Dr. Miller pre provided file my_scalation_2.0 from his webpage. Then change make a new scala class file in the package dir and edit there. 

Method 2: Start from hello world boiler plate template. Create a new dir under the main project folder called "lib" that has the "scalation_3-0.1.0-SNAPSHOT.jar" file in it.
you then need to navigate to ".bloop/root/". From here thre should be a "root-test.json" and a "root.json file". For both of these files you must add in a classpath so that Scala knows
how to access the scalation jar file. For example, I would add "/Users/sdk/Desktop/my_scalation_2.0/lib/scalation_3-0.1.0-SNAPSHOT.jar" as another element under the classpath json attribute. 

*/


//// HW3 Chapter 3.13 Exercise 14 


package hw3

import scala.math.sqrt
import scala.language.postfixOps
import java.io._

import scalation._
import scalation.mathstat._
import scalation.random._


def confidenceIntervalTest (s_size:Int): Unit =

  val (mu, sig) = (70.0, 8.0) // pop. mean and standard deviation
  val m = s_size // sample size
  val rm = sqrt (m)
  val rvg = Normal (mu, sig * sig) // Normal random variate generator
  var count_z, count_t = 0

  for it <- 1 to 100 do // test several datasets

    val y = VectorD (for i <- 0 until m yield rvg.gen) // sample from Normal distribution
    val (mu_, sig_) = (y.mean, y.stdev) // sample mean and standard deviation

    val ihw_z = z_sigma (sig_) / rm // interval half width: z-distribution
    val ci_z = (mu_ - ihw_z, mu_ + ihw_z) // z-confidence interval
    println (s"mu = $mu in ci_z = $ci_z?")

    if mu in ci_z then count_z += 1

    val ihw_t = t_sigma (sig_, m-1) / rm // interval half width: t-distribution
    val ci_t = (mu_ - ihw_t, mu_ + ihw_t) // z-confidence interval
    println (s"mu = $mu in ci_t = $ci_t?")

    if mu in ci_t then count_t += 1

    // val writer2 = new FileWriter(new File("./src/main/scala/hw3/output/ci_test_data.csv"), true)
    // writer2.write(s"$s_size, ${ci_z(0)}, ${ci_z(1)}, ${ci_t(0)}, ${ci_t(1)}\n")
    // writer2.close()

  end for

  println (s"mu inside $count_z % z-confidence intervals")
  println (s"mu inside $count_t % t-confidence intervals")

  // println(s"$s_size, $count_z, $count_t")

  val writer = new FileWriter(new File("./src/main/scala/hw3/output/count_test_data.csv"), true)
  writer.write(s"$s_size, $count_z, $count_t\n")
  writer.close()





end confidenceIntervalTest


object testCI extends App:

  // val tmp1 = 20 to 40 by 1 toList

  // val tmp2 = List(60, 80, 100)

  // val tests = tmp1 ::: tmp2

  val tests = 10 to 100 by 1 toList

  val writer = new FileWriter(new File("./src/main/scala/hw3/output/count_test_data.csv"))
  writer.write(s"m, z-dist, t-dist\n")
  writer.close()  
  
  // val writer2 = new FileWriter(new File("./src/main/scala/hw3/output/ci_test_data.csv"))
  // writer2.write(s"m, z-ci-st, z-ci-end, t-ci-st, t-ci-end\n")
  // writer2.close()

  for (test <- tests) confidenceIntervalTest(test)

end testCI