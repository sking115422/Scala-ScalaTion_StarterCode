
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Hao Peng
 *  @version 2.0
 *  @date    Sat Dec  8 14:32:12 EST 2018
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Model Framework: ACF and PACF
 */

package scalation
package modeling
package forecasting

import scala.math.{min, sqrt}

import scalation.mathstat._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Stats` case class is used to hold basic statistical information:
 *  mean, variance, auto-covariance, and auto-correlation.
 *  Note gamma0 (biased) does not equal the sample variance (unbiased)
 *  @param y     the response vector (time-series data) for the training/full dataset
 *  @param lags  the maximum number of lags
 */
case class Stats4TS (y: VectorD, lags: Int):

   val mu   = y.mean                                 // sample mean
   val sig2 = y.variance                             // sample variance
   val acv  = new VectorD (lags + 1)                 // auto-covariance vector
   for k <- acv.indices do acv(k) = y acov k         // k-th lag auto-covariance
   val acr  = acv / acv(0)                           // auto-correlation function

   //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
   /** Convert a `Stats4TS` object to a string.
    */
   override def toString: String =
       s"Stats (m = ${y.dim}, mu = $mu, sig2 = $sig2, acr = $acr)"
   end toString

end Stats4TS

val MAX_LAGS = 39                                    // maximum amount of lag supported

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Correlogram` trait provides functions for computing ACF and PACF.
 */
trait Correlogram (y: VectorD):

    private val ml = min (y.dim, MAX_LAGS)           // maximum lag to consider
    private var stats: Stats4TS = null               // statistics on time-series y
    private var psi:   MatrixD  = null               // pass in auto-covariance and max lags to Durbin-Levinson
    private var pacf:  VectorD  = null               // Partial Auto-Correlation Function (PACF) is main diagonal

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Make a Correlogram, i.e., compute stats, psi and pacf.
     *  @param y_  the current (e.g., training) times-series to use (defaults to full y)
     */
    def makeCorrelogram (y_ : VectorD = y): Unit =
        stats = Stats4TS (y, ml)
        psi   = durbinLevinson (stats.acv, ml)
        pacf  = psi(?)
    end makeCorrelogram

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the autocorrelation vector (ACF).
     */
    def acF: VectorD  = stats.acr

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the partial autocorrelation vector (PACF).
     */
    def pacF: VectorD = pacf

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the psi matrix.
     */
    def psiM: MatrixD = psi

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return basic statistics on time-series y or y_.
     */
    def statsF: Stats4TS = stats

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Apply the Durbin-Levinson Algorithm to iteratively compute the psi matrix.
     *  The last/p-th row of the matrix gives 'AR' coefficients.
     *  Note, also known as Levinson-Durbin.
     *  @see www.stat.tamu.edu/~suhasini/teaching673/time_series.pdf, p. 247
     *  @param g   the auto-covariance vector (gamma)
     *  @param ml  the maximum number of lags
     */
    private def durbinLevinson (g: VectorD, ml: Int): MatrixD =
        val psi_ = new MatrixD (ml+1, ml+1)                    // psi matrix (ml = max lags)
        val r    = new VectorD (ml+1); r(0) = g(0)

        for k <- 1 to ml do                                    // range up to max lags
            var sum = 0.0
            for (j <- 1 until k) sum += psi_(k-1, j) * g(k-j)
            val a = (g(k) - sum) / r(k-1)
            psi_(k, k) = a
            for j <- 1 until k do
                psi_(k, j) = psi_(k-1, j) - a * psi_(k-1, k-j)
            end for
            r(k) = r(k-1) * (1.0 - a * a)
        end for
        psi_
    end durbinLevinson

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Plot a function, e.g., Auto-Correlation Function (ACF), Partial Auto-Correlation
     *  Function (PACF) with confidence bound.
     *  @param fVec  the vector given function values
     *  @param name  the name of the function
     *  @param show  whether to show the fVec values
     */
    def plotFunc (fVec: VectorD, name: String, show: Boolean = true): Unit =
        val lag_axis = VectorD.range (0, ml+1)
        val zero     = new VectorD (ml+1)
        val bound    = VectorD (for k <- 0 to ml yield 1.96 / sqrt (y.dim - k))
        val mat      = MatrixD (fVec, zero, bound, -bound)
        new PlotM (lag_axis, mat, Array ("fVec", "zero", "bound"), "PlotM of " + name, true)
        if (show) println (s"$name: fVec = $fVec")
    end plotFunc

end Correlogram


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `correlogramTest` main function tests the `Correlogram` trait.
 *  > runMain scalation.modeling.forecasting.correlogramTest
 */
@main def correlogramTest (): Unit =

    import Example_LakeLevels.y

    banner ("Test Stats4TS")
    val stats = Stats4TS (y, MAX_LAGS)
    println (stats)
    val zero = new VectorD (stats.acr.dim)
    new Plot (null, stats.acr, zero, "ACF vs. k", true) 

    class Tester (y: VectorD) extends Correlogram (y)

    banner ("Test Correlogram")
    val ct = new Tester (y)
    ct.makeCorrelogram ()
    val acf  = ct.acF
    val pacf = ct.pacF
    println (s"acF = $acf")
    println (s"pacF = $pacf")
    ct.plotFunc (acf, "ACF")
    ct.plotFunc (pacf, "PACF")
    
end correlogramTest

