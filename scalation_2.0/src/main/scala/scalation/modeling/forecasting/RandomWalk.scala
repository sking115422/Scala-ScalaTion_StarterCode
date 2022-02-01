
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Sat Jun 13 01:27:00 EST 2017
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Model: Random Walk (guess previous value)
 */

package scalation
package modeling
package forecasting

import scalation.mathstat._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RandomWalk` class provides basic time series analysis capabilities for 
 *  RandomWalk models. RandomWalk models are often used for forecasting.
 *  Given time series data stored in vector y, its next value y_t = y(t)
 *  may be predicted based on prior value of y and its noise:
 *      y_t = y_t-1 + e
 *  where e is the noise vector.
 *----------------------------------------------------------------------------------
 *  @param y       the response vector (time-series data)
 *  @param tt      the time vector, if relevant (time index may suffice)
 *  @param hparam  the hyper-parameters (none => use null)
 */
class RandomWalk (y: VectorD, tt: VectorD = null, hparam: HyperParameter = null)
      extends Forecaster (y, tt, hparam)
         with Correlogram (y)
         with Fit (dfm = 1, df = y.dim - 1):

    private val debug = debugf ("RandomWalk", true)                    // debug function
    private val flaw  = flawf ("RandomWalk")                           // flaw function
    private var m     = y.dim                                          // number of time points
    private var yf: MatrixD = null                                     // the forecast matrix - time points x horizons

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the model name including its current hyper-parameter, e.g., RandomWalk.
     */
    override def modelName: String = s"RandomWalk"

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train/fit a `RandomWalk` model to the times-series data in vector y_.
     *  @param x_null  the data/input matrix (ignored, pass null)
     *  @param y_      the training/full response vector (e.g., full y)
     */
    def train (x_null: MatrixD, y_ : VectorD): Unit = 
        m = y_.dim                                                     // length of relevant time-series
        resetDF (1, m - 1)                                             // reset the degrees of freedom
        makeCorrelogram (y_)                                           // correlogram computes psi matrix
        debug ("train", s"parameters for RandomWalk model: NA")
    end train

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test PREDICTIONS of an RandomWalk forecasting model y_ = f(lags (y_)) + e and return its
     *  QoF vector.  Testing may be be in-sample (on the training set) or out-of-sample
     *  (on the testing set) as determined by the parameters passed in.
     *  Note: must call train before test.
     *  @param x_null  the training/testing data/input matrix (ignored, pass null)
     *  @param y_      the training/testing response/output vector (e.g., full y)
     */
    def test (x_null: MatrixD, y_ : VectorD): (VectorD, VectorD) =
        val (yy, yp) = testSetup (y_)                                  // get and align actual and predicted values
        e = yy - yp                                                    // set the residuals/errors
        resetDF (1, yy.dim - 1)                                        // reset the degrees of freedom
        (yp, diagnose (yy, yp))                                        // return predictions and QoF vector
    end test

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test FORECASTS of an RandomWalk forecasting model y_ = f(lags (y_)) + e and return its
     *  QoF vector.  Testing may be be in-sample (on the training set) or out-of-sample
     *  (on the testing set) as determined by the parameters passed in.
     *  Note: must call train before test.
     *  @param h     the forecasting horizon, number of steps ahead to produce forecasts
     *  @param y_    the training/testing response/output vector (e.g., full y)
     *  @param redo  whether to use existing forecasts or redo them (defaults to false)
     */
    def testf (h: Int, y_ : VectorD, redo: Boolean = false): VectorD =
        if yf == null || yf.dim2 < h+1 || redo then yf = forecastAll (h, y_)    // redo all forecasts
        val yy   = y_(h to y_.dim) 
        val yf_h = yf(?, h)(h to y_.dim)                               // pull column h from the forecast matrix and align
        resetDF (1, yy.dim - 1)                                        // reset the degrees of freedom
        diagnose (yy, yf_h)                                            // evaluate and return the QoF of these forecasts
    end testf

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Predict a value for time point/index t using 1-step ahead forecasts.
     *      y_t = y_t-1 
     *  @see predictAll in `Forecaster`
     *  @param t   the time point/index to be predicted
     *  @param y_  the actual values to use in making predictions
     */
    def predict (t: Int, y_ : VectorD): Double =
        if t < 1 || t > y_.dim then flaw ("predict", s"time index t = $t is out of range")
        y_(t-1)
    end predict

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Forecast values for all y_.dim time points and all horizons (1 through h-steps ahead).
     *  Record these in the yf matrix, where
     *      yf(t, k) = k-steps ahead forecast for y_t
     *  Note, column 0, yf(?, 0), is set to y (the actual time-series values).
     *  Forecast recurse down diagonals in the yf forecast matrix.
     *  The top right and bottom left triangles in yf matrix are not forecastable.
     *  @param h   the maximum forecasting horizon, number of steps ahead to produce forecasts
     *  @param y_  the actual values to use in making forecasts
     */
    def forecastAll (h: Int, y_ : VectorD): MatrixD =
        yf = new MatrixD (y_.dim+h, h+1)                               // forecasts for all time points t & horizons to h
        for t <- 0 until m do yf(t, 0) = y_(t)                         // first column is actual values, horizon 0
        for k <- 1 to h do
            for t <- y_.indices do                                     // make forecasts over all time points for horizon k
                 yf(t+k, k) = yf(t+k-1, k-1)                           // forecast down the diagonal
            end for
            debug ("forecastAll", s"yf(?, $k) = ${yf(?, k)}")
        end for
        yf                                                             // return matrix of forecasted values
    end forecastAll

end RandomWalk


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RandomWalk` companion object provides factory methods for the `RandomWalk` class.
 */
object RandomWalk:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create an `RandomWalk` object.
     *  @param y       the response vector (time series data)
     *  @param tt      the time vector, if relevant (time index may suffice)
     *  @param hparam  the hyper-parameters
     */
    def apply (y: VectorD, tt: VectorD = null, hparam: HyperParameter = null): RandomWalk = 
        new RandomWalk (y, tt, hparam)
    end apply

end RandomWalk


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `randomWalkTest` main function tests the `RandomWalk` class on real data: Forecasting lake levels.
 *  Test the test, predictAll, testf and forecastAll methods over the whole times-series.
 *  @see cran.r-project.org/web/packages/fpp/fpp.pdf
 *  > runMain scalation.modeling.forecasting.randomWalkTest
 */
@main def randomWalkTest (): Unit =

    import Example_LakeLevels.y
    val m = y.dim
    val t = VectorD.range (1, m)
    val h = 3                                                          // the forecasting horizon
 
    banner (s"Test: RandomWalk")
    val mod = new RandomWalk (y)                                       // create model for time series data
    mod.train (null, y)                                                // train the model on full dataset
    val yp = mod.testPred (y, t)

    val yf = mod.forecastAll (h, y)                                    // forecast h-steps ahead for all y
    println (s"yf = $yf")
    println (s"yf.dims = ${yf.dims}")
    assert (yf(?, 0)(0 until m) == y)                                  // column 0 must agree with actual values
    assert (yf(?, 1)(1 to m+1) == yp)                                  // column 1 must agree with one step-ahead predictions
    for k <- 1 to h do
        println (s"evalaute QoF for horizon $k:")
        println (Fit.fitMap (mod.testf (k, y)))                        // evaluate k-units ahead forecasts
    end for

    banner ("Select model based on ACF and PACF")
    mod.plotFunc (mod.acF, "ACF")                                      // Auto-Correlation Function (ACF)
    mod.plotFunc (mod.pacF, "PACF")                                    // Partial Auto-Correlation Function (PACF)

end randomWalkTest

