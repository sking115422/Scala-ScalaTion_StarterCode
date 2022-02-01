
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Hao Peng
 *  @version 2.0
 *  @date    Thu Jun 13 13:13:26 EDT 2019
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Model: Simple Exponential Smoothing
 *
 *  @see https://otexts.com/fpp2/ses.html
 */

package scalation
package modeling
package forecasting

import scala.collection.mutable.Set

import scalation.mathstat._
import scalation.optimization.L_BFGS_B
import scalation.random._

import Fit._
//import RollingValidation.trSize

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SimpleExpSmoothing` class provide very basic time series analysis using
 *  Simple Exponential Smoothing models.  The forecasted value is the weighted
 *  average the latest value and the previous smoothed value.  The smoothing parameter
 *  α in [0, 1] causes the contributions of older values to decay exponentially.
 *  @see Smoothing Equation in section 7.1.
 *      s_t = α y_t-1 + (1 - α) s_t-1                          // smoothing equation
 *      yf_t = s_t                                             // forecast equation
 *  where vector s is the smoothed version of vector y.
 *  @param y       the response vector (orginal time series data)
 *  @param tt      the time vector, if relevant (time index may suffice)
 *  @param hparam  the hyper-parameters
 */
class SimpleExpSmoothing (y: VectorD, tt: VectorD = null,
                          hparam: HyperParameter = SimpleExpSmoothing.hp)
      extends Forecaster (y, tt, hparam)
         with Correlogram (y)
         with Fit (dfm = 1, df = y.dim - 1):

    private val debug = debugf ("SimpleExpSmoothing", true)            // debug function
    private val flaw  = flawf ("SimpleExpSmoothing")                   // flaw function
    private val TOL   = 1E-4                                           // tolerance
    private val lo    = VectorD.fill (1)(0.0)                          // lower bound on α for optimizer
    private val up    = VectorD.fill (1)(1.1)                          // upper bound on α for optimizer (1.0 with some slack)

    private var α     = hparam ("α").toDouble                          // default value for the smoothing parameter
    private var s     = VectorD.nullv                                  // vector of smoothed/leveled values (state)
    private var opt   = true                                           // whehtherf to optimize the smoothing parameter
    private var yf    = MatrixD.nullm                                  // the forecast matrix - time points x horizons

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the model name including its current hyper-parameter.
     */
    override def modelName: String = "SimpleExpSmoothing"

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reset the smoothing parameter α.
     *  @param a  the smoothing parameter
     */
    def reset (a: Double): Unit = { α = a }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Toggle the opt flag that indicates whether optimization should be used to set α.
     */
    def toggleOpt (): Unit = { opt = ! opt }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Smooth the time-series data 'y', returning the leveled/smoothed data 'l'.
     *  May be viewed as unoptimized training.
     *  @see Smoothing Equation in section 7.1.
     *      s_t = α y_t-1 + (1 - α) s_t-1                                // smoothing equation
     *  @param a   the smoothing parameter (decay rate for older values)
     *  @param y_  the response/output vector (training/full)
     */
    def smooth (a: Double = α, y_ : VectorD = y): VectorD =
        s = new VectorD (y_.dim)
        s(0) = y(0)
        for t <- 1 until y_.dim do s(t) = a * y_(t-1) + (1 - a) * s(t-1)
        s
    end smooth

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the `SimpleExpSmoothing` model on the time-series data, by finding the value
     *  for the smoothing parameter 'α' that minimizes the sum of squared errors 'sse'.
     *  @param x_null  the data/input matrix (ignored, pass null)
     *  @param y_      the response/output vector (training/full)
     */
    override def train (x_null: MatrixD, y_ : VectorD): Unit =             // FIX - not working
 
        //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /*  The objective function to be minimized (sum of squared errors) by `L_BFGS_B`.
         *  @param x  the input vector 'VectorD (α)' to be optimized
         */
        def f_obj (x: VectorD): Double = (y_ - smooth (x(0), y_)).normSq   // only one parameter

        if opt then
            val optimizer = new L_BFGS_B (f_obj, l = lo, u = up)           // Quasi-Newton optimizer
            val opt = optimizer.solve (VectorD (α), toler = TOL)           // optimize value for α
            α = (opt._2)(0)                                                // pull α from vector result
        end if
        s = smooth (α)                                                     // vector of smoothed/predicted values, with optimized α
        println (s" train: diagnose = ${diagnose (y_, s)}")
        debug ("train", s"α = $α")
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
        val yp = s(0 to y_.dim)                                        // align with y_
        e = y_ - yp                                                    // set the residuals/errors
        resetDF (1, y_.dim - 1)                                        // reset the degrees of freedom
        (yp, diagnose (y_, yp))                                        // return predictions and QoF vector
    end test

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the parameter vector.
     */
    override def parameter: VectorD = VectorD (α)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Predict a value for time point/index t using 1-step ahead forecasts.
     *      y_t = y_t-1
     *  @see predictAll in `Forecaster`
     *  @param t   the time point/index to be predicted
     *  @param y_  the actual values to use in making predictions
     */
    def predict (t: Int, y_ : VectorD): Double =
        if t < 1 || t > y_.dim then flaw ("predict", s"time index t = $t is out of range")
        s(t)
    end predict

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the vector of predicted values on the training data.
     *  Must call 'smooth' or 'train' first.
     *  @param y_  the actual values to use in making predictions
     */
    override def predictAll (y_ : VectorD): VectorD = s

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Forecast values for all m time points and all horizons (1 through h-steps ahead).
     *  Record these in the yf matrix, where
     *      yf(t, k) = k-steps ahead forecast for y_t
     *  Note, yf(?, 0) is set to y (the actual time-series values).
     *  @param h   the maximum forecasting horizon, number of steps ahead to produce forecasts
     *  @param y_  the actual values to use in making predictions
     */
    def forecastAll (h: Int, y_ : VectorD): MatrixD =
        val m = y.dim
        yf     = new MatrixD (m, h+1)                                    // forecasts for all time points t & horizons to h
        val sf = new VectorD (h+1)
        yf(?, 0) = y                                                     // first column is actual values, horizon 0
        for k <- 1 to h do
            yf(0, k) = y(0)                                              // copy first c actual values
            sf(0)    = s(0)
            for t <- 1 until m do                                        // forecast the rest
                 yf(t, k) = sf(k-1)
                 sf(k)    = α * yf(t-1, k-1) + (1 - α) * sf(k-1)
            end for
            debug ("forecastAll", s"yf(?, $k) = ${yf(?, k)}")
        end for
        yf                                                               // return matrix of forecasted values
    end forecastAll

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /*  Produce a vector of size 'h', of 1 through 'h'-steps ahead forecasts for the model.
     *  @see Forecasting Equation in section 7.1.
     *  @param t  the time point from which to make forecasts
     *  @param h  the forecasting horizon, number of steps ahead to produce forecasts
     *
    override def forecast (t: Int, h: Int = 1): VectorD =
    {
        val yf = new VectorD (h+1)
        
        yf(0) = if (t == 0) y(0) else s(t-1)
        sf(0) = s(t)
        for (k <- 1 to h) {
            yf(k) = sf(k-1)
            sf(k) = α * yf(k-1) + (1 - α) * sf(k-1)
        } // for
        yf.slice (1)
    } // forecast
     */

end SimpleExpSmoothing
 

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SimpleExpSmoothing` companion object provides factory methods for the
 *  `SimpleExpSmoothing` class.
 */
object SimpleExpSmoothing:

    /** Base hyper-parameter specification for `SimpleExpSmoothing`
     */
    val hp = new HyperParameter;
    hp += ("α", 0.5, 0.5)                                        // default value for the smoothing parameter

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `SimpleExpSmoothing` object.
     *  @param y       the response vector (time series data)
     *  @param tt      the time vector, if relevant (time index may suffice)
     *  @param hparam  the hyper-parameters
     */
    def apply (y: VectorD, tt: VectorD = null, hparam: HyperParameter = hp): SimpleExpSmoothing =
        new SimpleExpSmoothing (y, tt, hparam)
    end apply

end SimpleExpSmoothing


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `simpleExpSmoothingTest` main function is used to test the `SimpleExpSmoothing` class.
 *  Forecasting lake levels.
 *  @see cran.r-project.org/web/packages/fpp/fpp.pdf
 *  > runMain scalation.modeling.forecasting.simpleExpSmoothingTest
 */
@main def simpleExpSmoothingTest (): Unit =

    import Example_LakeLevels.y
   
    val mod = new SimpleExpSmoothing (y)                         // time series model SimpleExpSmoothing
    val ar  = new AR (y)                                         // time series model AR(1)
   
    // Build AR(1) and SimpleExpSmoothing models for the time series data
   
    banner (s"Build AR(1) model")
    ar.train (null, y)
    val yp = ar.testPred (y)

    banner (s"Build SimpleExpSmoothing model")
    mod.train (null, y)
    val yp2 = mod.testPred (y)

    val yp3 = (yp + yp2) / 2.0
    mod.testPred (y, null, (yp3, mod.diagnose (y(0 to yp3.dim), yp3)))

/*
    for h <- 1 to 4 do                                           // h-steps ahead  forecast
        banner (s"Rolling Validation h = $h")
        val stats = SimpleRollingValidation.crossValidate2 (mod, kt_ = 1, h = h)
        Fit.showQofStatTable (stats)
    end for
*/

end simpleExpSmoothingTest


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `simpleExpSmoothingTest2` main function is used to test the `SimpleExpSmoothing` class.
 *  Test customized smoothing (call 'smooth') versus optimized smoothing (call 'train').
 *  > runMain scalation.modeling.forecasting.simpleExpSmoothingTest2
 */
@main def simpleExpSmoothingTest2 (): Unit =

    val m = 50
    val r = Random ()
    val y = VectorD (for (i <- 0 until m) yield i + 10.0 * r.gen)
//  val r2 = RandomVecTrend (m, noise = Uniform (0.0, 10.0))
//  val y2 = r2.gen
//  new Plot (null, y, y2, "RandomVecTrend")
    println (s"y = $y")

    val mod = new SimpleExpSmoothing (y)                         // smooth time series data: y vs. t

    banner ("Customized Simple Exponential Smoothing")
    mod.smooth (0.5)                                             // use customized parameters, don't train
    var yp = mod.testPred (y)
    println (s"yp = $yp")

    banner ("Optimized Simple Exponential Smoothing")
    mod.train (null, y)                                          // train to use optimal α
    yp = mod.testPred (y)
    println (s"yp = $yp")

end simpleExpSmoothingTest2


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `simpleExpSmoothingTest2` main function is used to test the `SimpleExpSmoothing` class.
 *  Test rolling validation.
 *  > runMain scalation.modeling.forecasting.simpleExpSmoothingTest3
 */
@main def simpleExpSmoothingTest3 (): Unit =

    val m = 50
    val r = Random ()
    val y = VectorD (for (i <- 0 until m) yield i + 10.0 * r.gen)
    val h = 3
    println (s"y = $y")

    val mod = new SimpleExpSmoothing (y)                         // smooth time series data: y vs. t

    banner ("Optimized Simple Exponential Smoothing")
    mod.train (null, y)                                          // train to use optimal α
    val yp = mod.testPred (y)
    println (s"yp = $yp")

    val yf = mod.forecastAll (h, y)
    for k <- 1 to h do                                          // h-steps ahead  forecast
        banner (s"forecastAll h = $h")
        new Plot (null, y, yf(k), s"SES: Plot y and yf(${k})", lines = true)

/*
        banner (s"Rolling Validation h = $h")
        val stats = SimpleRollingValidation.crossValidate2 (mod, kt_ = 1, h = h)
        Fit.showQofStatTable (stats)
*/
    end for

end simpleExpSmoothingTest3


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `simpleExpSmoothingTest4` main function is used to test the `SimpleExpSmoothing` class.
 *  Forecasting lake levels for several values of the smoothing parameter α.
 *  @see cran.r-project.org/web/packages/fpp/fpp.pdf
 *  > runMain scalation.modeling.forecasting.simpleExpSmoothingTest4
 */
@main def simpleExpSmoothingTest4 (): Unit =

    import Example_LakeLevels.{t, y}

    val mod = new SimpleExpSmoothing (y)                         // time series model SimpleExpSmoothing
    mod.toggleOpt ()                                             // switch auto optimization off

    for i <- 0 to 5 do
        val a = i.toDouble / 5.0
        banner (s"Build SimpleExpSmoothing model with α = $a")
        mod.reset (a)
        mod.train (null, y)
        mod.testPred (y)
    end for

end simpleExpSmoothingTest4

