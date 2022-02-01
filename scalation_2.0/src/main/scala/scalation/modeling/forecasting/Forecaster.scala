
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Hao Peng
 *  @version 2.0
 *  @date    Sat Dec  8 14:32:12 EST 2018
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Model Framework: Forecasters with Vector Input
 */

package scalation
package modeling
package forecasting

import scala.collection.mutable.Set
import scala.math.{abs, min, sqrt}
import scala.util.control.Breaks.{break, breakable}

import scalation.mathstat._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Forecaster` trait provides a common framework for several forecasters.
 *  Note, the train method must be called first followed by test.
 *  @param y       the response vector (time-series data)
 *  @param tt      the time vector, if relevant (time index may suffice)
 *  @param hparam  the hyper-parameters
 */
trait Forecaster (y: VectorD, tt: VectorD = null, hparam: HyperParameter = null)
      extends Model:

    private val debug = debugf ("Forecaster", true)                       // debug function
    private val flaw  = flawf ("Forecaster")                              // flaw function

    protected var e: VectorD = null                                       // residual/error vector [e_0, e_1, ... e_m-1]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the used response vector y.  Mainly for derived classes where y is
     *  transformed, e.g., `TranRegression`, `Regression4TS`.
     */
    def getY: VectorD = y

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the feature/variable names.  Override for models like SARIMAX.
     */
    def getFname: Array [String] = Array ("no-x features")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a time-series y_, train the forecasting function y_ = f(lags (y_)) + e,
     *  where f(lags (y_)) is a function of the lagged values of y_,
     *  by fitting its parameters.
     *  @param x_null  the training/testing data/input matrix (ignored, pass null)
     *  @param y_      the training/testing response/output vector (e.g., full y)
     */
    def train (x_null: MatrixD, y_ : VectorD): Unit

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test a forecasting model y_ = f(lags (y_)) + e and return its predictions
     *  and QoF vector.
     *  Testing may be be in-sample (on the training set) or out-of-sample
     *  (on the testing set) as determined by the parameters passed in.
     *  Note: must call train before test.
     *  @param x_null  the training/testing data/input matrix (ignored, pass null)
     *  @param y_      the training/testing response/output vector (e.g., full y)
     */
    def test (x_null: MatrixD, y_ : VectorD): (VectorD, VectorD)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set up testing by making predictions, aligning actual and predicted values,
     *  and computing residuals/errors.
     *  @param y_   the training/testing response/output vector (e.g., full y)
     */
    def testSetup (y_ : VectorD): (VectorD, VectorD) =
        val yp  = predictAll (y_)                                      // make predictions
        val yy  = y_(1 to y_.dim)
        val yyp = yp(0 to y_.dim-1)                                    // align actual and predicted vectors
        (yy, yyp)
    end testSetup

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test the in-sample QoF of the trained model and return the predicted values yp.
     *  @param y_      the training/testing response/output vector (e.g., full y)
     *  @param t       the time/time index for the plot's horizonatal axis
     *  @param yp_ft   the predicted response/output vector and its QoF, if given
     *  @param doPlot  whether to plot predicted and actual values vs. time t
     */
    def testPred (y_ : VectorD, t: VectorD = null, yp_ft: (VectorD, VectorD) = null,
                  doPlot: Boolean = true): VectorD =
        val (yp, ft) = if yp_ft == null then test (null, y_)           // compute predictions, QoF
                       else yp_ft                                      // use existing
        println (report (ft))
        if doPlot then new Plot (null, y_, yp, s"Plot of y, $modelName vs. t", true)
        yp
    end testPred

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the hyper-parameters.
     */
    def hparameter: HyperParameter = hparam

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the vector of parameter/coefficient values (they are model specific).
     *  Override for models with parameters.
     */
    def parameter: VectorD = null

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the vector of residuals/errors.
     */
    def residual: VectorD = { if e == null then flaw ("residual", "must call test method first"); e }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Predict a value for time point t using 1-step ahead forecasts.
     *      y_t  =  φ_0 y_t-1 + φ_1 y_t-2 + ... + φ_p-1 y_t-p
     *  When k < 0 let y_k = y_0 (assume first value repeats).
     *  @param tn  the time point (index) to be predicted
     *  @param y_  the actual values to use in making predictions
     */
    def predict (t: Int, y_ : VectorD): Double

    def predict (z: VectorD): Double =
        throw new UnsupportedOperationException ("predict (VectorD) use predictAll instead")
    end predict

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Predict values for all time points using 1-step ahead forecasts.
     *  Note, the returned predicted vector is time-shifted by one ahead of y_
     *      actual [0, m-1] vs. predicted [1, m]
     *  @param y_  the actual values to use in making predictions
     */
    def predictAll (y_ : VectorD): VectorD =
        VectorD (for t <- 1 to y_.dim yield predict (t, y_))
    end predictAll

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Produce a vector of size h, of 1 through h-steps ahead forecasts for the model.
     *      forecast the following time points:  t, t+1, ..., t-1+h.
     *  Note, invoke forecastAll first to create the yf matrix.
     *  @param t   the time point from which to make forecasts
     *  @param h   the forecasting horizon, number of steps ahead to produce forecasts
     */
    def forecast (t: Int, h: Int): VectorD = ???

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Forecast values for all time points using 1 through h-steps ahead forecasts.
     *  The h-th row of matrix is the horizon h forecast (where h = 0 is actual data).
     *  @param h   the forecasting horizon, number of steps ahead to produce forecasts, must be > 0
     *  @param y_  the actual values to use in making forecasts
     */
    def forecastAll (h: Int, y_ : VectorD): MatrixD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform forward selection to find the most predictive variable to add the
     *  existing model, returning the variable to add and the new model.
     *  May be called repeatedly.
     *  Note, all lags up and including 'p|q' define the model.
     *  @see `Fit` for index of QoF measures.
     *  @param cols   the lags/columns currently included in the existing model (currently ignored)
     *  @param idx_q  index of Quality of Fit (QoF) to use for comparing quality
     */
    def forwardSel (cols: Set [Int], idx_q: Int = QoF.rSqBar.ordinal): (Int, Forecaster) = ???

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform forward selection to find the most predictive lags/variables to have
     *  in the model, returning the variables added and the new Quality of Fit (QoF)
     *  measures for all steps.
     *  @see `Fit` for index of QoF measures.
     *  @param idx_q  index of Quality of Fit (QoF) to use for comparing quality
     *  @param cross  whether to include the cross-validation QoF measure (currently ignored)
     *
    def forwardSelAll (idx_q: Int = QoF.rSq.ordinal, cross: Boolean = false): (Set [Int], MatrixD) =
    {
        val rSq  = new MatrixD (MAX_LAGS, 3)                              // R^2, R^2 Bar, R^2 cv
        val cols = Set (1)                                                // start with lag1 in model

        println (s"forwardSelAll (l = 0): cols = $cols")
        breakable { for (l <- 2 until MAX_LAGS) {
            val (j, mod_j) = forwardSel (cols, index_q)                   // add most predictive variable
            if (j == -1) break ()
            cols     += j                                                 // add variable x_j
            val fit_j = mod_j.fit
            rSq(l)    = Fit.qofVector (fit_j, null)                       // use new model, mod_j, no cross
            if (true) { // (DEBUG) {
                val k = cols.size - 1
                println (s"==> forwardSelAll (l = $l): add (#$k) variable $j, cols = $cols, qof = ${fit_j(index_q)}")
            } // if
        }} // breakable for

        (cols, rSq.slice (0, cols.size-1))
    } // forwardSelAll
     */

end Forecaster


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Forecaster` companion object provides functions useful for forecasting
 */
object Forecaster:

    private val flaw = flawf ("Forecaster")                               // flaw function

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Point out the differences between two vectors/time-series.
     *  @param u      the first vector/time-series
     *  @param v      the second vector/time-series
     *  @param scale  the scale factor to set the tolerance 'tol'
     *  @param allow  flag indicating whether allow (via assert) any differences
     */
    def differ (u: VectorD, v: VectorD, scale: Double = 1E-9, allow: Boolean = true): Int =
        if u.dim != v.dim then flaw ("differ", s"requires u.dim = ${u.dim} = v.dim = ${v.dim}")
        val tol = u.mean * scale
        var cnt = 0
//      for t <- u.indices if u(t) !=~ v(t) do                            // machine epsilon
        for t <- u.indices if abs (u(t) - v(t)) > tol do                  // application tolerance
            cnt += 1
            println (s"differ at t = $t: ${u(t)} \t ${v(t)}")
        end for
        banner (s"differ (u, v): found $cnt points that differ")
        if ! allow then assert (cnt == 0)
        cnt
    end differ

end Forecaster

