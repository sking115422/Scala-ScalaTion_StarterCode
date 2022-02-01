
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Sun Jun  9 16:42:16 EDT 2019
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Model Framework: Base Trait for all Models
 */

package scalation
package modeling

import java.net.URI

import scala.runtime.ScalaRunTime.stringOf

import scalation.mathstat._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Model` trait provides a common framework for all models and serves as
 *  base trait for `Classifier`, `Forecaster` and `Predictor` traits.
 *  The train and test methods must be called first, e.g.,
 *       val model = NullModel (y)
 *       model.train (null, y)
 *       model.test (null, y)
 */
trait Model:

    /** The optional reference to an ontological concept
     */
    var modelConcept: URI = null

    /** The name for the model (or modeling technique).
     */
    var modelName: String = "Model"

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the feature/variable names.
     */
    def getFname: Array [String]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the model 'y_ = f(x_) + e' on a given dataset, by optimizing the model
     *  parameters in order to minimize error '||e||' or maximize log-likelihood 'll'.
     *  @param x_  the training/full data/input matrix (impl. classes may default to x)
     *  @param y_  the training/full response/output vector (impl. classes may default to y)
     */
    def train (x_ : MatrixD, y_ : VectorD): Unit

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test/evaluate the model's Quality of Fit (QoF) and return the predictions
     *  and QoF vectors.
     *  This may include the importance of its parameters (e.g., if 0 is in a parameter's
     *  confidence interval, it is a candidate for removal from the model).
     *  Extending traits and classess should implement various diagnostics for
     *  the test and full (training + test) datasets.
     *  @param x_  the testiing/full data/input matrix (impl. classes may default to x)
     *  @param y_  the testiing/full response/output vector (impl. classes may default to y)
     */
    def test (x_ : MatrixD, y_ : VectorD): (VectorD, VectorD)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Predict the value of y = f(z) by evaluating the model equation.
     *  @param z  the new vector to predict
     */
    def predict (z: VectorD): Double

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the model hyper-parameters (if none, return null).  Hyper-parameters
     *  may be used to regularize parameters or tune the optimizer.
     */
    def hparameter: HyperParameter

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the vector of model parameter/coefficient values.
     */
    def parameter: VectorD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return a basic report on a trained and tested model.
     *  @param ftVec  the vector of qof values produced by the `Fit` trait
     */
    def report (ftVec: VectorD): String =
        s"""
REPORT
    modelName  mn  = $modelName
    hparameter hp  = $hparameter
    features   fn  = ${stringOf (getFname)}
    parameter  b   = $parameter
    fitMap     qof = ${Fit.fitMap (ftVec)}
        """
    end report

end Model

