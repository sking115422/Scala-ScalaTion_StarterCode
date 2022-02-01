
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Mon Sep 24 19:00:23 EDT 2012
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Model: Simple Linear Regression (one variable, two parameters)
 */

package scalation
package modeling

import scala.collection.mutable.IndexedSeq

import scalation.mathstat._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SimpleRegression` class supports simple linear regression.  In this case,
 *  the vector x consists of the constant one and a single variable x1, i.e.,
 *  (1, x1).  Fit the parameter vector 'b' in the regression equation
 *      y  =  b dot x + e  =  [b0, b1] dot [1, x1] + e  =  b0 + b1 * x1 + e
 *  where e represents the residuals (the part not explained by the model).
 *  @param x       the data/input matrix augmented with a first column of ones
 *                 (only use the first two columns [1, x1])
 *  @param y       the response/output vector
 *  @param fname_  the feature/variable names (only use the first two names)
 */
class SimpleRegression (x: MatrixD, y: VectorD, fname_ : Array [String] = null)
      extends Predictor (x, y, if fname_ == null then null else fname_.slice (0, 2), null)
         with Fit (dfm = 1, df = x.dim - 2):

    private val debug = debugf ("SimpleRegression", true)          // debug function
    private val flaw  = flawf ("SimpleRegression")                 // flaw function

    modelName = "SimpleRegression"

    if x.dim2 < 2 then flaw ("init", s"data matrix must have at least 2 columns: ${x.dim2}")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the predictor by fitting the parameter vector (b-vector) in the
     *  simple regression equation
     *      y = b dot x + e  = [b0, b1] dot [1, x1] + e
     *  using the least squares method.
     *  @see www.analyzemath.com/statistics/linear_regression.html
     *  @param x_  the training/full data/input matrix
     *  @param y_  the training/full response/output vector
     */
    def train (x_ : MatrixD = x, y_ : VectorD = y): Unit =
        val m   = x_.dim                                           // number of rows/instances
        val x1  = x_(?, 1)                                         // get column 1 of x = [1.0, x1]
        val sx  = x1.sum                                           // sum of x values
        val sy  = y_.sum                                           // sum of y values
        val ssx = x1 dot x1                                        // sum of squares x
        val ssy = y_ dot y_                                        // sum of squares y
        val sxy = x1 dot y_                                        // sum of cross products

        b = new VectorD (2)                                        // parameter vector [b0, b1]
        b(1) = (m * sxy - sx * sy) / (m * ssx - sx*sx)             // slope
        b(0) = (sy - b(1) * sx) / m                                // intercept
    end train

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test a predictive model y_ = f(x_) + e and return its QoF vector.
     *  Testing may be be in-sample (on the training set) or out-of-sample
     *  (on the testing set) as determined by the parameters passed in.
     *  Note: must call train before test.
     *  @param x_  the testing/full data/input matrix (defaults to full x)
     *  @param y_  the testing/full response/output vector (defaults to full y)
     */
    def test (x_ : MatrixD = x, y_ : VectorD = y): (VectorD, VectorD) =
        val yp = predict (x_)                                      // make predictions
        (yp, diagnose (y_, yp))                                    // return predictions and QoF vector
    end test

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Produce a QoF summary for a model with diagnostics for each predictor 'x_0', 'x_1',
     *  and the overall Quality of Fit (QoF).
     *  @param x_     the testing/full data/input matrix
     *  @param fname  the array of feature/variable names
     *  @param b      the parameters/coefficients for the model
     *  @param vifs   the Variance Inflation Factors (VIFs)
     */
    override def summary (x_ : MatrixD = getX, fname_ : Array [String] = fname, b_ : VectorD = b,
                          vifs: VectorD = vif ()): String =
        super.summary (x_, fname_, b_, vifs)                       // summary from `Fit`
    end summary

end SimpleRegression


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SimpleRegression` companion object provides a simple factory method
 *  for building simple regression linear regression models.
 */
object SimpleRegression:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a Simple Linear Regression model from a combined data matrix.
     *  The first column of matrix xy should have all ones corresponding to the intercept
     *  (matrix from has two column vectors [1 x]).
     *  Take the first two columns for the predictor and the last column for the response.
     *  @see `SimplerRegression` for a model without an intercept parameter
     *  @param xy     the combined data matrix
     *  @param fname  the feature/variable names
     */
    def apply (xy: MatrixD, fname: Array [String] = null): SimpleRegression =
        new SimpleRegression (xy(?, 0 to 2), xy(?, xy.dim2-1), fname)
    end apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a Simple Linear Regression model, automatically prepending the
     *  column of ones (form matrix from two column vectors [1 x]).
     *  @param x      the data/input m-by-1 vector
     *  @param y      the response/output m-vector
     *  @param fname  the feature/variable names
     */
    def apply (x: VectorD, y: VectorD, fname: Array [String]): SimpleRegression =
        new SimpleRegression (MatrixD.one (x.dim) :^+ x, y, fname)
    end apply

end SimpleRegression


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `simpleRegressionTest` main function to test the `SimpleRegression` class.
 *      y = b0 + b1 * x
 *  > runMain scalation.modeling.simpleRegressionTest
 */
@main def simpleRegressionTest (): Unit =

    // 4 data points: constant x1
    val x = MatrixD ((4, 2), 1, 1,                                 // x 4-by-2 matrix
                             1, 2,
                             1, 3,
                             1, 4)
    val y = VectorD (1, 3, 3, 4)                                   // y vector

    banner ("Test1: Simple Regression Model: y = b_0 + b_1 x + e")
    println (s"x = $x")
    println (s"y = $y")

    val mod = new SimpleRegression (x, y)                          // create a simple regression model
    mod.trainNtest ()()                                            // train and test the model

end simpleRegressionTest


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `simpleRegressionTest2` main function to test the `SimpleRegression` class.
 *      y = b0 + b1 * x
 *  > runMain scalation.modeling.simpleRegressionTest2
 */
@main def simpleRegressionTest2 (): Unit =

    // 4 data points:
    val x = VectorD (1, 2, 3, 4)
    val y = VectorD (1, 3, 3, 4)

    banner ("Test2: Simple Regression Model: y = b_0 + b_1 x + e")
    println (s"x = $x")
    println (s"y = $y")

    val mod = SimpleRegression (x, y, null)                        // automatically prepends a column of ones
    mod.trainNtest ()()                                            // train and test the model

end simpleRegressionTest2


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `simpleRegressionTest3` main function is used to test the `SimpleRegression` class.
 *      y = b dot x = [b0, b1] dot [1, x1]
 *  @see www.analyzemath.com/statistics/linear_regression.html
 *  > runMain scalation.modeling.simpleRegressionTest3
 */
@main def simpleRegressionTest3 (): Unit =

    // 5 data points:   constant   x1
    val x = MatrixD ((5, 2), 1.0, 0.0,                             // x 5-by-2 matrix
                             1.0, 1.0,
                             1.0, 2.0,
                             1.0, 3.0,
                             1.0, 4.0)
    val y = VectorD (2.0, 3.0, 5.0, 4.0, 6.0)                      // y vector

    banner ("Test3: Simple Regression Model: y = b_0 + b_1 x + e")
    println (s"x = $x")
    println (s"y = $y")

    val mod = new SimpleRegression (x, y)                          // create a simple regression model
    mod.trainNtest ()()                                            // train and test the model

    val z = VectorD (1.0, 5.0)                                     // predict y for new point z
    println (s"predict ($z) = ${mod.predict (z)}")

    val yp = mod.predict (x)
    new Plot (x(?, 1), y, yp, "plot y and yp vs. x", lines = true)

end simpleRegressionTest3


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `simpleRegressionTest4` main function is used to test the `SimpleRegression` class.
 *      y = b dot x = b0 + b1 * x1
 *  @see mathbits.com/mathbits/tisection/Statistics2/linear.htm
 *  > runMain scalation.modeling.simpleRegressionTest4
 */
@main def simpleRegressionTest4 (): Unit =

    // 20 data points: just x1 coordinate
    val x1 = VectorD (  4.0,   9.0,  10.0,  14.0,   4.0,   7.0,  12.0,  22.0,   1.0,   3.0,
                        8.0,  11.0,   5.0,   6.0,  10.0,  11.0,  16.0,  13.0,  13.0,  10.0)
    val y  = VectorD (390.0, 580.0, 650.0, 730.0, 410.0, 530.0, 600.0, 790.0, 350.0, 400.0,
                      590.0, 640.0, 450.0, 520.0, 690.0, 690.0, 770.0, 700.0, 730.0, 640.0)

    val x = MatrixD.one (x1.dim) :^+ x1                            // form matrix x from vector x1

    banner ("Test4: Simple Regression Model: y = b_0 + b_1 x + e")
    println (s"x = $x")
    println (s"y = $y")

    val mod = new SimpleRegression (x, y)                          // create a simple regression model
    mod.trainNtest ()()                                            // train and test the model

    val z  = VectorD (1.0, 15.0)                                   // predict y for new point z
    println (s"predict ($z) = ${mod.predict (z)}")

    val yp = mod.predict (x)
    new Plot (x1, y, yp, "plot y and yp vs. x1", lines = true)

end simpleRegressionTest4


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `simpleRegressionTest5` main function is used to test the `SimpleRegression` class.
 *      y = b dot x = b0 + b1 * x1
 *  This version uses gradient descent to search for the optimal solution for b.
 *  > runMain scalation.modeling.simpleRegressionTest5
 */
@main def simpleRegressionTest5 (): Unit =

    // 4 data points:
    val x  = MatrixD ((4, 2), 1, 1,
                              1, 2,
                              1, 3,
                              1, 4)
    val x1 = x(?, 1)
    val y  = VectorD (1, 3, 3, 4)
    val _1 = VectorD.one (x1.dim)

    println (s"x = $x")

    val ITER = 10                                                  // number of iterations
    val eta  = 0.02                                                // try different values for the learning rate
    val mod  = new SimpleRegression (x, y)                         // create a simple regression model, don't train
    var b    = new VectorD (x.dim2)                                // starting point [0, 0] for parameter vector b

    banner (s"Test5: Simple Regression Model: gradient descent: eta = $eta")
    for it <- 1 to ITER do
        val yp = x * b
        val e  = y - yp
        val g  = VectorD (_1 dot e, x1 dot e)
        b     += g * eta
        val sse = e dot e
        println (s"for iteration $it, b = $b, sse = $sse")
    end for

end simpleRegressionTest5


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `simpleRegressionTest6` main function is used to test the `SimpleRegression` class.
 *      y = b dot x = b0 + b1 * x1
 *  This version does Exploratory Data Analysis (EDA) on the AutoMPG dataset.
 *  > runMain scalation.modeling.simpleRegressionTest6
 */
@main def simpleRegressionTest6 (): Unit =

    import Example_AutoMPG.{x, y}

    banner ("Plot response y vs. row index t")
    val nm = new NullModel (y)
    nm.trainNtest ()()                                             // train and test the model
    val yp = nm.predict (x)
    new Plot (null, y, yp, "EDA: y and yp (red) vs. t", lines = true)

    banner ("Correlation Matrix for columns of x")
    println (s"x.corr = ${x.corr}")

    for j <- x.indices2 do
        banner (s"Plot response y vs. predictor variable x$j")
        val xj = x(?, j)
        val mod = SimpleRegression (xj, y, null)
        mod.trainNtest ()()                                        // train and test the model
        val yp = mod.predict (mod.getX)
        new Plot (xj, y, yp, s"EDA: y and yp (red) vs. x$j", lines = true)
    end for

end simpleRegressionTest6

