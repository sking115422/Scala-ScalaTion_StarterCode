
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0 
 *  @date    Sat Aug 29 14:14:32 EDT 2020
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   ValueType - union datatype for atomic database values
 */

package scalation

import java.lang.Double.isNaN

import scala.collection.mutable.Map

//import scala.collection.immutable.{Vector => VEC}                       // for immutable
import scala.collection.mutable.{ArrayBuffer => VEC}                      // for mutable

import scala.math.{abs, max, min, pow, Pi, sqrt}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/*  Top-level definitions of values/constants and functions related to basic datatypes.
 *  @see CommonFunctions.scala for Top-level definitions of common scalar functions.
 *  @see Util.scala for top-level definitions utility functions.
 */

/** Smallest positive normal value of type double, 2^-1022 (retains full precision).
 *  Also, the smallest double such that 1.0 / 'SAFE_MIN' does not overflow.
 */
val MIN_NORMAL = java.lang.Double.MIN_NORMAL

/** Largest positive finite value of type double, 2^1023
 */
val MAX_VALUE = java.lang.Double.MAX_VALUE

/** Special value representing negative infinity: 1111111111110...0
 *  Ex: -1.0 / 0.0
 *  @see http://stackoverflow.com/questions/13317566/what-are-the-infinity-constants-in-java-really
 */
val NEGATIVE_INFINITY = java.lang.Double.NEGATIVE_INFINITY

/** Special value representing positive infinity: 0111111111110...0
 *  Ex: 1.0 / 0.0
 *  @see http://stackoverflow.com/questions/13317566/what-are-the-infinity-constants-in-java-really
 */
val POSITIVE_INFINITY = java.lang.Double.POSITIVE_INFINITY

/** Smallest double such that 1.0 + EPSILON != 1.0, slightly above 2^-53.
 *  Also, known as the "machine epsilon".
 *  @see https://issues.scala-lang.org/browse/SI-3791
 */
val EPSILON = 1.1102230246251568E-16               // 1 + EPSILON okay

/** Default tolerance should be much larger than the "machine epsilon".
 *  Application dependent => redefine as needed per application.
 */
val TOL = 1000.0 * EPSILON

/** The number 2π (needed in common calculations)
 */
val _2Pi = 2.0 * Pi

/** The number srqt 2π (needed in common calculations)
 */
val sqrt_2Pi = sqrt (2.0 * Pi)

/** Indicators of missing/illegal values per datatype
 */
val NO_DOUBLE = -0.0
val NO_INT    = java.lang.Integer.MIN_VALUE
val NO_LONG   = java.lang.Long.MIN_VALUE
val NO_STRING = null.asInstanceOf [String]

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Return the maximum of three values: x, y, z.
 *  @param x  the first value
 *  @param y  the second value
 *  @param z  the third value
 */
inline def max3 (x: Double, y: Double, z: Double): Double = max (max (x, y), z)

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Return the absolute value of x with the sign of y.
 *  @param x  the value contributor
 *  @param y  the sign contributor
 */
inline def sign (x: Double, y: Double): Double = if y < 0.0 then -abs (x) else abs (x)

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Power function for scala Longs x ~^ y.  Compute: math.pow (x, y).toLong without using
 *  floating point, so as to not lose precision.
 *  @param x  the Long base parameter
 *  @param y  the Long exponent parameter
 */
def powl (x: Long, y: Long): Long =
    var base   = x
    var exp    = y
    var result = 1L
    while exp != 0L do
        if (exp & 1L) == 1L then result *= base
        exp >>= 1L
        base *= base
    result
end powl

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Return the relative difference between x and y.
 *  @param x  the first double precision floating point number
 *  @param y  the second double precision floating point number
 */
def rel_diff (x: Double, y: Double): Double = abs (x - y) / max (abs (x), abs (y))

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Determine whether two double precision floating point numbers 'x' and 'y'
 *  are nearly equal.  Two numbers are considered to be nearly equal, if within
 *  '2 EPSILON'.  A number is considered to be nearly zero, if within '2 MIN_NORMAL'.
 *  To accommodate round-off errors, may use 'TOL' instead of 'EPSILON'.
 *----------------------------------------------------------------------------------------
 *  @see stackoverflow.com/questions/4915462/how-should-i-do-floating-point-comparison
 *----------------------------------------------------------------------------------------
 *  If both 'x' and 'y' are NaN (Not-a-Number), the IEEE standard indicates that should
 *  be considered always not equal.  For 'near_eq', they are considered nearly equal.
 *  Comment out the first line below to conform more closely to the IEEE standard.
 *  @see stackoverflow.com/questions/10034149/why-is-nan-not-equal-to-nan
 *----------------------------------------------------------------------------------------
 *  @param x  the first double precision floating point number
 *  @param y  the second double precision floating point number
 */
def near_eq (x: Double, y: Double): Boolean =
    if isNaN (x) && isNaN (y) then return true          // comment out to follow IEEE standard
    if x == y then return true                          // they are equal

    val diff  = abs (x - y)
    val norm1 = min (abs (x) + abs (y), MAX_VALUE)
    diff < max (MIN_NORMAL, TOL * norm1)
end near_eq

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Format a double value for printing.
 *  @param x  the double value to format
 */
def fmt (x: Double): String = "%.6f".format (x)

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Extend `Int` to include an exponentiation operator (~^), nearly equal (=~) and in/out.
 */
extension (x: Int)
    def ~^ (y: Int): Int = pow (x.toDouble, y.toDouble).toInt
    def =~ (y: Int): Boolean = x == y
    def in (r: (Int, Int)): Boolean = r._1 <= x && x <= r._2
    def out (r: (Int, Int)): Boolean = x < r._1 || r._2 < x

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Extend `Long` to include an exponentiation operator (~^), nearly equal (=~) and in/out.
 */
extension (x: Long)
    def ~^ (y: Long): Long = powl (x, y)
    def =~ (y: Long): Boolean = x == y
    def in (r: (Long, Long)): Boolean = r._1 <= x && x <= r._2
    def out (r: (Long, Long)): Boolean = x < r._1 || r._2 < x

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Extend `Double` to include an exponentiation operator (~^), nearly equal (=~) and in/out.
 */
extension (x: Double)
    def ~^ (y: Double): Double = pow (x, y)
    def =~ (y: Double): Boolean = near_eq (x, y)
    def in (r: (Double, Double)): Boolean = r._1 <= x && x <= r._2
    def out (r: (Double, Double)): Boolean = x < r._1 || r._2 < x

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Extend `String` to include an exponentiation operator (~^), nearly equal (=~) and in/out,
 *  as well operators for numeric types.
 */
extension (x: String)
    def ~^ (y: String): String = "NaN"
    def =~ (y: String): Boolean = x.toLowerCase () == y.toLowerCase ()
    def in (r: (String, String)): Boolean = r._1 <= x && x <= r._2
    def out (r: (String, String)): Boolean = x < r._1 || r._2 < x
    def unary_- : String = "-" + x
    def - (y: String): String = x diff y
    def * (y: String): String = x.repeat (y.toInt)
    def * (y: Int): String = x.repeat (y)
    def / (y: String): String = "NaN"
    def mkDouble: Double = safe_toDouble (x)

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Safe method for converting a string into a Double that catches the exceptions.
 *  @param s  the string to convert to a double
 */
def safe_toDouble (s: String): Double =
    var d: Double = NO_DOUBLE
    try 
        d = java.lang.Double.parseDouble (s)
    catch
        case ex: java.lang.NullPointerException =>
            println ("safe_toDouble: can't parse null string")
        case ex: java.lang.NumberFormatException =>
            println (s"safe_toDouble: can't parse $s to create a Double")
    end try
    d
end safe_toDouble


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ValueType` type is a union type for atomic database values.
 */
type ValueType = (Int | Long | Double | String)

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Extension methods for `ValueType`.
 */
extension (x: ValueType)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The toInt extension method converts a `ValueType` to an `Int`.
     */
    def toInt: Int = 
        x match
        case _: Int    => x.asInstanceOf [Int].toInt
        case _: Long   => x.asInstanceOf [Long].toInt
        case _: Double => x.asInstanceOf [Double].toInt
        case _: String => x.asInstanceOf [String].toInt
    end toInt

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The toDouble extension method converts a `ValueType` to a `Double`.
     */
    def toDouble: Double = 
        x match
        case _: Int    => x.asInstanceOf [Int].toDouble
        case _: Long   => x.asInstanceOf [Long].toDouble
        case _: Double => x.asInstanceOf [Double].toDouble
        case _: String => x.asInstanceOf [String].toDouble
    end toDouble

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The < extension method determines whether x < y.
     */
    def < (y: ValueType) =
        x match
        case _: Int    => x.asInstanceOf [Int]    < y.asInstanceOf [Int]
        case _: Long   => x.asInstanceOf [Long]   < y.asInstanceOf [Long]
        case _: Double => x.asInstanceOf [Double] < y.asInstanceOf [Double]
        case _: String => x.asInstanceOf [String] < y.asInstanceOf [String]
    end <

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The <= extension method determines whether x <= y.
     */
    def <= (y: ValueType) =
        x match
        case _: Int    => x.asInstanceOf [Int]    <= y.asInstanceOf [Int]
        case _: Long   => x.asInstanceOf [Long]   <= y.asInstanceOf [Long]
        case _: Double => x.asInstanceOf [Double] <= y.asInstanceOf [Double]
        case _: String => x.asInstanceOf [String] <= y.asInstanceOf [String]
    end <=

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The > extension method determines whether x > y.
     */
    def > (y: ValueType) =
        x match
        case _: Int    => x.asInstanceOf [Int]    > y.asInstanceOf [Int]
        case _: Long   => x.asInstanceOf [Long]   > y.asInstanceOf [Long]
        case _: Double => x.asInstanceOf [Double] > y.asInstanceOf [Double]
        case _: String => x.asInstanceOf [String] > y.asInstanceOf [String]
    end >

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The >= extension method determines whether x >= y.
     */
    def >= (y: ValueType) =
        x match
        case _: Int    => x.asInstanceOf [Int]    >= y.asInstanceOf [Int]
        case _: Long   => x.asInstanceOf [Long]   >= y.asInstanceOf [Long]
        case _: Double => x.asInstanceOf [Double] >= y.asInstanceOf [Double]
        case _: String => x.asInstanceOf [String] >= y.asInstanceOf [String]
    end >=


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Property` type is a map from property names to property values.
 */
type Property = Map [String, ValueType]

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The +++ extension method concatenates properties and renames to avoid ambiguity.
 */
extension (p: Property)
    def +++ (q: Property): Property = 
        val pq = p.clone
        for qe <- q do pq += (if p contains qe._1 then (qe._1 + "2", qe._2) else qe)
        pq
    end +++


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `valueTypeTest` main function is used to test the `ValueType` type.
 *  > runMain scalation.valueTypeTest
 */
@main def valueTypeTest (): Unit =

    val store = VEC [ValueType] (0, 1L, 2.0, "three")
    println (s"store = $store")
    println (s"store(0) == 1: ${store(0) == 1}")
    println (s"store(0) < 1: ${store(0) < 1}")
    //println (s"store(0) > 1: ${store(0) > 1}")

end valueTypeTest

