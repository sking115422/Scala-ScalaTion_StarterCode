
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Tue Aug 24 20:13:53 EDT 2021
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Vect union type for vectors used in databases.
 */

package scalation
package database
package relation

import scalation.mathstat._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Type definition for a union of column/vector types.
 */
type Vect = VectorD | VectorI | VectorL | VectorS

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Extension methods for the `Vect` type.
 */
extension (x: Vect)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Covert `Vect` to a vector of double-precision reals `VectorD`.
     */
    def toDouble: VectorD =
        x match
        case _: VectorD => x.asInstanceOf [VectorD]
        case _: VectorI => x.asInstanceOf [VectorI].toDouble
        case _: VectorL => x.asInstanceOf [VectorL].toDouble
        case _: VectorS => x.asInstanceOf [VectorS].toDouble
    end toDouble

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Covert `Vect` to a vector of integers `VectorI`.
     */
    def toInt: VectorI =
        x match
        case _: VectorD => x.asInstanceOf [VectorD].toInt
        case _: VectorI => x.asInstanceOf [VectorI]
        case _: VectorL => x.asInstanceOf [VectorL].toInt
        case _: VectorS => x.asInstanceOf [VectorS].toInt
    end toInt

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Covert `Vect` to a vector of long inetgers `VectorL`.
     */
    def toLong: VectorL =
        x match
        case _: VectorD => x.asInstanceOf [VectorD].toLong
        case _: VectorI => x.asInstanceOf [VectorI].toLong
        case _: VectorL => x.asInstanceOf [VectorL]
        case _: VectorS => x.asInstanceOf [VectorS].toLong
    end toLong

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Covert `Vect` to a vector of strings `VectorS`.
     */
    def toString2: VectorS =
        x match
        case _: VectorD => x.asInstanceOf [VectorD].toString2
        case _: VectorI => x.asInstanceOf [VectorI].toString2
        case _: VectorL => x.asInstanceOf [VectorL].toString2
        case _: VectorS => x.asInstanceOf [VectorS]
    end toString2

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Filter the vector according the predicate, return the index positions where it is true.
     *  @param p  the predicate function
     */
    def filterPos (p: ValueType => Boolean): collection.immutable.IndexedSeq [Int] =
        x match
        case _: VectorD => x.asInstanceOf [VectorD].filterPos (p)
        case _: VectorI => x.asInstanceOf [VectorI].filterPos (p)
        case _: VectorL => x.asInstanceOf [VectorL].filterPos (p)
        case _: VectorS => x.asInstanceOf [VectorS].filterPos (p)
    end filterPos

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the sub-vector of elements from the given index positions.
     *  @param pos  the given index positions
     */
    def at (pos: collection.immutable.IndexedSeq [Int]): Vect =
        x match
        case _: VectorD => x.asInstanceOf [VectorD] (pos)
        case _: VectorI => x.asInstanceOf [VectorI] (pos)
        case _: VectorL => x.asInstanceOf [VectorL] (pos)
        case _: VectorS => x.asInstanceOf [VectorS] (pos)
    end at 

