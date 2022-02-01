
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Thu Jun 17 19:29:23 EDT 2021
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Matrix Data Structure of Doubles
 */

package scalation
package mathstat

import java.util.Arrays.copyOf
import java.io.PrintWriter

import scala.collection.immutable.{IndexedSeq => IIndexedSeq}
import scala.collection.mutable.{ArrayBuffer, IndexedSeq, Set}
import scala.math.sqrt
import scala.runtime.ScalaRunTime.stringOf

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MatrixD` class stores and operates on Numeric Matrices of base type `Double`.
 *  the 'cfor' loop is used for innermost loops for faster execution.
 *  @param dim   the first (row) dimension of the matrix
 *  @param dim2  the second (column)dimension of the matrix
 *  @param v     the 2D array used to store matrix elements
 */
class MatrixD (val dim:  Int,
               val dim2: Int,
               private [mathstat] var v: Array [Array [Double]] = null):

    private val debug = debugf ("MatrixD", true)                 // partial invocation of debug function
    private val flaw  = flawf ("MatrixD")                        // partial invocation of flaw function

    if v == null then                                            // no array => allocate array
        v = Array.ofDim [Double] (dim, dim2)
    else                                                         // existing array => check dimensions
        val v_dim  = v.length
        val v_dim2 = if v_dim > 0 then v(0).length else dim2
        if dim != v_dim || dim2 != v_dim2 then
            flaw ("init", s"dimensions are wrong: dims = ($dim, $dim2) vs. ($v_dim, $v_dim2)")
//          throw new Exception ()
        end if
        if dim == 0 || dim2 == 0 then
            flaw ("init", s"warning, a matrix dimension is zero: dims = ($dim, $dim2)")
//          throw new Exception ()
        end if
    end if

    /** The row index range
     */
    val indices  = 0 until dim

    /** The column index range
     */
    val indices2 = 0 until dim2

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the row and column dimensions of this matrix.
     */
    def dims: (Int, Int) = (dim, dim2)

    private val minDim  = math.min (dim, dim2)                   // the minimun dimension
    private val TSZ     = 100                                    // the tile/block size (tunable)
    private var fString = "%g,\t"                                // output format spec

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return a deep copy of this matrix (note: clone may not be deep).
     *  Uses Java's native `Arrays.copyOf` for efficient copying of 2D array.
     */
    def copy: MatrixD =
        val a = new Array [Array [Double]] (dim)
        var i = 0
        cfor (i < dim, i += 1) { a(i) = copyOf (v(i), dim2) }
        new MatrixD (dim, dim2, a)
    end copy

// apply, update and related methods

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the ELEMENT in row i, column j of this matrix.
     *  usage: x(3, 2)
     *  @param i  the row index
     *  @param j  the column index
     */
    inline def apply (i: Int, j: Int): Double = v(i)(j)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the intersection of the ROWS in range ir and COLUMNS in range jr
     *  of this matrix as a new independent matrix.
     *  usage: x(3 to 6, 2 to 4)
     *  @param ir  the index range of rows to return
     *  @param jr  the index range of columns to return
     */
    def apply (ir: Range, jr: Range): MatrixD =
        val i1 = ir.start; val j1 = jr.start
        val a  = Array.ofDim [Double] (ir.size, jr.size)
        for i <- ir do
            val v_i = v(i); val a_i = a(i-i1)
            for j <- jr do a_i(j-j1) = v_i(j)
        end for
        new MatrixD (ir.size, jr.size, a)
    end apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the actual i-th ROW of this matrix.
     *  usage: x(3)
     *  @param i  the row index
     */
    inline def apply (i: Int): VectorD = new VectorD (dim2, v(i))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the ROWS in range ir of this matrix as a new independent matrix.
     *  usage: x(3 to 6)
     *  @param ir  the index range of rows to return
     */
    def apply (ir: Range): MatrixD =
        val i1 = ir.start
        val a  = Array.ofDim [Array [Double]] (ir.size)
        for i <- ir do a(i-i1) = copyOf (v(i), dim2)
        new MatrixD (ir.size, dim2, a)
    end apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the ROWS in index set iset of this matrix as a new independent matrix.
     *  usage: x(Set (3, 5, 7))
     *  @param iset  the index set of rows to return
     */
    def apply (iset: Set [Int]): MatrixD =
        val a = Array.ofDim [Array [Double]] (iset.size)
        var k = 0
        for i <- iset do { a(k) = copyOf (v(i), dim2); k += 1 }
        new MatrixD (iset.size, dim2, a)
    end apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the ROWS in index sequence idx of this matrix as a new independent matrix.
     *  usage: x(Array (3, 5, 7))
     *  @param idx  the index sequence of rows to return
     */
    def apply (idx: IndexedSeq [Int]): MatrixD =
        val a = Array.ofDim [Array [Double]] (idx.size)
        var k = 0
        for i <- idx do { a(k) = copyOf (v(i), dim2); k += 1 }
        new MatrixD (idx.size, dim2, a)
    end apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the ROWS in index sequence idx of this matrix as a new independent matrix.
     *  usage: x(Array (3, 5, 7))
     *  @param idx  the index sequence of rows to return
     */
    def apply (idx: IIndexedSeq [Int]): MatrixD =
        val a = Array.ofDim [Array [Double]] (idx.size)
        var k = 0
        for i <- idx do { a(k) = copyOf (v(i), dim2); k += 1 }
        new MatrixD (idx.size, dim2, a)
    end apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the j-th COLUMN of this matrix as an independent vector.
     *  usage: x(?, 2)
     *  @param all  use the all rows indicator ?
     *  @param j    the column index
     */
    inline def apply (all: Char, j: Int): VectorD =
        val a = Array.ofDim [Double] (dim)
        var i = 0
        cfor (i < dim, i += 1) { a(i) = v(i)(j) }
        new VectorD (dim, a)
    end apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the COLUMNS in range jr of this matrix as a new independent matrix.
     *  usage: x(?, 2 to 4)
     *  @param all  use the all rows indicator ?
     *  @param jr   the index range of columns to return
     */
    def apply (all: Char, jr: Range): MatrixD =
        val j1 = jr.start
        val a  = Array.ofDim [Double] (dim, jr.size)
        for i <- indices do
            val v_i = v(i); val a_i = a(i)
            for j <- jr do a_i(j-j1) = v_i(j)
        end for
        new MatrixD (dim, jr.size, a)
    end apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the COLUMNS in index set jset of this matrix as a new independent matrix.
     *  usage: x(?, Set (2, 4, 6))
     *  @param all   use the all rows indicator ?
     *  @param jset  the index set of columns to return
     */
    def apply (all: Char, jset: Set [Int]): MatrixD =
        val a = Array.ofDim [Double] (dim, jset.size)
        for i <- indices do
           val v_i = v(i); val a_i = a(i)
           var l = 0
           for j <- jset do { a_i(l) = v_i(j); l += 1 }
        end for
        new MatrixD (dim, jset.size, a)
    end apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the COLUMNS in index sequence jdx of this matrix as a new independent matrix.
     *  usage: x(?, Array (2, 4, 6))
     *  @param all  use the all rows indicator ?
     *  @param jdx  the index set of columns to return
     */
    def apply (all: Char, jdx: IndexedSeq [Int]): MatrixD =
        val a = Array.ofDim [Double] (dim, jdx.size)
        for i <- indices do
           val v_i = v(i); val a_i = a(i)
           var l = 0
           for j <- jdx do { a_i(l) = v_i(j); l += 1 }
        end for
        new MatrixD (dim, jdx.size, a)
    end apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the main DIAGONAL of this matrix as a new independet vector.
     *  usage: x(?)
     *  @param diag  use the all diagonal elements indicator ?
     */
    inline def apply (diag: Char): VectorD =
        val a = Array.ofDim [Double] (minDim)
        for i <- 0 until minDim do a(i) = v(i)(i)
        new VectorD (minDim, a)
    end apply
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return all but the i-th ROW of this matrix as a new independent matrix.
     *  usage: x.not(3)
     *  @param i  the row index to exclude
     */
    def not (i: Int): MatrixD = this(0 until i) ++ this(i+1 until dim)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return all but the ROWS in index sequence idx of this matrix as a new
     *  independent matrix.
     *  usage: x.not(Array (3, 5, 7))
     *  @param idx  the index sequence of rows to exclude
     */
    def not (idx: IndexedSeq [Int]): MatrixD =
        val a = Array.ofDim [Array [Double]] (dim - idx.size)
        var k = 0
        for i <- indices if ! (idx contains i) do { a(k) = copyOf (v(i), dim2); k += 1 }
        new MatrixD (a.length, dim2, a)
    end not

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return all but the j-th COLUMN of this matrix as a new independent matrix..
     *  usage: x.not(?, 2)
     *  @param all  use the all rows indicator ?
     *  @param j    the column index to exclude
     */
    def not (all: Char, j: Int): MatrixD =
        if j == 0 then             this(?, j+1 until dim2)
        else if j == dim2 - 1 then this(?, 0 until j)
        else this(?, 0 until j) ++^ this(?, j+1 until dim2)
    end not

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Split the rows from this matrix to form two matrices:  one from the rows in
     *  rowIndex (e.g., testing set) and the other from rows not in rowIndex
     *  (e.g., training set).
     *  @param idx  the row indices to include/exclude
     */
    def split (idx: IndexedSeq [Int]): (MatrixD, MatrixD) = (this(idx), not(idx))
    def split (idx: VectorI): (MatrixD, MatrixD) = { val idx_ = idx.toMuIndexedSeq; (this(idx_), not(idx_)) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the dim-by-dim2 lower triangle of this matrix (rest are zero).
     */ 
    def lower: MatrixD =
        val a = Array.ofDim [Double] (dim, dim2)
        for (i <- indices; j <- 0 to i) a(i)(j) = v(i)(j)
        new MatrixD (dim, dim2, a)
    end lower  
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the dim2-by-dim2 upper triangle of this matrix (rest are zero).
     */ 
    def upper: MatrixD =
        val a = Array.ofDim [Double] (dim2, dim2)
        for (i <- indices2; j <- i until dim2) a(i)(j) = v(i)(j)
        new MatrixD (dim2, dim2, a)
    end upper

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Update the ELEMENT in row i, column j of this matrix.
     *  usage: x(i, j) = 5
     *  @param i  the row index
     *  @param j  the column index
     *  @param s  the scalar value to assign
     */
    inline def update (i: Int, j: Int, s: Double): Unit = v(i)(j) = s

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Update the elements in the i-th ROW of this matrix.
     *  usage: x(i) = u
     *  @param i  the row index
     *  @param u  the vector to assign
     */
    inline def update (i: Int, u: VectorD): Unit = v(i) = u.toArray

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Update the elements in the j-th COLUMN of this matrix.
     *  usage: x(?, 2) = u
     *  @param all  use the all rows indicator ?
     *  @param j    the column index
     *  @param u    the vector to assign
     */
    def update (all: Char, j: Int, u: VectorD): Unit =
        for i <- 0 until dim do v(i)(j) = u(i)
    end update

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Update the main DIAGONAL of this matrix according to the given scalar.
     *  usage: x(?, ?) = 5
     *  @param d1  use the all diagonal elements indicator ?
     *  @param d2  use the all diagonal elements indicator ?
     *  @param s   the scalar value to assign
     */
    def update (d1: Char, d2: Char, s: Double): Unit =
        for i <- 0 until minDim do v(i)(i) = s
    end update

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Update the main DIAGONAL of this matrix according to the given vector.
     *  usage: x(?, ?) = u
     *  @param d1  use the all diagonal elements indicator ?
     *  @param d2  use the all diagonal elements indicator ?
     *  @param u   the vector to assign
     */
    def update (d1: Char, d2: Char, u: VectorD): Unit =
        for i <- 0 until minDim do v(i)(i) = u(i)
    end update

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set this matrix's i-th ROW to the elements in vector u.  May have left-over
     *  elements in row unassigned.
     *  @param i  the row index
     *  @param u  the vector value to assign
     */
    def set (i: Int, u: VectorD): Unit = 
        if u.dim > dim2 then flaw ("set", "vector u is larger than the number of columns")
        for j <- u.indices do v(i)(j) = u(j)
    end set

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the k-th DIAGONAL of this matrix to the elements in vector u.
     *  @param u  the vector to set the diagonal to
     *  @param k  how far above the main diagonal, e.g., (-1, 0, 1) for (sub, main, super)
     */
    def setDiag (u: VectorD, k: Int = 0): Unit =
        val dm = math.min (dim, dim2)
        val (j, l) = (math.max (-k, 0), math.min (dm-k, dm))
        for i <- j until l do v(i)(i+k) = u(i-j)
    end setDiag

// Build new matrix from existing matrices

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Transpose this matrix (swap columns <=> rows).
     *  Note: new MatrixD (dim2, dim, v.transpose) does not work when a dimension is 0.
     */
    def transpose: MatrixD = 
        val a = Array.ofDim [Double] (dim2, dim)
        for j <- indices do
            val v_j = v(j)
            var i = 0
            cfor (i < dim2, i += 1) { a(i)(j) = v_j(i) }
        end for
        new MatrixD (dim2, dim, a)
    end transpose

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Concatenate (row-wise) this matrix and matrix y (requires y to have the
     *  same column dimension as this).
     *  @param y  the other matrix
     */
    def ++ (y: MatrixD): MatrixD =
        if dim2 != y.dim2 then
            flaw ("++", s"requires same column dimensions: dim2 = $dim2 != y.dim2 = $y.dim2")

        new MatrixD (dim + y.dim, dim2, v ++ y.v)
    end ++

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Concatenate (column-wise) this matrix and matrix y (requires y to have the
     *  same row dimension as this).
     *  @param y  the other matrix
     */
    def ++^ (y: MatrixD): MatrixD =
        if dim != y.dim then
            flaw ("++^", s"requires same row dimensions: dim = $dim != y.dim = $y.dim")

        val n = dim2 + y.dim2
        val a = Array.ofDim [Double] (dim, n)
        for i <- a.indices do
            val a_i = a(i)
            var j = 0
            cfor (j < dim2, j += 1) { a_i(j) = v(i)(j) }
            cfor (j < n, j += 1)    { a_i(j) = y.v(i)(j-dim2) }
        end for
        new MatrixD (dim, n, a)
    end ++^

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Concatenate (row) vector u and this matrix, i.e., prepend u to this.
     *  @param u  the vector to be prepended as the new first row in new matrix
     */
    def +: (u: VectorD): MatrixD =
        if u.dim != dim2 then
            flaw ("+:", s"vector does not match row dimension: u.dim = ${u.dim} != dim2 = dim2")

        val c = new MatrixD (dim + 1, dim2)
        for i <- c.indices do c(i) = if i == 0 then u else this(i-1)
        c
    end +:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Concatenate (column) vector u and this matrix, i.e., prepend u to this.
     *  @param u  the vector to be prepended as the new first column in new matrix
     */
    def +^: (u: VectorD): MatrixD =
        if u.dim != dim then
            flaw ("+^:", s"vector does not match column dimension: u.dim = ${u.dim} != dim = $dim")

        val c = new MatrixD (dim, dim2 + 1)
        for j <- c.indices2 do c(?, j) = if j == 0 then u else this(?, j-1)
        c
    end +^:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Concatenate this matrix and (row) vector u, i.e., append u to this.
     *  @param u  the vector to be appended as the new last row in new matrix
     */
    def :+ (u: VectorD): MatrixD =
        if u.dim != dim2 then
            flaw (":+", s"vector does not match row dimension: u.dim = ${u.dim} != dim2 = $dim2")

        val c = new MatrixD (dim + 1, dim2)
        for i <- c.indices do c(i) = if i < dim then this(i) else u
        c
    end :+

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Concatenate this matrix and (column) vector u, i.e., append u to this.
     *  @param u  the vector to be appended as the new last column in new matrix
     */
    def :^+ (u: VectorD): MatrixD =
        if u.dim != dim then
            flaw (":^+", s"vector does not match column dimension: u.dim = ${u.dim} != dim = $dim")

        val c = new MatrixD (dim, dim2 + 1)
        for j <- c.indices2 do c(?, j) = if j < dim2 then this(?, j) else u
        c
    end :^+

// Add (+) matrix and (matrix, vector, scalar)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add this matrix and matrix y (requires y to have at least the dimensions of this).
     *  Alias rows to avoid double subscripting.
     *  @param y  the other matrix
     */
    def + (y: MatrixD): MatrixD =
        if y.dim < dim || y.dim2 < dim2 then
            flaw ("+", "matrix + matrix - incompatible dimensions: this = $dims, y = ${y.dims}")

        val a = Array.ofDim [Double] (dim, dim2)
        for i <- indices do
            val v_i = v(i); val y_i = y.v(i); val a_i = a(i)
            var j = 0
            cfor (j < dim2, j += 1) { a_i(j) = v_i(j) + y_i(j) }
        end for
        new MatrixD (dim, dim2, a) 
    end +

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add this matrix and (row) vector u.
     *  @param u  the vector to add
     */
    def + (u: VectorD): MatrixD =
        if u.dim < dim2 then
            flaw ("+", "matrix + vector - incompatible dimensions: this = $dims, u = $u.dim")

        val a = Array.ofDim [Double] (dim, dim2)
        for i <- indices do
            val v_i = v(i); val a_i = a(i)
            var j = 0
            cfor (j < dim2, j += 1) { a_i(j) = v_i(j) + u(j) }
        end for
        new MatrixD (dim, dim2, a) 
    end +

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add this matrix and scaler u.
     *  @param u  the scalar to add
     */
    def + (u: Double): MatrixD =
        val a = Array.ofDim [Double] (dim, dim2)
        for i <- indices do
            val v_i = v(i); val a_i = a(i)
            var j = 0
            cfor (j < dim2, j += 1) { a_i(j) = v_i(j) + u }
        end for
        new MatrixD (dim, dim2, a)
    end +

// Subtract (-) from matrix, (matrix, vector, scalar)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Subtract from this matrix the matrix y (requires y to have at least the dimensions of this).
     *  Alias rows to avoid double subscripting.
     *  @param y  the other matrix
     */
    def - (y: MatrixD): MatrixD =
        if y.dim < dim || y.dim2 < dim2 then
            flaw ("-", "matrix - matrix - incompatible dimensions: this = $dims, y = ${y.dims}")

        val a = Array.ofDim [Double] (dim, dim2)
        for i <- indices do
            val v_i = v(i); val y_i = y.v(i); val a_i = a(i)
            var j = 0
            cfor (j < dim2, j += 1) { a_i(j) = v_i(j) - y_i(j) }
        end for
        new MatrixD (dim, dim2, a)
    end -

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Subtract from this matrix, the (row) vector u.
     *  @param u  the vector to subtract
     */
    def - (u: VectorD): MatrixD =
        if u.dim < dim2 then
            flaw ("-", "matrix - vector - incompatible dimensions: this = $dims, u = $u.dim")

        val a = Array.ofDim [Double] (dim, dim2)
        for i <- indices do
            val v_i = v(i); val a_i = a(i)
            var j = 0
            cfor (j < dim2, j += 1) { a_i(j) = v_i(j) - u(j) }
        end for
        new MatrixD (dim, dim2, a)
    end -

   //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Subtract from this matrix, the scalar u.
     *  @param u  the scalar to subtract
     */
    def - (u: Double): MatrixD =
        val a = Array.ofDim [Double] (dim, dim2)
        for i <- indices do
            val v_i = v(i); val a_i = a(i)
            var j = 0
            cfor (j < dim2, j += 1) { a_i(j) = v_i(j) - u }
        end for
        new MatrixD (dim, dim2, a)
    end -

// Multiply element-wise (*~) matrix and (matrix, vector)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply element-wise, this matrix and matrix y (requires y to have at least
     *  the dimensions of this).  Alias rows to avoid double subscripting.
     *  Also known as Hadamard product.
     *  @param y  the other matrix
     */
    def *~ (y: MatrixD): MatrixD =
        if y.dim < dim || y.dim2 < dim2 then
            flaw ("*~", "matrix *~ matrix - incompatible dimensions: this = $dims, y = ${y.dims}")

        val a = Array.ofDim [Double] (dim, dim2)
        for i <- indices do
            val v_i = v(i); val y_i = y.v(i); val a_i = a(i)
            var j = 0
            cfor (j < dim2, j += 1) { a_i(j) = v_i(j) * y_i(j) }
        end for
        new MatrixD (dim, dim2, a)
    end *~

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply this matrix by vector u to produce another matrix v_ij * u_j.
     *  E.g., multiply a matrix by a diagonal matrix represented as a vector.
     *  @param u  the vector to multiply by
     */
    def *~ (u: VectorD): MatrixD =
        val dm = math.min (dim2, u.dim)
        val a  = Array.ofDim [Double] (dim, dm)
        for i <- indices do
            val v_i = v(i); val a_i = a(i)
            var j = 0
            cfor (j < dm, j += 1) { a_i(j) = v_i(j) * u(j) }
        end for
        new MatrixD (dim, dm, a)
    end *~

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply vector u by this matrix to produce another matrix u_i * v_ij.
     *  E.g., multiply a diagonal matrix represented as a vector by a matrix.
     *  This operator is right associative (vector *~: matrix).
     *  @param u  the vector to multiply by
     */
    def *~: (u: VectorD): MatrixD =
        val dm = math.min (dim2, u.dim)
        val a  = Array.ofDim [Double] (dim, dm)
        for i <- indices do
            val v_i = v(i); val a_i = a(i)
            var j = 0
            cfor (j < dm, j+= 1) { a_i(j) = u(i) * v_i(j) }
        end for
        new MatrixD (dim, dm, a)
    end *~:

// Multiply (*) matrix and (matrix, vector, scalar)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply this matrix and matrix y (requires y to have at least the dimensions of this).
     *  Alias rows to avoid double subscripting, use tiling/blocking, and optimized i, k, j loop order.
     *  @see software.intel.com/content/www/us/en/develop/documentation/advisor-cookbook/top/
     *  optimize-memory-access-patterns-using-loop-interchange-and-cache-blocking-techniques.html
     *  @param y  the other matrix
     */
    def * (y: MatrixD): MatrixD =
        if y.dim != dim2 then
            flaw ("*", s"matrix * matrix - incompatible cross dimensions: dim2 = $dim2, y.dim = ${y.dim}")

        val a = Array.ofDim [Double] (dim, y.dim2)

        for ii <- 0 until dim by TSZ do
            val i2 = math.min (ii + TSZ, dim)
            for kk <- 0 until dim2 by TSZ do
                val k2 = math.min (kk + TSZ, dim2)
                for jj <- 0 until y.dim2 by TSZ do
                    val j2 = math.min (jj + TSZ, y.dim2)

                    for i <- ii until i2 do
                        val v_i = v(i); val a_i = a(i)
                        for k <- kk until k2 do
                            val y_k = y.v(k); val v_ik = v_i(k)
                            var j = jj
                            cfor (j < j2, j += 1) { a_i(j) += v_ik * y_k(j) }
                        end for
                    end for

                end for
            end for
        end for
        new MatrixD (dim, y.dim2, a)
    end *

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply this matrix and vector y (requires y to have at least dim2 elements).
     *  Alias rows to avoid double subscripting and use array ops.
     *  @param y  the vector to multiply by
     */
    def * (y: VectorD): VectorD =
        if y.dim < dim2 then
            flaw ("*", s"matrix * vector - dimension of vector y: y.dim = ${y.dim} < dim2 = $dim2")

        val a = Array.ofDim [Double] (dim)
        for i <- indices do
            val v_i = v(i)
            var sum = 0.0
            var j = 0
            cfor (j < dim2, j += 1) { sum += v_i(j) * y(j) }
            a(i) = sum
        end for
        new VectorD (dim, a)
    end *

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply (row) vector y by this matrix.  Note '*:' is right associative.
     *  vector = vector *: matrix
     *  @param y  the vector to multiply by
     */
    def *: (y: VectorD): VectorD = this.transpose * y

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply this matrix and scaler u.
     *  @param u  the scalar to multiply by
     */
    def * (u: Double): MatrixD =
        val a = Array.ofDim [Double] (dim, dim2)
        for i <- indices do
            val v_i = v(i); val a_i = a(i)
            var j = 0
            cfor (j < dim2, j += 1) { a_i(j) = v_i(j) * u }
        end for
        new MatrixD (dim, dim2, a)
    end *

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply this matrix and matrix y (requires y to have at least the dimensions of this).
     *  Alias rows to avoid double subscripting, transpose y first and use array ops.
     *  A simpler, less efficient version of '*'.
     *  @param y  the other matrix
     */
    def mul (y: MatrixD): MatrixD =
        if dim2 != y.dim then
            flaw ("mul", s"matrix mul matrix - incompatible cross dimensions: dim2 = $dim2, y.dim = ${y.dim}")

        val a  = Array.ofDim [Double] (dim, y.dim2)
        val yt = y.v.transpose
        for i <- indices do
            val v_i = v(i); val a_i = a(i)
            for j <- y.indices2 do
                val y_j = yt(j)
                var sum = 0.0
                var k = 0
                cfor (k < dim2, k += 1) { sum += v_i(k) * y_j(k) }
                a_i(j) = sum
            end for
        end for
        new MatrixD (dim, y.dim2, a) 
    end mul

// Divide (/) matrix by (matrix, vector, scalar)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Divide element-wise, this matrix by matrix y (requires y to have at least
     *  the dimensions of this).  Alias rows to avoid double subscripting.
     *  @param y  the other matrix
     */
    def / (y: MatrixD): MatrixD =
        if y.dim < dim || y.dim2 < dim2 then
            flaw ("/", "matrix / matrix - incompatible dimensions: this = $dims, y = ${y.dims}")

        val a = Array.ofDim [Double] (dim, dim2)
        for i <- indices do
            val v_i = v(i); val y_i = y.v(i); val a_i = a(i)
            var j = 0
            cfor (j < dim2, j += 1) { a_i(j) = v_i(j) / y_i(j) }
        end for
        new MatrixD (dim, dim2, a)
    end /

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Divide element-wise, this matrix by (row) vector u.
     *  @param u  the vector to divide by
     */
    def / (u: VectorD): MatrixD =
        if u.dim < dim2 then
            flaw ("/", "matrix / vector - incompatible dimensions: this = $dims, u = $u.dim")

        val a = Array.ofDim [Double] (dim, dim2)
        for i <- indices do
            val v_i = v(i); val a_i = a(i)
            var j = 0
            cfor (j < dim2, j += 1) { a_i(j) = v_i(j) / u(j) }
        end for
        new MatrixD (dim, dim2, a)
    end /

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Divide element-wise this matrix by scaler u.
     *  @param u  the scalar to divide by
     */
    def / (u: Double): MatrixD =
        val a = Array.ofDim [Double] (dim, dim2)
        for i <- indices do
            val v_i = v(i); val a_i = a(i)
            var j = 0
            cfor (j < dim2, j += 1) { a_i(j) = v_i(j) / u }
        end for
        new MatrixD (dim, dim2, a)
    end /

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Raise the elements in this matrix to the p-th power (e.g., x~^2 = x *~ x)
     *  Being element-wise, x~^2 is not x * x.
     *  @param p  the scalar power
     */
    def ~^ (p: Double): MatrixD =
        val a = Array.ofDim [Double] (dim, dim2)
        for i <- indices do
            val v_i = v(i); val a_i = a(i)
            var j = 0
            cfor (j < dim2, j += 1) { a_i(j) = v_i(j) ~^ p }
        end for
        new MatrixD (dim, dim2, a)
    end ~^

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply each column of this matrix by its other columns to forms a matrix
     *  consisting of all 2-way cross terms [ x_i * x_j ] for j < i.
     */
    def crossAll: MatrixD =
        val n = dim2
        if n < 2 then flaw ("crossAll", s"requires at least 2 columns, but n = $n")
        val nn = n * (n - 1) / 2
        debug ("crossAll", s"create matrix with dims = ($dim, $nn)")
        val xx = new MatrixD (dim, nn)
        var k = 0
        for i <- indices2; j <- 0 until i do 
            xx(?, k) = this(?, i) * this(?, j)
            k += 1
        end for
        xx
    end crossAll

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply each column of this matrix by its other two columns to forms a matrix
     *  consisting of all 3-way cross terms [ x_i * x_j * x_k ] for k < j < i.
     */
    def crossAll3: MatrixD =
        val n = dim2
        if n < 3 then flaw ("crossAll3", s"requires at least 3 columns, but n = $n")
        val nn = n * (n - 1) * (n - 2) / 6
        debug ("crossAll3", s"create matrix with dims = ($dim, $nn)")
        val xx = new MatrixD (dim, nn)
        var l = 0
        for i <- indices2; j <- 0 until i; k <- 0 until j do 
            xx(?, l) = this(?, i) * this(?, j) * this(?, k)
            l += 1
        end for
        xx
    end crossAll3

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the dot product of this matrix and matrix y (requires y to have at
     *  least the dimensions of this).  Alias rows to avoid double subscripting,
     *  use tiling/blocking, and use array ops.
     *  @see en.wikipedia.org/wiki/Matrix_multiplication_algorithm
     *  @param y  the other matrix
     */
    def dot (y: MatrixD): MatrixD =
        if dim2 != y.dim then
            flaw ("dot", s"matrix dot matrix - incompatible cross dimensions: dim2 = $dim2, y.dim = ${y.dim}")

        val a = Array.ofDim [Double] (dim, y.dim)
        for ii <- 0 until dim by TSZ do
            for jj <- 0 until y.dim2 by TSZ do
                for kk <- 0 until dim2 by TSZ do
                    val k2 = math.min (kk + TSZ, dim2)

                    for i <- ii until math.min (ii + TSZ, dim) do
                        val v_i = v(i); val a_i = a(i)
                        for j <- jj until math.min (jj + TSZ, y.dim2) do
                            val y_j = y.v(j)
                            var sum = 0.0
                            var k = kk
                            cfor (k < k2, k += 1) { sum += v_i(k) * y_j(k) }
                            a_i(j) += sum
                        end for
                    end for

                end for
            end for
        end for
        new MatrixD (dim, y.dim, a)
    end dot

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Flatten this matrix in row-major fashion, returning a vector containing
     *  all the elements from the matrix.
     */
    def flatten: VectorD =
        val a = Array.ofDim [Double] (dim * dim2)
        var k = 0
        for i <- indices do
           val v_i = v(i)
           var j = 0
           cfor (j < dim2, j += 1) { a(k) = v_i(j); k += 1 }
        end for
        new VectorD (a.length, a)
    end flatten

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Solve for x using back substitution (/~) in the equation
     *      u * x = y
     *  where this matrix (u) must be upper triangular.
     *  @see MatLab's / operator
     *  @param y  the constant vector
     */
    def /~ (y: VectorD): VectorD =
        val a = Array.ofDim [Double] (dim2)          // array to hold solution
        val b = y.v                                  // y's internal array 
        for k <- dim2 - 1 to 0 by -1 do              // solve for x in u*x = y
            val u_k = v(k)                           // k-th row
            var sum = 0.0
            for j <- k + 1 until dim2 do sum += u_k(j) * a(j)
            a(k) = (b(k) - sum) / v(k)(k)
        end for
        new VectorD (dim2, a)                        // return vector x
    end /~

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether this matrix and y are nearly equal.
     */
    def =~ (y: MatrixD): Boolean =
        if dim != y.dim || dim2 != y.dim2 then return false
        for i <- indices; j <- indices2 if ! (v(i)(j) =~ y.v(i)(j)) do return false
        true
    end =~

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Map each row of this matrix by applying function f to each row vector and
     *  returning the collected result as a vector.
     *  @param f  the vector to scalar function to apply
     */
    def map (f: VectorD => Double): VectorD =
        VectorD (for i <- indices yield f(this(i)))
    end map

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the sum of this matrix, i.e., the sum of all its elements.
     */
    def sum: Double =
        var s = 0.0
        for i <- indices; j <- indices2 do s += v(i)(j)
        s
    end sum

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the column sums of this matrix, i.e., the sums for each of its columns.
     */
    def sumV: VectorD =
        var s = new VectorD (dim2)
        for i <- indices; j <- indices2 do s(j) += v(i)(j)
        s
    end sumV

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the maximum value for the entire matrix.
     */
    def mmax: Double =
        var x = v(0).max
        for i <- 1 until dim do { val z = v(i).max; if z > x then x = z }
        x
    end mmax

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the maximum value for each column in the matrix.
     */
    def max: VectorD = VectorD (for j <- indices2 yield this(?, j).max)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the minimum value for the entire matrix.
     */
    def mmin: Double =
        var x = v(0).min
        for i <- 1 until dim do { val z = v(i).min; if z < x then x = z }
        x
    end mmin

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the minimum value for each column in the matrix.
     */
    def min: VectorD = VectorD (for j <- indices2 yield this(?, j).min)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the trace of this matrix, i.e., the sum of the elements on the
     *  main diagonal.  Should also equal the sum of the eigenvalues.
     *  @see Eigen.scala
     */
    def trace: Double =
        if dim != dim2 then flaw ("trace", "trace only works on square matrices")

        var sum = 0.0
        for i <- indices do sum += v(i)(i)
        sum
    end trace

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the 1-norm of this matrix, i.e., the maximum 1-norm of the
     *  column vectors.  This is useful for comparing matrices (a - b).norm1.
     *  @see en.wikipedia.org/wiki/Matrix_norm
     */
    def norm1: Double = (for j <- indices2 yield this(?, j).norm1).max

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the column means of this matrix.
     */
    def mean: VectorD = VectorD (for j <- indices2 yield this(?, j).mean)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the column variances of this matrix.
     */
    def variance: VectorD = VectorD (for j <- indices2 yield this(?, j).variance)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the column standard deviations of this matrix.
     */
    def stdev: VectorD = VectorD (for j <- indices2 yield this(?, j).stdev)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return a matrix that is in the reverse row order of this matrix.
     */
    def reverse: MatrixD = new MatrixD (dim, dim2, v.reverse)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return whether this matrix is symmetric (i.e, equals its transpose).
     */
    def isSymmetric: Boolean =
        for i <- indices; j <- 0 until i if v(i)(j) != v(j)(i) do return false
        true
    end isSymmetric

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return whether this matrix is nonnegative (has no elements less than 0).
     */
    def isNonnegative: Boolean =
        for i <- indices; j <- indices2 if v(i)(j) < 0.0 do return false
        true
    end isNonnegative

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Swap (in-place) rows i and k in this matrix.
     *  @param i  the first row in the swap
     *  @param k  the second row in the swap
     */
    def swap (i: Int, k: Int): Unit =
        val tmp = v(i); v(i) = v(k); v(k) = tmp
    end swap

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Swap (in-place) the elements in rows i and k starting from column col.
     *  @param i    the first row in the swap
     *  @param k    the second row in the swap
     *  @param col  the starting column for the swap
     */
    def swap (i: Int, k: Int, col: Int): Unit =
        val a = this; var tmp = 0.0
        for j <- col until dim2 do { tmp = a(k, j); a(k, j) = a(i, j); a(i, j) = tmp }
    end swap

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Swap (in-place) columns j and l in this matrix.
     *  @param j    the first column in the swap
     *  @param l    the second column in the swap
     */
    def swapCol (j: Int, l: Int): Unit =
        var tmp = 0.0
        for i <- indices do { tmp = v(i)(l); v(i)(l) = v(i)(j); v(i)(j) = tmp }
    end swapCol

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Center this matrix to zero mean, column-wise, by subtracting the mean.
     *  @param mu_x  the vector of column means for this matrix
     */
    def center (mu_x: VectorD = mean): MatrixD = this - mu_x

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the sample covariance matrix for the columns of matrix this x.
     */
    def cov: MatrixD =
        val z = center ()
        (z.transpose * z) / (dim.toDouble - 1.0)
    end cov

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the population covariance matrix for the columns of this matrix.
     */
    def cov_ : MatrixD =
        val z = center ()
        (z.transpose * z) / dim.toDouble
    end cov_

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the correlation matrix for the columns of this matrix.
     *  If either variance is zero (column i, column j), will result in Not-a-Number (NaN),
     *  return one if the vectors are the same, or -0 (indicating undefined).
     *  Note:  sample vs. population results in essentailly the same values.
     *  @see the related cos function
     */
    def corr: MatrixD =
        val covv = cov                                   // sample covariance matrix
        val cor  = MatrixD.eye (covv.dim, covv.dim)      // correlation matrix

        for i <- covv.indices do
            val var_i = covv (i, i)                      // variance of column i
            for j <- 0 until i do
                cor(i, j) = covv (i, j) / sqrt (var_i * covv (j, j))
                if cor(i, j).isNaN then cor(i, j) = if (v(i) == v(j)) 1.0 else -0.0
                cor(j, i) = cor (i, j)
            end for
        end for
        cor
    end corr

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the cosine similarity matrix for the columns of matrix 'x'.
     *  If the vectors are centered, will give the correlation.
     *  @see stats.stackexchange.com/questions/97051/]
     *       building-the-connection-between-cosine-similarity-and-correlation-in-r
     */
    def cos: MatrixD =
        val cs = MatrixD.eye (dim2, dim2)                // cosine matrix

        for i <- cs.indices do
            val y  = this(?, i)                          // ith column vector
            val ny = y.norm
            for j <- 0 until i do
                val z  = this(?, j)                      // jth column vector
                val nz = z.norm
                cs(i, j) = (y dot z) / (ny * nz)
                cs(j, i) = cs (i, j)
            end for
        end for
        cs
    end cos

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert this matrix to a string.
     */
    override def toString: String = 
        val sb = new StringBuilder ("\nMatrixD(")
        if dim == 0 || dim2 == 0 then return sb.append (")").mkString
        for i <- indices; j <- indices2 do
            sb.append (fString.format (v(i)(j)))
            if j == dim2-1 then sb.replace (sb.length-1, sb.length, "\n \t")
        end for
        sb.replace (sb.length-4, sb.length, ")").mkString
    end toString

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Write this matrix to a CSV-formatted text file with name fileName.
     *  @param fileName  the name of file to hold the data
     */
    def write (fileName: String): Unit =
        val out = new PrintWriter (fileName)
        for i <- indices do
            for j <- indices2 do 
                out.print (v(i)(j))
                if j < dim2-1 then out.print (",")
            end for
            out.println ()
        end for
        out.close
    end write

end MatrixD


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MatrixD companion object provides factory methods.
 */
object MatrixD:

    private val flaw = flawf ("MatrixD")                          // flaw function

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a matrix from repeated values.
     *  @param dim  the (row, column) dimensions
     *  @param u    the repeated values
     */
    def apply (dim: (Int, Int), u: Double*): MatrixD =
        val a = Array.ofDim [Double] (dim._1, dim._2)
        for i <- 0 until dim._1; j <- 0 until dim._2 do a(i)(j) = u(i * dim._2 + j)
        new MatrixD (dim._1, dim._2, a)
    end apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a square matrix of dimension dim where all elements equal zero.
     *  @param dim  the square dimensions
     */
    def apply (dim: Int): MatrixD = new MatrixD (dim, dim)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a matrix from a variable argument list of vectors (row-wise).
     *  Use transpose to make it column-wise.
     *  @param vs  the vararg list of vectors
     */
    def apply (vs: VectorD*): MatrixD =
        val (m, n) = (vs.length, vs(0).length)
        val a = Array.ofDim [Array [Double]] (m)
        for i <- vs.indices do a(i) = vs(i).v
        new MatrixD (m, n, a)
    end apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a matrix from an mutable `IndexedSeq` of vectors (row-wise).
     *  Use transpose to make it column-wise.
     *  @param vs  the indexed sequence of vectors
     */
    def apply (vs: IndexedSeq [VectorD]): MatrixD =
        val (m, n) = (vs.length, vs(0).length)
        val a = Array.ofDim [Array [Double]] (m)
        for i <- vs.indices do a(i) = vs(i).v
        new MatrixD (m, n, a)
    end apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a matrix from an immutable `IndexedSeq` of vectors (row-wise),
     *  as produce by for yield.  Use transpose to make it column-wise.
     *  @param vs  the indexed sequence of vectors
     */
    def apply (vs: collection.immutable.IndexedSeq [VectorD]): MatrixD =
        val (m, n) = (vs.length, vs(0).length)
        val a = Array.ofDim [Array [Double]] (m)
        for i <- vs.indices do a(i) = vs(i).v
        new MatrixD (m, n, a)
    end apply

    private var DEF_SEP  = ','                               // default character separating the values
    private val PROGRESS = 100                               // give feedback at progress count

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a matrix by reading from a text file, e.g., a CSV file.
     *  @param fileName  the name of file holding the data
     *  @param skip      the initial number of lines/rows to skip
     *  @param sp        the character used to separate values (',', '\t', ...)
     */
    def load (fileName: String, skip: Int = 0, sp: Char = DEF_SEP): MatrixD =
        val lines = readFileIntoArray (fileName)             // array of strings/lines
        val m  = lines.length                                // number lines in the file
        val mm = m - skip                                    // number of lines with data
        val a  = Array.ofDim [Array [Double]] (mm)           // array buffer to hold data values
        var n  = -1                                          // number of values in a row (TBD)

        for i <- skip until m do
            val j = i - skip
            a(j) = for str <- lines(i).split (sp) yield str.mkDouble
            if (i+1) % PROGRESS == 0 then println (s"load: read $i data rows so far ...")
            if n < 0 then n = a(i).length
            else if a(i).length != n then flaw ("load", s"row $i has the wrong length")
        end for
        println (s"load: read in an $mm-by-$n matrix from $fileName")
        new MatrixD (mm, n, a)
    end load

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a matrix of dimension dim by 1 that consists of all ones.
     *  @param dim   the row dimension
     */
    def one (dim: Int): MatrixD =
        val a = Array.fill (dim, 1)(1.0)
        new MatrixD (dim, 1, a)
    end one

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a matrix of dimensions dim by dim2 where all elements equal zero.
     *  @param dim   the row dimension
     *  @param dim2  the column dimension
     */
    def eye (dim: Int, dim2: Int): MatrixD = 
        val x = new MatrixD (dim, dim2)
        x(?, ?) = 1.0                                     // set diagonal to one
        x
    end eye

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a matrix of dimensions dim by dim2 where all elements equal to the given value. 
     *  @param dim    the row dimension
     *  @param dim2   the column dimension
     *  @param value  the given value to assign to all elements
     */
    def fill (dim: Int, dim2: Int, value: Double): MatrixD = 
        val a = Array.fill (dim, dim2)(value)
        new MatrixD (dim, dim2, a)
    end fill

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** A null matrix of type `MatrixD`.
     */
    val nullm: MatrixD = null.asInstanceOf [MatrixD]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the outer product of vector x and vector y.  The result of the
     *  outer product is a matrix where element (i, j) is the product of i-th element
     *  of x with the j-th element of y.
     *  @param x  the first vector
     *  @param y  the second vector
     */
    def outer (x: VectorD, y: VectorD): MatrixD =
        val a = Array.ofDim [Double] (x.dim, y.dim)
        for i <- x.indices; j <- y.indices do a(i)(j) = x(i) * y(j)
        new MatrixD (x.dim, y.dim, a)
    end outer

end MatrixD


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MatrixDExample` object provides example instances of the`MatrixD` class.
 */
object MatrixDExample:

    val x = MatrixD ((8, 8), 1, 2,  3,  4,  5,  6,  7,  8,
                             2, 3,  4,  5,  6,  7,  8,  9,
                             3, 4,  5,  6,  7,  8,  9, 10,
                             4, 5,  6,  7,  8,  9, 10, 11,
                             5, 6,  7,  8,  9, 10, 11, 12,
                             6, 7,  8,  9, 10, 11, 12, 13,
                             7, 8,  9, 10, 11, 12, 13, 14,
                             8, 9, 10, 11, 12, 13, 14, 15)

    val y = MatrixD ((8, 8), 1, 2,  3,  4,  5,  6,  7,  8,
                             2, 3,  4,  5,  6,  7,  8,  9,
                             3, 4,  5,  6,  7,  8,  9, 10,
                             4, 5,  6,  7,  8,  9, 10, 11,
                             5, 6,  7,  8,  9, 10, 11, 12,
                             6, 7,  8,  9, 10, 11, 12, 13,
                             7, 8,  9, 10, 11, 12, 13, 14,
                             8, 9, 10, 11, 12, 13, 14, 15)

end MatrixDExample

import MatrixDExample.{x, y}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `matrixDTest` main function tests the `MatrixD` class.  Compares the performance
 *  of matrix addition implementations
 *  > runMain scalation.mathstat.matrixDTest
 */
@main def matrixDTest (): Unit =

    println (s"x = $x")

    banner ("Test apply methods")

    println (s" x(3, 2)           = ${x(3, 2)}")                  // element (3, 2)
    println (s" x(3 to 6, 2 to 4) = ${x(3 to 6, 2 to 4)}")        // slice of rows and columns
    println (s" x(3)              = ${x(3)}")                     // row 3
    println (s" x(3 to 6)         = ${x(3 to 6)}")                // slice of rows
    println (s" x(?, 2)           = ${x(?, 2)}")                  // column 2
    println (s" x(?, 2 to 4)      = ${x(?, 2 to 4)}")             // slice of columns

    banner ("Test element-wise methods")

    println (s" x + y  = ${x + y}")
    println (s" x - y  = ${x - y}")
    println (s" x *~ y = ${x *~ y}")
    println (s" x / y  = ${x / y}")
    println (s" x ~^ 2 = ${x ~^ 2}")

    println (s" x.crossAll = ${x.crossAll}")

    val a = new MatrixD (1000, 1000)
    val b = new MatrixD (1000, 1000)
    for i <- a.indices; j <- a.indices2 do { a(i, j) = i + j; b(i, j) = a(i, j) }

    for it <- 1 to 10 do
        banner (s"Timing results to iteration $it")
        val t1 = gauge { a + b };  println (s" a + b  = $t1")
        val t2 = gauge { a - b };  println (s" a - b  = $t2")
        val t3 = gauge { a *~ b }; println (s" a *~ b = $t3")
        val t4 = gauge { a / b };  println (s" a / b  = $t4")
    end for

end matrixDTest


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `matrixDTest2` main function tests the `MatrixD` class.  Compares the performance
 *  of matrix multiplication implementations.
 *  > runMain scalation.mathstat.matrixDTest2
 */
@main def matrixDTest2 (): Unit =

    println (s" x mul y  = ${x mul y}")
    println (s" x * y    = ${x * y}")
    println (s" x dot y  = ${x dot y}")
    println (s" x * y(0) = ${x * y(0)}")

    val a = new MatrixD (1000, 1000)
    val b = new MatrixD (1000, 1000)
    for i <- a.indices; j <- a.indices2 do { a(i, j) = i + j; b(i, j) = a(i, j) }

    for it <- 1 to 10 do
        banner (s"Timing results to iteration $it")
        val t1 = gauge { a mul b };  println (s" a mul b  = $t1")
        val t2 = gauge { a * b };    println (s" a * b    = $t2")
        val t3 = gauge { a dot b };  println (s" a dot b  = $t3")
        val t4 = gauge { a * b(0) }; println (s" a * b(0) = $t4")
    end for

end matrixDTest2


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `matrixDTest3` main function tests the `MatrixD` class.  Test the back substitution
 *  (/~) operator for solving for x in u * x = y.
 *  > runMain scalation.mathstat.matrixDTest3
 */
@main def matrixDTest3 (): Unit =

    val u = MatrixD ((3, 3), 1, 2, 3,
                             0, 4, 5,
                             0, 0, 6)
    val y = VectorD (1, 2, 3)

    val x = u /~ y
    println (s" u /~ y    = $x")
    assert (u * x == y)

end matrixDTest3

