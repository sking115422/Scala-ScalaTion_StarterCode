
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Santosh Uttam Bobade
 *  @version 2.0
 *  @date    Sun Aug 23 15:42:06 EDT 2015
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Base Trait for Relational Database Engines
 *
 *  An implementation supporting columnar relational databases facilitating easy
 *  and rapid analytics.  The columns in a table/relation are vectors from the
 *  `scalation.mathstat` package.  Vectors and matrices may be readily extracted
 *  from a relation and feed into any of the numerous analytics techniques provided
 *  in `scalation.modeling`.  The implementation provides most of the columnar
 *  relational algebra operators given in the following paper:
 *  @see db.csail.mit.edu/projects/cstore/vldb.pdf
 *
 *  Some of the operators have Unicode versions: @see `scalation.UnicodeTest`
 */

package scalation
package database
package relation

import scala.collection.mutable.{ArrayBuffer, IndexedSeq, Map}
import scala.collection.immutable.StringOps
import scala.reflect.ClassTag

import scalation.mathstat._

/** Boolean function that uses the value for the given column name (String)
 *  in the predicate (e.g., used by 'where' and 'filter')
 */
type Predicate = (String, ValueType => Boolean)

/** Boolean function that uses the values for two given column names (String, String)
 *  in the predicate (e.g., used by 'thetajoin')
 */
type Predicate2 = (String, String, (ValueType, ValueType) => Boolean)

/** Indicates which relation and which column an aggregate is to be applied to
 */
type AggFunction = (Table, String) => Vect

/** Aggregate column type has Aggregate Funtion, new column name and old column name
 */
type AggColumn = (AggFunction, String, String)

/** Type definition for a row/tuple
 */
type Row = Vector [ValueType]

/** The base directory for data files
 */
val BASE_DIR = DATA_DIR + "relation" + ⁄


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Table` object provides functions for the `Table` trait.
 */
object Table:

    private val flaw    = flawf ("Table")                  // flaw function
    private var _ucount = 0                                // counter for making unique table names

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the next unique count.
     */
    def ucount (): Int = { _ucount += 1; _ucount }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given row 'tuple', project onto the given column positions specified in 'cPos'.
     *  @param tuple  the row on which to apply the projection
     *  @param cPos   the column positions 
     */
    def project (tuple: Row, cPos: IndexedSeq [Int]): Row =
        cPos.map (tuple(_)).toVector
    end project

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a domain string 'dom', project onto the given column positions specified
     *  in 'cPos'.
     *  @param dom   the domain string on which to apply the projection
     *  @param cPos  the column positions 
     */
    def projectD (dom: String, cPos: IndexedSeq [Int]): String =
        if dom != null then
            val sb = new StringBuilder
            for i <- cPos do sb.append (dom(i))
            sb.toString
        else null
    end projectD
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether 't' and 'u' are the same on column positions 'tp' and 'up'.
     *  @param t   the first tuple
     *  @param u   the second tuple
     *  @param tp  the column positions for tuple t
     *  @param up  the column positions for tuple u
     */
    def sameOn (t: Row, u: Row, tp: ArrayBuffer [Int], up: ArrayBuffer [Int]): Boolean =
        project (t, tp) sameElements project (u, up)
    end sameOn

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a tuple with missing values for each column according to the given
     *  domains.  This function is used by 'leftJoin' and 'rightJoin'.
     *  @param domain  the domains of the table for which a null tuple is required
     */
    def nullTuple (domain: String): Row =
        var v = Array.ofDim [ValueType] (domain.length)
        v.indices.map (i =>
            domain(i) match {
                case 'D' => v(i) = NO_DOUBLE
                case 'I' => v(i) = NO_INT
                case 'L' => v(i) = NO_LONG
                case 'S' => v(i) = NO_STRING
                case _ => flaw ("nullTuple", s"not supported domain type ${domain(i)}")
            })
        v.toVector
    end nullTuple

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert a `VectorS` into a `VectorI` by mapping each distinct value in
     *  `VectorS` into a distinct numeric integer value, returning the new vector
     *  and the bidirectional mapping.  Use the 'from' method in `BiMap` to recover
     *  the original string.
     *  e.g., VectorS ("A", "B", "C", "A", "D") will be mapped to VectorI (0, 1, 2, 0, 3)
     *  @param s  the vector of string numbers to convert
     */
    def map2Int (s: VectorS): (VectorI, BiMap [String, Int]) =
        val map = new BiMap [String, Int] ()
        var counter = 0
        for i <- s.indices if ! (map contains (s(i))) do
            map     += s(i) -> counter
            counter += 1
        end for
        val c = new VectorI (s.dim)
        for i <- s.indices do c(i) = map(s(i))
        (c, map)
    end map2Int

end Table

import Table._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Table` trait defines methods for operating on vectors.
 *  The vectors form the columns of the columnar relational datastore.
 *  Columns may have any of the following types:
 *      D - `Double`   - `VectorD` -  64 bit double precision floating point number
 *      I - `Int`      - `VectorI` -  32 bit integer
 *      L - `Long`     - `VectorL` -  64 bit long integer
 *      S - `StrNum`   - `VectorS` -  variable length numeric string
 */
trait Table:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the size in terms of number of columns in the table.
     */
    def cols: Int

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return all of the columns in the table.
     */
    def columns: Vector [Vect]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the column in the table with column name cName.
     *  @param cName  column name used to retrieve the column vector
     */
    def column (cName: String): Vect

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the names of columns in the table.
     */
    def colNames: ArrayBuffer [String]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the mapping from column names to column positions.
     */
    def colsMap: Map [String, Int]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the domains for the columns in the table.
     */
    def domains: String

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the range of index values for the table.
     */
    def indices: Range = 0 until rows

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the size in terms of number of rows in the table.
     */
    def rows: Int

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a row by pulling values from all columns at position i.
     *  @param i  the i-th position
     */
    def row (i: Int): Row

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a row by pulling values from an array of strings and converting
     *  elements to their appropriate types.
     *  @param sos   the sequence of strings holding the values
     *  @param _typ  the string of corresponding types, e.g., 'SDI'
     */
    @throws (classOf [Exception])
    def row (sos: ArrayBuffer [String], _typ: String): Row =
        var result: Vector [ValueType] = null
        val typ = if _typ == null then "S" * sos.length else _typ    // missing => assume String
        try
            result = (for j <- sos.indices yield
                typ(j) match
                case 'D' => if sos(j).isEmpty then 0.0 else new StringOps (sos(j)).toDouble
                case 'I' => if sos(j).isEmpty then 0   else new StringOps (sos(j)).toInt
                case 'L' => if sos(j).isEmpty then 0L  else new StringOps (sos(j)).toLong
                case _   => sos(j)
                end match
            ).toVector.asInstanceOf [Vector [ValueType]]
        catch
            case ex: Exception =>
                println (s"row function throw exception, row is:\n $sos \ntuple length is: ${sos.size}")
                throw ex
        end try
        result
    end row

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether this table contains a row matching the given 'tuple'.
     *  @param tuple  an aggregation of columns values (potential row)
     */
    def contains (tuple: Row): Boolean

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Rename this table, returning a shallow copy of the table.
     *  @param newName  the new name for the table.
     */
    def rename (newName: String): Table

    // ================================================================= PROJECT

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Project onto the columns with the given column names.
     *  @param cName  the names of the columns to project onto
     */
    def project (cName: String*): Table

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Project onto the columns with the given column names.
     *  @param cName  the names of the columns to project onto
     */
    def π (cName: String*): Table = project (cName :_*)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Project onto the columns with the given column positions.
     *  @param cPos   the column positions to project onto
     *  @param cName  the optional new names for the columns to project onto
     */
    def project (cPos: IndexedSeq [Int], cName: ArrayBuffer [String] = null): Table

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Project onto the columns with the given column positions.
     *  @param cPos   the column positions to project onto
     *  @param cName  the optional new names for the columns to project onto
     */
    def π (cPos: IndexedSeq [Int], cName: ArrayBuffer [String] = null): Table = project (cPos, cName)

    // ========================================================== PROJECT-SELECT

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Select elements from column cName in this table that satisfy the
     *  predicate p and project onto that column.
     *  @param cName  the name of the column used for selection
     *  @param p      the predicate (`Boolean` function) to be satisfied
     */
    def selproject (cName: String, p: ValueType => Boolean): Table

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Select elements from column cName in this table that satisfy the
     *  predicate p and project onto that column.
     *  @param cName  the name of the column used for selection
     *  @param p      the predicate (`Boolean` function) to be satisfied
     */
    def σπ (cName: String, p: ValueType => Boolean): Table = selproject (cName, p)

    // ======================================================== EXTENDED PROJECT

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Aggregate/project on the given columns (an extended projection operator that
     *  applies aggregate operators to aggregation columns and regular projection
     *  to projection columns).
     *  @see en.wikipedia.org/wiki/Relational_algebra
     *  @param aggCol  the columns to aggregate on: (aggregate function, new column name, old column name)*
     *  @param cName   the other columns to project on
     */
//  def eproject (aggCol: AggColumn*)(cName: String*): Table

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Aggregate/project on the given columns (an extended projection operator that
     *  applies aggregate operators to aggregation columns and regular projection
     *  to projection columns).
     *  @see en.wikipedia.org/wiki/Relational_algebra
     *  @param aggCol  the columns to aggregate on: (aggregate function, new column name, old column name)*
     *  @param cName   the other columns to project on
     */
//  def Π (aggCol: AggColumn*)(cName: String*): Table = eproject (aggCol :_*)(cName :_*)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Aggregate/project on the given column (an extended projection operator that
     *  applies an aggregate operator to an aggregation column and regular projection
     *  to projection columns).
     *  @see en.wikipedia.org/wiki/Relational_algebra
     *  @param aggr_func  the aggregate function
     *  @param newcol     the new column name
     *  @param oldcol     the old column name
     */
    def eproject (aggr_func: AggFunction, newcol: String, oldcol: String): Table

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Aggregate/project on the given column (an extended projection operator that
     *  applies an aggregate operator to an aggregation column and regular projection
     *  to projection columns).
     *  @see en.wikipedia.org/wiki/Relational_algebra
     *  @param aggr_func  the aggregate function
     *  @param newcol     the new column name
     *  @param oldcol     the old column name
     */
    def Π (aggr_func: AggFunction, newcol: String, oldcol: String): Table =
        eproject (aggr_func, newcol, oldcol)
    end Π

    // ================================================================== SELECT

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Select elements from columns in cName in this table that satisfy
     *  the predicate 'p'.
     *  @param cName  the name of the column used for selection
     *  @param p      the predicate (`Boolean` function) to be satisfied
     */
    def select (cName: String, p: ValueType => Boolean): Table

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Select elements from columns in cName in this table that satisfy
     *  the predicate 'p'.
     *  @param cName  the name of the column used for selection
     *  @param p      the predicate (`Boolean` function) to be satisfied
     */
    def σ (cName: String, p: ValueType => Boolean): Table = select (cName, p)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Select across all columns at the specified row positions.
     *  @param pos  the specified row positions
     */
    def selectAt (pos: collection.immutable.IndexedSeq [Int]): Table

    // =========================================================== SET OPERATORS

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Union this table and r2.  Check that the two tables are compatible.
     *  @param r2  the other table
     */
    def union (r2: Table): Table 

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Union this table and r2.  Check that the two tables are compatible.
     *  @param r2  the other table
     */
    def ⋃ (r2: Table): Table = union (r2)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Intersect this table and r2.  Check that the two tables are compatible.
     *  @param r2  the other table
     */
    def intersect (r2: Table): Table

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Intersect this table and r2.  Check that the two tables are compatible.
     *  @param r2  the other table
     */
    def ⋂ (r2: Table): Table = intersect (r2)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Take the difference of this table and r2 (this - r2).  Check that
     *  the two tables are compatible.
     *  @param r2  the other table
     */
    def minus (r2: Table): Table

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Take the difference of this table and r2 (this - r2).  Check that
     *  the two tables are compatible.
     *  @param r2  the other table
     */
    def - (r2: Table): Table = minus (r2)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether this table and r2 are incompatible by having
     *  differing numbers of columns or differing domain strings.
     *  @param r2  the other table
     */
    def incompatible (r2: Table): Boolean

    // ================================================================= PRODUCT

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the Cartesian product of this table and r2 (this × r2).
     *  @param r2  the second table
     */
    def product (r2: Table): Table

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the Cartesian product of this table and r2 (this × r2).
     *  @param r2  the second table
     */
    def × (r2: Table): Table = product (r2)

    // ==================================================================== JOIN

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Join this table and r2 by performing a "natural-join".  Rows from both
     *  tables are compared requiring agreement on common attributes (column names).
     *  @param r2  the rhs table in the join operation
     */
    def join (r2: Table): Table = join (colNames intersect r2.colNames, r2)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Join this table and r2 by performing a "natural-join".  Rows from both
     *  tables are compared requiring agreement on common attributes (column names).
     *  @param r2  the rhs table in the join operation
     */
    def ⋈ (r2: Table): Table = join (r2)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Join this table and r2 by performing a "natural-join".  Rows from both
     *  tables are compared requiring cName values to be equal.
     *  @param cName  the common join column name for both table
     *  @param r2     the rhs table in the join operation
     */
    def join (cName: String, r2: Table): Table = join (ArrayBuffer (cName), r2)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Join this table and r2 by performing a "natural-join".  Rows from both
     *  tables are compared requiring cName values to be equal.
     *  @param cName  the common join column name for both table
     *  @param r2     the rhs table in the join operation
     */
    def ⋈ (cName: String, r2: Table): Table = join (ArrayBuffer (cName), r2)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Join this table and r2 by performing a "natural-join".  Rows from both
     *  tables are compared requiring cName values to be equal.
     *  @param cName  the common join column names for both table
     *  @param r2     the rhs table in the join operation
     */
    def join (cName: ArrayBuffer [String], r2: Table): Table

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Join this table and r2 by performing a "natural-join".  Rows from both
     *  tables are compared requiring cName values to be equal.
     *  @param cName  the common join column names for both table
     *  @param r2      the rhs table in the join operation
     */
    def ⋈ (cName: ArrayBuffer [String], r2: Table): Table = join (cName, r2)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Join this table and r2 by performing an "equi-join".  Rows from both
     *  tables are compared requiring cName1 values to equal cName2 values.
     *  Disambiguate column names by appending "2" to the end of any duplicate column name.
     *  @param cName1  the join column name of this table (e.g., the Foreign Key)
     *  @param cName2  the join column name of table r2 (e.g., the Primary Key)
     *  @param r2      the rhs table in the join operation
     */
    def join (cName1: String, cName2: String, r2: Table): Table =
        join (ArrayBuffer (cName1), ArrayBuffer (cName2), r2)
    end join

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Join this table and r2 by performing an "equi-join".  Rows from both
     *  tables are compared requiring cName1 values to equal cName2 values.
     *  Disambiguate column names by appending "2" to the end of any duplicate column name.
     *  @param cName1  the join column name of this table (e.g., the Foreign Key)
     *  @param cName2  the join column name of table r2 (e.g., the Primary Key)
     *  @param r2      the rhs table in the join operation
     */
    def ⋈ (cName1: String, cName2: String, r2: Table): Table =
        join (ArrayBuffer (cName1), ArrayBuffer (cName2), r2)
    end ⋈

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Join this table and r2 by performing an "equi-join".  Rows from both
     *  tables are compared requiring cName1 values to equal cName2 values.
     *  Disambiguate column names by appending "2" to the end of any duplicate column name.
     *  @param cName1  the join column names of this table (e.g., the Foreign Key)
     *  @param cName2  the join column names of table r2 (e.g., the Primary Key)
     *  @param r2      the rhs table in the join operation
     */
    def join (cName1: ArrayBuffer [String], cName2: ArrayBuffer [String], r2: Table): Table

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Join this table and r2 by performing an "equi-join".  Rows from both
     *  tables are compared requiring cName1 values to equal cName2 values.
     *  Disambiguate column names by appending "2" to the end of any duplicate column name.
     *  @param cName1  the join column names of this table (e.g., the Foreign Key)
     *  @param cName2  the join column names of table r2 (e.g., the Primary Key)
     *  @param r2       the rhs table in the join operation
     */
    def ⋈ (cName1: ArrayBuffer [String], cName2: ArrayBuffer [String], r2: Table): Table =
        join (cName1, cName2, r2)
    end ⋈

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The theta join, handle the predicates in where are connect by "and" (where a....and b....).
     *  @param r2  the second table
     *  @param p0  the first theta join predicate (r1 cName, r2 cName, predicate to compare these two column)
     *  @param p   the rest of theta join predicates (r1 cName, r2 cName, predicates to compare these two column)
     */
    def join (r2: Table, p0: Predicate2, p: Predicate2*): Table

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The theta join, handle the predicates in where are connect by "and" (where a....and b....).
     *  @param r2  the second table
     *  @param p0  the first theta join predicate (r1 cName, r2 cName, predicate to compare these two column)
     *  @param p   the rest of theta join predicates (r1 cName, r2 cName, predicates to compare these two column)
     */
    def ⋈ (r2: Table, p0: Predicate2, p: Predicate2*): Table =
        join (r2, p0, p :_*)
    end ⋈

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Join this table and r2 by performing an "left-join".  Rows from both
     *  tables are compared requiring cName1 values to equal cName2 values.
     *  Disambiguate column names by appending "2" to the end of any duplicate column name.
     *  All rows from the left table are maintained with missing values indicators used
     *  where needed.
     *  @param cName1  the join column names of this table (e.g., the Foreign Key)
     *  @param cName2  the join column names of table r2 (e.g., the Primary Key)
     *  @param r2      the rhs table in the join operation
     */
    def leftJoin (cName1: String, cName2: String, r2: Table): Table

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Join this table and r2 by performing an "left-join".  Rows from both
     *  tables are compared requiring cName1 values to equal cName2 values.
     *  Disambiguate column names by appending "2" to the end of any duplicate column name.
     *  All rows from the left table are maintained with missing values indicators used
     *  where needed.
     *  Note: although this is the semi-join symbol, due to Unicode limitations, it is
     *  used for left-join.
     *  @param cName1  the join column names of this table (e.g., the Foreign Key)
     *  @param cName2  the join column names of table r2 (e.g., the Primary Key)
     *  @param r2      the rhs table in the join operation
     */
    def ⋉ (cName1: String, cName2: String, r2: Table): Table = leftJoin (cName1, cName2, r2)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Join this table and r2 by performing an "apprioxmate left-join".  Rows from both
     *  tables are compared requiring cName1 values to apprximately equal cName2 values.
     *  Disambiguate column names by appending "2" to the end of any duplicate column name.
     *  All rows from the left table are maintained with missing values indicators used
     *  where needed.
     *  @param thres   the approximate equality threshold
     *  @param cName1  the join column names of this table (e.g., the Foreign Key)
     *  @param cName2  the join column names of table r2 (e.g., the Primary Key)
     *  @param r2      the rhs table in the join operation
     */
    def leftJoinApx (thres: Double = 0.001) (cName1: String, cName2: String, r2: Table): Table

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Join this table and r2 by performing an "approximate left-join".  Rows from both
     *  tables are compared requiring cName1 values to apprximately equal cName2 values.
     *  Disambiguate column names by appending "2" to the end of any duplicate column name.
     *  All rows from the left table are maintained with missing values indicators used
     *  where needed.
     *  @param cName1  the join column names of this table (e.g., the Foreign Key)
     *  @param cName2  the join column names of table r2 (e.g., the Primary Key)
     *  @param r2      the rhs table in the join operation
     */
    def ⋉ (thres: Double = 0.001) (cName1: String, cName2: String, r2: Table): Table =
    {
        leftJoinApx (thres)(cName1, cName2, r2)
    } // ⋉ 

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Join this table and r2 by performing an "right-join".  Rows from both
     *  tables are compared requiring cName1 values to equal cName2 values.
     *  Disambiguate column names by appending "2" to the end of any duplicate column name.
     *  All rows from the right table are maintained with missing values indicators used
     *  where needed.
     *  @param cName1  the join column names of this table (e.g., the Foreign Key)
     *  @param cName2  the join column names of table r2 (e.g., the Primary Key)
     *  @param r2      the rhs table in the join operation
     */
    def rightJoin (cName1: String, cName2: String, r2: Table): Table = r2.leftJoin (cName2, cName1, this)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Join this table and r2 by performing an "right-join".  Rows from both
     *  tables are compared requiring cName1 values to equal cName2 values.
     *  Disambiguate column names by appending "2" to the end of any duplicate column name.
     *  All rows from the right table are maintained with missing values indicators used
     *  where needed.
     *  @param cName1  the join column names of this table (e.g., the Foreign Key)
     *  @param cName2  the join column names of table r2 (e.g., the Primary Key)
     *  @param r2      the rhs table in the join operation
     */
    def ⋊ (cName1: String, cName2: String, r2: Table): Table = r2.leftJoin (cName2, cName1, this)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Combine two sequences of column names, keeping all names from cn1 and
     *  only those in cn2 that are not repeats (i.e., not already in cn1).
     *  @param cn1  the first sequence of column names
     *  @param cn2  the second sequence of column names
     */
    protected def uniq_union (cn1: ArrayBuffer [String], cn2: ArrayBuffer [String]): ArrayBuffer [String] =
        var cn3 = cn1
        for j <- cn2.indices if ! (cn3 contains cn2(j)) do cn3 = cn3 :+ cn2(j)
        cn3
    end uniq_union

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Combine two sequences of column names and disambiguate any repeated names
     *  by appending "2".
     *  @param cn1  the first sequence of column names
     *  @param cn2  the second sequence of column names 
     */
    protected def disambiguate (cn1: ArrayBuffer [String], cn2: ArrayBuffer [String]): ArrayBuffer [String] =
        val n1 = cn1.length
        val cn = ArrayBuffer [String] ()
        for j <- 0 until n1 + cn2.length yield
            cn += (if j < n1 then cn1(j)
                   else { val nm2 = cn2(j - n1); if cn1 contains nm2 then nm2 + "2" else nm2 })
        end for
        cn
    end disambiguate

    // ================================================================ GROUP BY

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Group this table by specified column name, returning this table.
     *  @param cName  the group column
     */
    def groupBy (cName: String): Table
//  def groupBy (cName: String*): Table

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Group this table by specified column name, returning this table.
     *  @param cName  the group column
     */
    def γ (cName: String): Table = groupBy (cName)
//  def γ (cName: String*): Table = groupBy (cName :_*)

    // ================================================================= ORDER BY

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Order (ascending) the rows in the table by the selected columns cName.
     *  A stable sorting is used to allow sorting on multiple columns.
     *  @param cName  the column names that are to be sorted
     */
    def orderBy (cName: String*): Table

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Order (ascending) the rows in the table by the selected columns cName.
     *  A stable sorting is used to allow sorting on multiple columns.
     *  @param cName  the column names that are to be sorted
     */
    def ω (cName: String*): Table = orderBy (cName :_*)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Order (descending) the rows in the table by the selected columns cName.
     *  A stable sorting is used to allow sorting on multiple columns.
     *  @param cName  the column names that are to be sorted
     */
    def reverseOrderBy (cName: String*): Table

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Order (descending) the rows in the table by the selected columns cName.
     *  A stable sorting is used to allow sorting on multiple columns.
     *  @param cName  the column names that are to be sorted
     */
    def ωω (cName: String*): Table = reverseOrderBy (cName :_*)

    // ================================================================= UPDATES

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add tuple to this table as a new row.
     *  @param tuple  an aggregation of columns values (new row)
     */
    def add (tuple: Row): Unit

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add a tuples to this table as a new row, materialize and return updated table.
     *  May call for last tuple in a batch of tuples.
     *  @param tuple  an aggregation of columns values (new row)
     */
    def addm (tuple: Row): Table

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Update the column named cName using function func for elements with
     *  value matchStr.
     *  @param cName     the name of the column to be updated
     *  @param newVal    the value used to assign updated values
     *  @param matchVal  the value to be matched to elements
     */
    def update (cName: String, newVal: ValueType, matchVal: ValueType): Unit

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Update the column named cName using function func for elements with
     *  value matchStr.
     *  @param cName     the name of the column to be updated
     *  @param func      the function used to assign updated values
     *  @param matchVal  the value to be matched to elements
     */
    def update (cName: String, func: (ValueType) => ValueType, matchVal: ValueType): Unit

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Update the column named cName using function func for elements where
     *  the predicate 'pred' evaluates to true.
     *  @param cName  the name of the column to be updated
     *  @param func   the function used to assign updated values         // FIX - generalize type
     *  @param pred   the predicated used to select elements for update
     */
    def update (cName: String, func: (ValueType) => ValueType, pred: (ValueType) => Boolean): Unit

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Delete the rows from this table that satisfy the predicates.
     *  @param  p  tuple(1): column name, tuple(2): predicate (ValueType => Boolean)
     */
    def delete (p: Predicate*): Table

    // =============================================================== TO MATRIX

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert this table into a matrix of doubles, e.g., 
     *       in the regression equation: xb = y create matrix xy
     *  @param colPos  the column positions to use for the matrix
     */
    def toMatrixD (colPos: ArrayBuffer [Int]): MatrixD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert this table into a matrix of doubles and a vector of doubles.
     *       in the regression equation: xb = y create matrix x and vector y
     *  @param colPosM  the column positions to use for the matrix
     *  @param colPosV  the column position to use for the vector
     */
    def toMatrixDD (colPosM: ArrayBuffer [Int], colPosV: Int): (MatrixD, VectorD)

    // =============================================================== TO VECTOR

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert the colPos column of this relation into a vector of doubles.
     *  @param colPos  the column position to use for the vector
     */
    def toVectorD (colPos: Int = 0): VectorD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert the colName column of this relation into a vector of doubles.
     *  @param colName  the column name to use for the vector
     */
    def toVectorD (colName: String): VectorD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert the colPos column of this relation into a vector of integers.
     *  @param colPos  the column position to use for the vector
     */
    def toVectorI (colPos: Int = 0): VectorI

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert the colName column of this relation into a vector of integers.
     *  @param colName  the column name to use for the vector
     */
    def toVectorI (colName: String): VectorI

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert the colPos column of this relation into a vector of long integers.
     *  @param colPos  the column position to use for the vector
     */
    def toVectorL (colPos: Int = 0): VectorL

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert the colName column of this relation into a vector of long integers.
     *  @param colName  the column name to use for the vector
     */
    def toVectorL (colName: String): VectorL

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert the colPos column of this relation into a vector of string-num.
     *  @param colPos  the column position to use for the vector
     */
    def toVectorS (colPos: Int = 0): VectorS

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert the colName column of this relation into a vector of string-num.
     *  @param colName  the column name to use for the vector
     */
    def toVectorS (colName: String): VectorS

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Save this table in a file using serialization.
     */
    def save (): Unit

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Show this relation row by row.
     *  @param limit  the limit on the number of tuples to display
     */
    def show (limit: Int = Int.MaxValue): Unit

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Write this table into a CSV file with each row written to a line.
     *  @param fileName  the file name of the data file
     */
    def writeCSV (fileName: String): Unit

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Write this table into a JSON file.
     *  @param fileName  the file name of the data file
     */
    def writeJSON (fileName: String): Unit
  
end Table

