
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Yang Fan, Vinay Bingi, Santosh Uttam Bobade
 *  @version 1.6
 *  @date    Sun Aug 23 15:42:06 EDT 2015
 *  @see     LICENSE (MIT style license file).
 *
 *  An implementation supporting columnar relational databases facilitating easy
 *  and rapid analytics.  The columns in a relation are vectors from the
 *  `scalation.linalgebra` package.  Vectors and matrices may be readily extracted
 *  from a relation and feed into any of the numerous analytics techniques provided
 *  in `scalation.analytics`.  The implementation provides most of the columnar
 *  relational algebra operators given in the following paper:
 *  @see db.csail.mit.edu/projects/cstore/vldb.pdf
 *
 *  Some of the operators have unicode versions: @see `scalation.util.UnicodeTest`
 *
 *  Supports Time Series Databases (TSDB) via `TimeNum` domain/datatype and leftJoinApx
 *  rightJoinApx methods.
 */

package scalation
package database
package relation

import java.io._

import scala.collection.mutable.{ArrayBuffer => VEC, HashMap, IndexedSeq, Map}
import scala.math.min
import scala.io.Source.fromInputStream

import scalation.mathstat._

import Table._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Relation` companion object provides additional functions for the `Relation`
 *  class.
 *  FIX - apply methods - make compatible with RelationSQL
 */
object Relation:

    private val flaw = flawf ("Relation")                                  // flaw function

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create an unpopulated relation.
     *  @param name     the name of the relation
     *  @param key      the column number for the primary key (< 0 => no primary key)
     *  @param domain   an optional string indicating domains for columns (e.g., 'SD' = 'String', 'Double')
     *  @param colName  the names of columns
     */
    def apply (name: String, key: Int, domain: String, colName: String*): Relation =
        val n = colName.length
        val colName_ = VEC (colName :_* )
        new Relation (name, colName_, Vector.fill [Vect] (n)(null), key, domain)
    end apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create an unpopulated relation.
     *  @param name     the name of the relation
     *  @param key      the column number for the primary key (< 0 => no primary key)
     *  @param domain   an optional string indicating domains for columns (e.g., 'SD' = 'String', 'Double')
     *  @param colName  the names of columns
     */
    def apply (name: String, key: Int, domain: String, colName: VEC [String]): Relation =
        val n = colName.length
        new Relation (name, colName, Vector.fill [Vect] (n)(null), key, domain)
    end apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a relation from a sequence of row/tuples.  These rows must be converted
     *  to columns.
     *  @param name     the name of the relation
     *  @param colName  the names of columns
     *  @param row      the sequence of rows to be converted to columns for the columnar relation
     *  @param key      the column number for the primary key (< 0 => no primary key)
     *  @param domain   an optional string indicating domains for columns (e.g., 'SD' = 'String', 'Double')
     */
    def apply (name: String, colName: VEC [String], row: VEC [Row], key: Int, domain: String): Relation =
        val equivCol = Vector.fill [Vect] (colName.length)(null)
        val r2 = new Relation (name, colName, equivCol, key, domain)
        for tuple <- row do r2.add (tuple)
        r2.materialize ()
    end apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a relation from a sequence of row/tuples.  These rows must be converted
     *  to columns.
     *  @param name     the name of the relation
     *  @param colName  the names of columns
     *  @param row      the sequence of rows to be converted to columns for the columnar relation
     *  @param key      the column number for the primary key (< 0 => no primary key)
     */
    def apply (name: String, colName: VEC [String], row: VEC [Row], key: Int): Relation =
        val equivCol = Vector.fill [Vect] (colName.length)(null)
        val r2 = new Relation (name, colName, equivCol, key, null)
        for tuple <- row do r2.add (tuple)
        r2.materialize ()
    end apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Read the relation with the given name into memory using serialization.
     *  @param name  the name of the relation to load
     */
    def apply (name: String): Relation =
        val ois = new ObjectInputStream (new FileInputStream (STORE_DIR + name + SER))
        val obj = ois.readObject ()
        ois.close ()
        val res = obj.asInstanceOf [Relation]
        res
    end apply

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Read the relation with the given name into memory loading its columns
     *  with data from the CSV file named fileName.
     *  Note: "ln.split (eSep, -1)" will keep all values even if empty "one,,three" -> "one","",three"
     *  @param fileName  the file name of the data file
     *  @param name      the name of the relation
     *  @param colName   the names of columns
     *  @param key       the column number for the primary key (< 0 => no primary key)
     *  @param domain    an optional string indicating domains for columns (e.g., 'SD' = 'String', 'Double')
     *  @param skip      the number of lines in the CSV file to skip (e.g., header line(s))
     *  @param eSep      the element separation string/regex (e.g., "," ";" " +")
     */
    def apply (fileName: String, name: String, colName: VEC [String], key: Int,
               domain: String, skip: Int, eSep: String): Relation =
        var cnt    = skip
        val lines  = getFromURL_File (fileName)
        val newCol = Vector.fill [Vect] (colName.length)(null)
        val r3     = new Relation (name, colName, newCol, key, domain)
        for ln <- lines do
            val buf = VEC.from (ln.split (eSep, -1))
            if cnt <= 0 then r3.add (r3.row (buf, domain)) else cnt -= 1
        end for
        r3.materialize ()
    end apply

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Read the relation with the given name into memory loading its columns
     *  with data from the CSV file named fileName.  In this version, the column
     *  names are read from the first line of the file.
     *  @param fileName  the file name of the data file
     *  @param name      the name of the relation
     *  @param key       the column number for the primary key (< 0 => no primary key)
     *  @param domain    an optional string indicating domains for columns (e.g., 'SD' = 'String', 'Double')
     *  @param eSep      the element separation string/regex (e.g., "," ";" " +")
     *  @param cPos      the sequence of column positions in the input file to be used (null => select all)
     */
    def apply (fileName: String, name: String, key: Int, domain: String, eSep: String,
               cPos: VEC [Int]): Relation =
        val lines = getFromURL_File (fileName)
        var first = true
        var colBuffer: Array [VEC [String]] = null
        var colName:   VEC [String] = null
        var newCol:    Vector [Vect] = null

        if cPos == null then                                            // select all columns
            for ln <- lines do
                if first then
                    colName   = VEC.from (ln.split (eSep, -1).map (_.trim))
                    colBuffer = Array.fill (colName.length)(new VEC ())
                    first = false
                else
                    val values = ln.split (eSep, -1).map (_.trim)
                    for i <- colName.indices do colBuffer(i) += values(i)
                end if
            end for
        else                                                            // select cPos columns
            if domain.length != cPos.length then
                flaw ("apply", "cPos length should be same as domain length")
            end if
            for ln <- lines do
                if first then
                    val name  = ln.split (eSep, -1).map (_.trim)
                    colName = VEC [String] ()
                    colBuffer = Array.fill (cPos.length)(new VEC ())
                    for i <- colBuffer.indices do colName += name(cPos(i))
                    first = false
                else
                    val values = ln.split (eSep, -1).map (_.trim)
                    for i <- colName.indices do colBuffer(i) += values(cPos(i))
                end if
            end for
        end if
        new Relation (name, colName, makeCol (colBuffer, domain), key, domain)
    end apply

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Make the columns for the columnar table from data stored in colBuffer.
     *  @param colBuffer  the column buffer holding the data
     *  @param domain     the domains/datatypes for the columns
     */
    private def makeCol (colBuffer: Array [VEC [String]], domain: String): Vector [Vect] =
        colBuffer.indices.map (i =>
            if domain == null || domain == "" then VectorS.fromStrings (colBuffer(i))
            else domain(i) match {
                case 'D' => VectorD.fromStrings (colBuffer(i))
                case 'I' => VectorI.fromStrings (colBuffer(i))
                case 'L' => VectorL.fromStrings (colBuffer(i))
                case 'S' => VectorS.fromStrings (colBuffer(i))
                case 'X' => VectorS.fromStrings (colBuffer(i))
                case _   => flaw ("makeCol", s"domain type ${domain(i)} not supported")
                            null.asInstanceOf [Vect]
        }).toVector.asInstanceOf [Vector [Vect]]
    end makeCol

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Read the relation with the given name into memory loading its columns
     *  with data from the CSV file named fileName.  In this version, the column
     *  names are read from the first line of the file.  It uses col2 which is a
     *  temporary VEC, and maintains indices.
     *  @param fileName  the file name of the data file
     *  @param name      the name of the relation
     *  @param key       the column number for the primary key (< 0 => no primary key)
     *  @param domain    an optional string indicating domains for columns (e.g., 'SD' = 'String', 'Double')
     *  @param eSep      the element separation string/regex (e.g., "," ";" " +")
     */
    def apply (fileName: String, name: String, domain: String, key: Int, eSep: String = ","): Relation =
        var first         = true
        val lines         = getFromURL_File (fileName)
        var r3: Relation  = null
        var currentlineno = 0

        for ln <- lines do
            if first then
                val colName = VEC.from (ln.split (eSep, -1))
                val newCol  = Vector.fill [Vect] (colName.length)(null)
                r3    = new Relation (name, colName, newCol, key, domain)
                first = false
            else
                if currentlineno % 1000 == 0 then println (s"$currentlineno")
                val buf = VEC.from (ln.split (eSep, -1))
                r3.add (r3.row (buf, domain))
                currentlineno += 1
            end if
        end for
        r3.materialize ()
    end apply

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Read the relation with the given name into memory loading its columns
     *  with data from the CSV file named fileName.  This version assumes
     *  defaults for eSep and skip of ("," and 0).
     *  @param fileName  the file name of the data file
     *  @param name      the name of the relation
     *  @param colName   the names of columns
     *  @param key       the column number for the primary key (< 0 => no primary key)
     *  @param domain    an optional string indicating domains for columns (e.g., 'SD' = 'String', 'Double')
     */
    def apply (fileName: String, name: String, colName: VEC [String], key: Int,
               domain: String): Relation =
        val eSep   = ","
        val lines  = getFromURL_File (fileName)
        val newCol = Vector.fill [Vect] (colName.length)(null)
        val r3     = new Relation (name, colName, newCol, key, domain)
        for ln <- lines do
            val buf = VEC.from (ln.split (eSep, -1))
            r3.add (r3.row (buf, domain))
        end for
        r3.materialize ()
    end apply

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Read the relation with the given name into memory loading its columns
     *  with data from the .arff file named fileName.
     *  @param fileName  the file name of the data file
     *  @param key       the column number for the primary key (< 0 => no primary key)
     *  @param domain    an optional string indicating domains for columns (e.g., 'SD' = 'String', 'Double')
     */
    def apply (fileName: String, key: Int, domain: String): Relation =
        val eSep = "[, ]"
        val lines = getFromURL_File (fileName)
        var name: String = null
        var colBuffer: Array [VEC [String]] = null
        var colName = VEC [String]()
        var newCol: Vector [Vect] = null
        var foundData = false
        for ln <- lines do
            if ln.indexOf ("%") != 0 then                          // skip comment
                if ln.indexOf ("@relation") == 0 then
                    name = ln.split (eSep, -1)(1)
                else if ln.indexOf ("@attribute") == 0 then
                    colName += ln.split (eSep, -1)(1)
                else if ln.indexOf ("@data") == 0 then
                    foundData = true
                    colBuffer = Array.ofDim (colName.length)
                    for i <- colBuffer.indices do colBuffer (i) = new VEC ()
                else if foundData then
                    val values = ln.split (eSep, -1)
                    values.indices.foreach (i => { colBuffer (i) += values (i) })
                end if
            end if
        end for
        new Relation (name, colName, colBuffer.indices.map (i => VectorS (colBuffer(i))).toVector, key, domain)
    end apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Read the relation with the given name into memory from a JSON file.
     *  @see https://github.com/FasterXML/jackson-databind
     *  @author Shubham Vasant Shingate
     *  FIX - does not work Scala 2.13
     *  @param fileName  the file name of the JSON file
     *  @param name      the name of the relation to load
     *
    def apply (fileName: String, name: String): Relation =
        import scala.jdk.CollectionConverters.asScalaIteratorConverter
//      import scala.collection.JavaConverters.asScalaIteratorConverter

        import com.fasterxml.jackson.databind.ObjectMapper
        type JSON_ELEM = java.util.LinkedHashMap [String, String]
        type JSON_TYPE = java.util.List [JSON_ELEM]

        var jsonList: java.util.List [JSON_ELEM] = null
        try
            val objMapper = new ObjectMapper ()
            val jsonStr   = fromInputStream (new FileInputStream (fileName)).mkString
            jsonList      = objMapper.readValue (jsonStr, classOf [JSON_TYPE])
        catch
            case e: FileNotFoundException => flaw ("apply", s"file $fileName not found")
            case e: IOException           => flaw ("apply", s"unable to read $fileName: $e")
        end try

        var splitStr = jsonList.get(0).toString
        var arrSize  = 0
        var flag     = true
        val colNames = VEC [String] ()
        while arrSize != 1 do
            val subStr = splitStr.split ("=", 2)
            arrSize    = subStr.length
            if subStr(0).startsWith ("{") && arrSize != 1 then subStr(0) = subStr(0).substring(1)
            if ! flag && arrSize != 1 then subStr(0) = subStr(0).split (", ", 2)(1)
            if arrSize != 1 then
                colNames += subStr(0)
                splitStr  = subStr(1)
                flag      = false
            end if
        end while

        val rel = new Relation (name, colNames, Vector.fill [Vect] (colNames.length)(null), 0)
        for jsonData <- asScalaIteratorConverter (jsonList.iterator ()).asScala do
            val tuple  = VEC (asScalaIteratorConverter (jsonData.values().iterator()).asScala.toSeq :_*)
            rel.add (rel.row (tuple, null))
        end for
        rel.materialize ()
        rel
    } // apply
     */

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a relation from the xy matrix of doubles.
     *  @param xy       the matrix containing the data
     *  @param name     the name of the relation
     *  @param colName  the names of columns
     *  @param key      the column number for the primary key (< 0 => no primary key)
     *  @param domain   an optional string indicating domains for columns (e.g., 'SD' = 'String', 'Double')
     */
    def fromMatrixD (xy: MatrixD, name: String, colName: VEC [String], key: Int = -1,
                    domain: String = null): Relation =
        val newCol = for j <- xy.indices2 yield xy(?, j)
        new Relation (name, colName, newCol.toVector, key, domain)
    end fromMatrixD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a relation from the x matrix of doubles and y vector of doubles
     *  or integers.
     *  @param x        the matrix containing the data
     *  @param y        the vector containing the data
     *  @param name     the name of the relation
     *  @param colName  the names of columns
     *  @param key      the column number for the primary key (< 0 => no primary key)
     *  @param domain   an optional string indicating domains for columns (e.g., 'SD' = 'String', 'Double')
     */
    def fromMatrixDD (x: MatrixD, y: VectorD, name: String, colName: VEC [String], key: Int = -1,
                    domain: String = null): Relation =
        val newCol = for j <- x.indices2 yield x(?, j)
        new Relation (name, colName, newCol.toVector :+ y, key, domain)
    end fromMatrixDD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the count (number of elements) of each of the columns of columnar
     *  relation r.
     *  @param r  the given relation
     */
    def count (r: Relation): IndexedSeq [Int] = VEC (r.col.map (_.size) :_*)

    def count1 (r: Table, c: String): VectorI =
        val r2 = r.asInstanceOf [Relation]
        val newcol_vals = VEC [Int] ()
        val old_col = r.column (c)
        old_col.foreach (x => { newcol_vals += r2.groupMap (x).length })
        VectorI (newcol_vals)
    end count1

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the minimum of each of the columns of columnar relation r.
     *  @param r  the given relation
     */
    def min (r: Relation): Vector [Double] = for c <- r.col yield c.toDouble.min

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the maximum of each of the columns of columnar relation r.
     *  @param r  the given relation
     */
    def max (r: Relation): Vector [Double] = for c <- r.col yield c.toDouble.max

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the mean of each of the columns of columnar relation r.
     *  @param r  the given relation
     */
    def sum (r: Relation): Vector [Double] = for c <- r.col yield c.toDouble.sum

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the mean of each of the columns of columnar relation r.
     *  @param r  the given relation
     */
    def mean (r: Relation): Vector [Double] = for c <- r.col yield c.toDouble.mean

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the variance of each of the columns of columnar relation r.
     *  @param r  the given relation
     */
    def variance (r: Relation): Vector [Double] = for c <- r.col yield c.toDouble.variance

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get a Vect of sum of the cName column for the r relation base on each group,
     *  the result will be the same size.
     *  @param r      the relation to operate on
     *  @param cName  sum on column "cName"
     *
    def sum (r: Relation, cName: String): Vect =
        val cPos    = r.colMap.get(cName).get
        val domainc = r.domain(cPos)
        var columnlist: Vect = null
        var count   = 0
        var pointer = 0
        var sumlist: Vect = null
        for idx <- r.orderedIndex do
//          columnlist = Vect.:+ (columnlist,r.index(idx)(cPos),r.domain,cPos)
            columnlist = Vect.:+ (columnlist,r.index(idx)(cPos))
            if count +1 == r.grouplist(pointer) then
                val thisroundsum = Vec.sum(columnlist)
//              sumlist = Vect.:+ (sumlist, thisroundsum, r.domain, cPos)
                sumlist = Vect.:+ (sumlist, thisroundsum)
                columnlist = null
                pointer += 1
            end if
            count += 1
        end for
        sumlist
    end sum
     */

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get a Vect of max of the cName column for the r relation.
     *  @param r the relation you want to operate on
     *  @param cName  max on column "cName"
     *
    def max (r: Relation, cName: String): Vect =
        val cPos    = r.colMap.get(cName).get
        val domainc = r.domain(cPos)
        var columnlist: Vect = null
        var count   = 0
        var pointer = 0
        var maxlist: Vect = null
        for idx <- r.orderedIndex do
//          columnlist = Vect.:+ (columnlist,r.index(idx)(cPos),r.domain,cPos)
            columnlist = Vect.:+ (columnlist,r.index(idx)(cPos))
            if count + 1 == r.grouplist(pointer) then
                val thisroundsum = Vec.max(columnlist)
//              maxlist = Vect.:+ (maxlist, thisroundsum, r.domain, cPos)
                maxlist = Vect.:+ (maxlist, thisroundsum)
                columnlist = null
                pointer += 1
            end if
            count += 1
        end for
        maxlist
    end max
     */

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get a Vect of min of the cName column for the r relation
     *  @param r      the relation you want to operate on
     *  @param cName  min on column "cName"
     *
    def min (r: Relation, cName: String): Vect =
        val cPos    = r.colMap.get(cName).get
        val domainc = r.domain(cPos)
        var columnlist: Vect = null
        var count   = 0
        var pointer = 0
        var minlist: Vect = null
        for idx <- r.orderedIndex do
//          columnlist = Vect.:+ (columnlist,r.index(idx)(cPos),r.domain,cPos)
            columnlist = Vect.:+ (columnlist,r.index(idx)(cPos))
            if count + 1 == r.grouplist(pointer) then
                val thisroundsum = Vec.min(columnlist)
//              minlist = Vect.:+ (minlist, thisroundsum, r.domain, cPos)
                minlist = Vect.:+ (minlist, thisroundsum)
                columnlist = null
                pointer += 1
            end if
            count += 1
        end for
        minlist
    end min
     */

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get a Vect of average of the cName column for the r relation.
     *  @param r      the relation you want to operate on
     *  @param cName  average on column "cName"
     *
    def avg (r: Relation, cName: String): Vect =
        val cPos    = r.colMap.get(cName).get
        val domainc = r.domain(cPos)
        var columnlist: Vect = null
        var count   = 0
        var pointer = 0
        var avglist: Vect = null
        for idx <- r.orderedIndex do
//          columnlist = Vect.:+ (columnlist, r.index(idx)(cPos), r.domain, cPos)
            columnlist = Vect.:+ (columnlist, r.index(idx)(cPos))
            if count + 1 == r.grouplist(pointer) then
                val thisroundsum = Vec.mean(columnlist)
//              avglist = Vect.:+ (avglist, thisroundsum, r.domain, cPos)
                avglist = Vect.:+ (avglist, thisroundsum)
                columnlist = null
                pointer += 1
            end if
            count += 1
        end for
        avglist
    end avg
     */

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get a Vect of count of the cName column for the r relation.
     *  @param r      the relation you want to operate on
     *  @param cName  the column name for the column to be counted
     *
    def count (r: Relation, cName: String): Vect =
        val cPos = r.colMap.get (cName).get
        var countlist: Vect = null
        var i = 0
        for p <- r.grouplist do
            val count = p - i
//          countlist = Vect.:+ (countlist, count, r.domain, cPos)
            countlist = Vect.:+ (countlist, count)
            i = p
        end for
        countlist
    end count
     */

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get a Vect of count of the cName column for the r relation.
     *  @param r      the relation you want to operate on
     *  @param cName  the column name for the column to be counted
     */
    def count (r: Relation, cName: String): Relation =
        val cPos = r.colMap(cName)
        r.groupBy (cName)
        
        val res = for (k, v) <- r.groupMap yield (k, v.size)
        println (s"count: res = $res")

        val newCName  = VEC (cName, "count")
        val col0      = VectorS (res.keys.toVector.asInstanceOf [Vector [String]])  // FIX - generalize
        val col1      = VectorI (res.values.toVector)
        val newCol    = Vector (col0, col1).asInstanceOf [Vector [Vect]]
        println (s"newCol = $newCol")
        val newKey    = 0
        val newDomain = r.domain(cPos).toString + "I"
        new Relation (r.name + "_c_" + ucount (), newCName, newCol, newKey, newDomain)
    end count

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From function return cartesian product of all the relations.
     *  @param relations  the relations making up the from clause
     */
    def from (relations: Relation*): Relation =
        var result = relations(0)
        for i <- 1 until relations.size do result = result product relations(i)
        result
    end from

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test/execute the given query. 
     *  @param query   the query to execute
     *  @param result  the table resulting from the query
     */
    def test (query: String, result: Table): Unit =
        banner (s"test: query = $query")
        result.show ()
    end test

end Relation

import Relation._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Relation` class stores and operates on vectors.  The vectors form the
 *  columns of the columnar relational datastore.  Columns may have any of the
 *  following types:
 *      D - `Double`   - `VectorD` -  64 bit double precision floating point number
 *      I - `Int`      - `VectorI` -  32 bit integer
 *      L - `Long`     - `VectorL` -  64 bit long integer
 *      S - `String`   - `VectorS` -  variable length numeric string
 *      X - `String`   - `VectorS` -  variable length numeric string (extended column width)
 *  FIX - (1) don't allow (public) var
 *        (2) avoid unchecked or incomplete .asInstanceOf [T]
 *------------------------------------------------------------------------------
 *  @param name     the name of the relation
 *  @param colName  the names of columns
 *  @param col      the Scala Vector of columns making up the columnar relation
 *  @param key      the column number for the primary key (< 0 => no primary key)
 *  @param domain   an optional string indicating domains for columns (e.g., 'SD' = 'String', 'Double')
 *  @param fKeys    an optional sequence of foreign keys - VEC (column name, ref table name, ref column position)
 *  @param enter    whether to enter the newly created relation into the `Catalog`
 */
class Relation (val name: String, val colName: VEC [String], var col: Vector [Vect] = null,
                val key: Int = 0, val domain: String = null, var fKeys: VEC [(String, String, Int)] = null,
                enter: Boolean = true)
     extends Table with Serializable:

    private   val debug           = debugf ("Relation", true)                   // debug function
    private   val flaw            = flawf ("Relation")                          // flaw function
    private [relation] val colMap = Map [String, Int] ()                        // map column name -> column number
    private   val groupMap        = Map [ValueType, VEC [Int]] ()               // group rows by column value
//  private   var grouplist       = Vector [Int] ()                             // rows in group
    protected val index           = Map [ValueType, Row] ()                     // index that maps a key into row
    protected val indextoKey      = HashMap [Int, ValueType] ()                 // map index -> key
    private   var keytoIndex      = HashMap [ValueType, Int] ()                 // map key -> index
    protected var orderedIndex    = Vector [ValueType] ()                       // re-ordering of the key column
    protected var hasIndex        = false                                       // whether this relation has an non-empty index

    if col == null then col = Vector.fill [Vect] (colName.length)(null)
    if colName.length != col.length then flaw ("constructor", "incompatible sizes for 'colName' and 'col'")
//  if enter then Catalog.add (name, colName, key, domain)

    for j <- colName.indices do colMap += colName(j) -> j

    @transient
    private val col2 = Vector.fill (colName.size)(VEC [ValueType] ())   // efficient holding area for building columns

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The generateIndex method helps, e.g., the popTable, methods to generate
     *  an index for the table.
     *  @param reset  if reset is true, use old index to build new index; otherwise, create new index
     */
    def generateIndex (reset: Boolean = false): Unit =
        if ! reset then
            for i <- 0 until rows do
                val mkey = if key != -1 then row(i)(key)                        // key column is specified
                           else i                                               // key column is not specified
                val tuple    = row(i)
                index       += mkey -> tuple
                indextoKey  += i -> mkey
                keytoIndex  += mkey -> i
                orderedIndex = orderedIndex :+ mkey
            end for
        else                                                                    // use old index to build
            val newoderedIndex = new VEC [ValueType] ()
            val newkeytoIndex =  new HashMap [ValueType, Int] ()
            for i <- orderedIndex.indices do
                val mkey       = if key != -1 then orderedIndex(i) else i
                val tuple      = row(keytoIndex(mkey))
                index         += mkey -> tuple
                newkeytoIndex += mkey -> i
                newoderedIndex.update (newoderedIndex.length, mkey)
            end for
            orderedIndex = newoderedIndex.toVector                              // map old keytoIndex to rowIndex to
            keytoIndex   = newkeytoIndex
        end if
        hasIndex = true
    end generateIndex

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the size in terms of number of columns in the relation.
     */
    def cols: Int = col.length

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return all of the columns in the relation.
     */
    def columns: Vector [Vect] = col

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the column in the relation with column name cName.
     *  @param cName  column name used to retrieve the column vector
     */
    def column (cName: String): Vect = col(colMap (cName))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the names of columns in the relation.
     */
    def colNames: VEC [String] = colName

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the mapping from column names to column positions.
     */
    def colsMap: Map [String, Int] = colMap

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the domains for the columns in the relation.
     */
    def domains: String = domain

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a row by pulling values from all columns at position i.
     *  @param i  the i'th position
     */
    def row (i: Int): Row = for c <- col yield c(i)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the size in terms of number of rows in the relation.
     */
    def rows: Int = if col(0) == null then 0 else col(0).size

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether this relation contains a row matching the given tuple.
     *  @param tuple  an aggregation of columns values (potential row)
     */
    def contains (tuple: Row): Boolean =
        for i <- 0 until rows if row(i) sameElements tuple do return true
        false
    end contains

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Rename this table, returning a shallow copy of this table.
     *  @param newName  the new name for the table.
     */
    def rename (newName: String): Relation =
        new Relation (newName, colName, col, key, domain, fKeys)
    end rename

    // ================================================================= PROJECT

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Project onto the columns with the given column names.
     *  @param cName  the names of the columns to project onto
     */
    def project (cName: String*): Relation = project (VEC (cName.map (colMap (_)) :_*),
                                                      VEC (cName :_*))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Project onto the columns with the given column positions using the given
     *  column names.
     *  @param cPos   the positions of the columns to project onto
     *  @param cName  the names of the columns to project onto
     */
    def project (cPos: IndexedSeq [Int], cName: VEC [String] = null): Relation =
        val newCName  = if cName == null then for i <- cPos yield colName(i)
                        else cName
        val newCol    = cPos.map (col(_)).toVector
        val newKey    = if cPos contains key then cPos.indexOf (key) else -1
        val newDomain = projectD (domain, cPos)
        new Relation (name + "_p_" + ucount (), newCName.asInstanceOf [VEC [String]], newCol, newKey, newDomain)
    end project

    // ======================================================== EXTENDED PROJECT

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Aggregate/project on the given columns (an extended projection operator that
     *  applies aggregate operators to aggregation columns and regular projection
     *  to projection columns).
     *  @see en.wikipedia.org/wiki/Relational_algebra
     *  @param aggCol  the columns to aggregate on: (aggregate function, new column name, old column name)*
     *  @param cName   the other columns to project on
     *
    def eproject (aggCol: AggColumn*)(cName: String*): Relation =
        val aRange  = 0 until aggCol.size
        val nCols   = aggCol.size + cName.size
        val funName = VEC [String] ()
        for c <- aggCol do
            if ! (colName contains c._3) then throw new IllegalArgumentException (s"column ${c._3} to aggregate on does not exist")
            else funName += c._2
        end for

        if grouplist.isEmpty then groupBy (colName(key))
        val newCol    = Vector.fill [Vect] (nCols)(null)
        val newCName  = VEC ((cName ++ funName) :_*)
        var newDomain = cName.map (n => colMap(n)).map (i => domain(i))
        for i <- aRange do
            newDomain = if funName(i) contains "count" then newDomain :+ 'I'     // aggregate's result domain is based on aggregate column
                        else newDomain :+ domain(colMap(aggCol(i)._3))
        end for
        val r2 = new Relation (name + "_e_" + ucount (), newCName, newCol, key, newDomain.mkString (""))
        if rows == 0 then return r2                                              // no rows means early return

        val agglist = for i <- aRange yield aggCol(i)._1(this, aggCol(i)._3)
        if cName.size != 0 then
            val cPos    = VEC (cName.map (colMap(_)) :_*)                // position of cName
            val cPos2   = aggCol.map ((a: AggColumn) => colMap(a._3))            // position of aggregate columns
            val shrinkR = pi(cPos, null)                                         // projected relation
            var row_i   = 0
            var group_j = 0
            orderedIndex.foreach (idx => {
                var thisrow = shrinkR.row(keytoIndex(idx))
                for aggf <- agglist.indices do thisrow = thisrow :+ Vect (agglist (aggf), group_j)
                r2.add_ni (thisrow)
                row_i += 1
                if row_i == grouplist(group_j) then group_j += 1
            })
            r2.materialize ()
        else                                                                     // only project on the aggregate column
            for i <- aRange do
                r2.col = if i == 0 then Vector (agglist(i)) else r2.col :+ agglist(i)
            end for
        end if
        r2
    end eproject
     */

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Aggregate/project on the given column (an extended projection operator that
     *  applies an aggregate operator to an aggregation column and regular projection
     *  to projection columns).
     *  @see en.wikipedia.org/wiki/Relational_algebra
     *  @param aggr_func  the aggregate function - FIX - currently just a string
     *  @param newcol     the new column name
     *  @param oldcol     the old column name
     *  @param cName      the other columns to project on - FIX - missing
     */
//  def eproject (aggr_func: String, newcol: String, oldcol: String): Relation =
    def eproject (aggr_func: AggFunction, newcol: String, oldcol: String): Relation =
//      if aggr_func != "count" then
//          throw new IllegalArgumentException (s"aggr_func ${aggr_func} aggregate function does not exist")
//      end if
        if ! (colName contains oldcol) then
            throw new IllegalArgumentException (s"column ${oldcol} to aggregate on does not exist")
        end if
        println ("col_old " + col(colMap(oldcol)))
        val old_col = col(colMap(oldcol))
        val newcol_vals = VEC [Int] ()
        old_col.foreach (x => {newcol_vals += groupMap(x).length})
        val newdomain = "I" + domain(colMap(oldcol))
        println ("newcol_vals :" + newcol_vals)
        println (groupMap)
        val r2 = new Relation (name + "_sk_", VEC (newcol, oldcol),
                               Vector (VectorI (newcol_vals), old_col), -1, newdomain)
        r2.materialize ()
        r2.generateIndex ()
        println (r2)
        //r2.show() not working --- see internals of show and check with Prof
        r2
    end eproject

    def eproject (aggr_func: AggColumn): Relation =
        if ! (colName contains aggr_func._3) then
            throw new IllegalArgumentException (s"column ${aggr_func._3} to aggregate on does not exist")
        end if
        val funName   = aggr_func._1
        val newcol    = aggr_func._2
        val oldcol    = aggr_func._3
        val old_col   = col(colMap(oldcol))
        val newcol_vals = funName (this, oldcol)
        var newdomain = "I" + domain(colMap(oldcol))
        val r2 = new Relation (name + "_sk_", VEC (newcol, oldcol), Vector (newcol_vals, old_col), -1, newdomain)
        r2.materialize ()
        r2.generateIndex ()
        r2
    end eproject

    // ========================================================== PROJECT-SELECT

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Select elements from column cName in this relation that satisfy the
     *  predicate p and project onto that column.
     *  @param cName  the name of the column used for selection
     *  @param p      the predicate (`Boolean` function) to be satisfied
     */
    def selproject (cName: String, p: ValueType => Boolean): Relation =
        val nu     = getMeta (cName)
        val newCol = Vector (col (nu._1).filter (p)).asInstanceOf [Vector [Vect]]
        new Relation (name + "_s_" + ucount (), nu._2, newCol, nu._3, nu._4)
    end selproject

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get meta-data about the column with name cName.
     *  @param cName  the name of the column
     */
    private def getMeta (cName: String): (Int, VEC [String], Int, String) =
        val cn = colMap (cName)                                                 // column position
        (cn, VEC (cName), if cn == key then key else -1, projectD (domain, IndexedSeq (cn)))
    end getMeta

    // ================================================================== SELECT

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Select elements from columns in cName in this relation that satisfy
     *  the predicate p.
     *  @param cName  the name of the column used for selection
     *  @param p      the predicate (`Boolean` function) to be satisfied
     */
    def select (cName: String, p: ValueType => Boolean): Relation =
        val colc = col (colMap (cName))
        selectAt (colc.filterPos (p))
    end select

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Select across all columns at the specified column positions.
     *  @param pos  the specified column positions
     */
    def selectAt (pos: collection.immutable.IndexedSeq [Int]): Relation =
        val newCol = (for j <- col.indices yield col(j).at(pos)).asInstanceOf [Vector [Vect]]
        new Relation (name + "_s_" + ucount (), colName, newCol, key, domain)
    end selectAt

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the relation whose rows are equal to value in the column with the given name. 
     *  @param cv  the (column-name, value) pair, e.g., ("time", 5.00)
     */
    def == (cv: (String, ValueType)): Relation = select (cv._1, (x: ValueType) => x == cv._2)
    def != (cv: (String, ValueType)): Relation = select (cv._1, (x: ValueType) => x != cv._2)
    def <  (cv: (String, ValueType)): Relation = select (cv._1, (x: ValueType) => x < cv._2)
    def <= (cv: (String, ValueType)): Relation = select (cv._1, (x: ValueType) => x <= cv._2)
    def >  (cv: (String, ValueType)): Relation = select (cv._1, (x: ValueType) => x > cv._2)
    def >= (cv: (String, ValueType)): Relation = select (cv._1, (x: ValueType) => x >= cv._2)

    // =========================================================== SET OPERATORS

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Union all (dups allowed) this relation and r2.  Check that the two relations
     *  are compatible.  If they are not, return the first this relation.
     *  @param _r2  the other relation
     */
    def unionAll (_r2: Table): Relation =
        val r2 = _r2.asInstanceOf [Relation]
        if incompatible (r2) then return this                                   // take only this relation

        val newCol = (for j <- col.indices yield col(j) ++ r2.columns(j)).toVector.asInstanceOf [Vector [Vect]]
        new Relation (name + "_u_" + ucount (), colName, newCol, -1, domain)
    end unionAll

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Union this relation and r2.  Check that the two relations are compatible.
     *  If they are not, return the first this relation.
     *  @param _r2  the other relation
     */
    def union (_r2: Table): Relation =
        val r2 = _r2.asInstanceOf [Relation]
        if incompatible (r2) then return this                                   // take only this relation
        val newCol  = (for j <- col.indices yield (col(j) ++ r2.columns(j)).asInstanceOf [Vect])
        val newCols = newCol.transpose.distinct.transpose

        val ncl = VEC [Vect] ()
        for cl <- newCols do
            val first = cl(0)
            first match
            case _: Double => val rs = VectorD (for j <- cl yield j.asInstanceOf [Double]); ncl += rs
            case _: Int    => val rs = VectorI (for j <- cl yield j.asInstanceOf [Int]);    ncl += rs
            case _: Long   => val rs = VectorL (for j <- cl yield j.asInstanceOf [Long]);   ncl += rs
            case _: String => val rs = VectorS (for j <- cl yield j.asInstanceOf [String]); ncl += rs
            end match
        end for
        new Relation (name + "_u_" + ucount (), colName, ncl.toVector, -1, domain)
    end union

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Intersect this relation and r2.  Check that the two relations are compatible.
     *  Slower and only to be used if there is no index.
     *  @param _r2  the other relation
     */
    def intersect (_r2: Table): Relation =
        val r2 = _r2.asInstanceOf [Relation]
        if hasIndex && r2.hasIndex then return intersect2 (r2)
        if incompatible (r2) then return null

        val newCol = Vector.fill [Vect] (colName.length)(null)
        val r3 = new Relation (name + "_u_" + ucount (), colName, newCol.toVector, -1, domain)
        for i <- 0 until rows if r2 contains row(i) do r3.add (row(i))
        r3.materialize ()
    end intersect

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Intersect this relation and r2.  Check that the two relations are compatible.
     *  Use index to finish intersect operation.
     *  @param _r2  the other relation
     */
    def intersect2 (_r2: Table): Relation =
        val r2 = _r2.asInstanceOf [Relation]
        if incompatible (r2) then return null

        val newCol = Vector.fill [Vect] (colName.length) (null)
        val r3     = new Relation (name + "_u_" + ucount (), colName, newCol, -1, domain)

        for i <- orderedIndex.indices do
            if r2.keytoIndex isDefinedAt orderedIndex(i) then
                if row(i) sameElements r2.row(r2.keytoIndex (orderedIndex(i))) then r3.add_ni (row(i))
            end if
        end for
        r3.materialize ()
    end intersect2

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Take the difference of this relation and r2 (this - r2).  Check that
     *  the two relations are compatible.
     *  @param _r2  the other relation
     */
    def minus (_r2: Table): Relation =
        val r2 = _r2.asInstanceOf [Relation]
        if incompatible (r2) then return null

        val newCol = Vector.fill [Vect] (colName.length)(null)
        val r3 = new Relation (name + "_m_" + ucount (), colName, newCol, key, domain)
        for i <- 0 until rows if ! (r2 contains row(i)) do r3.add (row(i))
        r3.materialize ()
    end  minus

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Take the difference of this relation and r2 (this - r2).  Check that
     *  the two relations are compatible.  Indexed based minus.
     *  @param r2  the other relation
     */
    def minus2 (r2: Relation): Relation =
        if incompatible (r2) then return null

        val newCol = Vector.fill [Vect] (colName.length)(null)
        val r3 = new Relation (name + "_m_" + ucount (), colName, newCol, key, domain)
        for i <- orderedIndex.indices do
            if r2.keytoIndex isDefinedAt orderedIndex(i) then
                if ! (row(i) sameElements r2.row(r2.keytoIndex (orderedIndex(i)))) then r3.add_ni (row(i))
            else
                r3.add_ni (row(i))
            end if
        end for
        r3.materialize ()
    end minus2

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether any rows/tuples exist in this relation.
     */
    def exists: Boolean = rows > 0

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether this relation and r2 are incompatible by having
     *  differing numbers of columns or differing domain strings.
     *  @param r2  the other relation/table
     */
    def incompatible (r2: Table): Boolean =
         if cols != r2.cols then
             flaw ("incompatible", s"$name and r2 have differing number of columns")
             true
         else if domains != r2.domains then
             flaw ("incompatible", s"$name and r2 have differing domain strings")
             true
         else
             false
         end if
    end incompatible

    // ================================================================= PRODUCT

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the Cartesian product of this relation and r2 (this × r2).
     *  @param r2  the second relation
     */
    def product (r2: Table): Relation =
        val ncols     = cols + r2.cols
        val newCName  = disambiguate (colName, r2.colNames)
        val newCol    = Vector.fill [Vect] (ncols) (null)
        val newKey    = key                                                      // FIX
        val newDomain = domain + r2.domains
        val r3 = new Relation (name + "_j_" + ucount (), newCName, newCol, newKey, newDomain)

        for i <- 0 until rows do
            val t = row(i)
            for j <- 0 until r2.rows do r3.add (t ++ r2.row(j))
        end for
        r3.materialize ()
    end product

    // ==================================================================== JOIN

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Join this relation and r2 by performing an "equi-join".  Rows from both
     *  relations are compared requiring cName1 values to equal cName2 values.
     *  Disambiguate column names by appending "2" to the end of any duplicate column name.
     *  @param cName1  the join column names of this relation (e.g., the Foreign Key)
     *  @param cName2  the join column names of relation r2 (e.g., the Primary Key)
     *  @param r2      the rhs relation in the join operation
     */
    def join (cName1: VEC [String], cName2: VEC [String], r2: Table): Relation =
        val ncols = cols + r2.cols
        val cp1   = cName1.map (colMap (_))                                      // get column positions in this
        val cp2   = cName2.map (r2.colsMap (_))                                  // get column positions in r2
        if cp1.length != cp2.length then flaw ("join", "incompatible sizes on match columns")

        val newCName  = disambiguate (colName, r2.colNames)
        val newCol    = Vector.fill [Vect] (ncols) (null)
        val newKey    = key                                                      // FIX
        val newDomain = domain + r2.domains
        val r3 = new Relation (name + "_j_" + ucount (), newCName, newCol, newKey, newDomain)

        for i <- 0 until rows do
            val t = row(i)
            for j <- 0 until r2.rows do
                val u = r2.row(j)
                if sameOn (t, u, cp1, cp2) then r3.add (t ++ u)
            end for
        end for
        r3.materialize ()
    end join

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Join this relation and r2 by performing an "equi-join", use index to join
     *  @param cName1  the join column names of this relation (e.g., the Foreign Key)
     *  @param cName2  the join column names of relation r2 (e.g., the Primary Key)
     *  @param r2      the rhs relation in the join operation
     */
    def joinindex (cName1: VEC [String], cName2: VEC [String], r2: Relation): Relation =
        val ncols = cols + r2.cols
        val cp1   = cName1.map (colMap (_))                                      // get column positions in this
        val cp2   = cName2.map (r2.colMap (_))                                   // get column positions in r2
        if cp1.length != cp2.length then flaw ("join", "incompatible sizes on match columns")

        val newCName = disambiguate (colName, r2.colName)
        val newCol   = Vector.fill [Vect] (ncols)(null)
        val newKey   = if r2.key == cp2(0) then key                              // foreign key in this relation
                       else if key == cp1(0) then r2.key                         // foreign key in r2 table
                       else -1                                                   // key not in join and composite keys not allowed

        val newDomain = domain + r2.domains
        val r3 = new Relation (name + "_j_" + ucount (), newCName, newCol, newKey, newDomain)

        if cp1.size == 1 && cp2.size == 1 then
            if key == cp1(0) && r2.key == cp2(0) then
                for k <- orderedIndex do
                    val t = index(k)
                    val u = r2.index.getOrElse (k, null)
                    if u != null then r3.add_ni (t ++ u)
                end for
            else if key == cp1(0) then
                for idx <- r2.orderedIndex do
                    val u = r2.index(idx)
                    val t = index.getOrElse ((u(cp2(0))), null)
                    if t != null then r3.add_ni (t ++ u)
                    r3.add_ni(t ++ u)
                end for
            else if r2.key == cp2(0) then
                for idx <- orderedIndex do
                    val t = index(idx)
                    val u = r2.index.getOrElse ((t(cp1(0))), null)
                    if u != null then r3.add_ni (t ++ u)
                end for
            end if
        end if
        r3.materialize ()
    end joinindex

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Join this relation and r2 by performing a "natural-join".  Rows from both
     *  relations are compared requiring cName values to be equal.
     *  @param cName  the common join column names for both relation
     *  @param _r2    the rhs relation in the join operation
     */
    def join (cName: VEC [String], _r2: Table): Relation =
        val r2    = _r2.asInstanceOf [Relation]
        val ncols = cols + r2.cols - cName.length
        val cp1   = cName.map (colMap (_))                                       // get column positions in this
        val cp2   = cName.map (r2.colMap (_))                                    // get column positions in r2
        var newDomain2 = r2.domain
        for i <- cp1.length - 1 to 0 by -1 do
            val (cp1_i, cp2_i) = (cp1(i), cp2(i))
            if domain(cp1_i) != r2.domain(cp2_i) then flaw ("join", s"column types do not match: $cp1, $cp2")
            newDomain2 = removeAt (newDomain2, cp2_i)
        end for
        val cp3= r2.colName.map (r2.colMap (_)) diff cp2                         // r2 specific columns

        val newCName  = uniq_union (colName, r2.colName)
        val newCol    = Vector.fill [Vect] (ncols) (null)
        val newKey    = key                                                      // FIX
        val newDomain = domain + newDomain2
        val r3 = new Relation (name + "_j_" + ucount (), newCName, newCol, newKey, newDomain)

        for i <- 0 until rows do
            val t = row(i)
            for j <- 0 until r2.rows do
                val u = r2.row(j)
                if sameOn (t, u, cp1, cp2) then { val u3 = Table.project (u, cp3); r3.add (t ++ u3) }
            end for
        end for
        r3.materialize ()
    end join

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The theta join, handle the predicates in where are connect by "and" (where a....and b....).
     *  @param _r2  the second relation
     *  @param p0   the first theta join predicate (r1 cName, r2 cName, predicate to compare these two column)
     *  @param p    the rest of theta join predicates (r1 cName, r2 cName, predicates to compare these two column)
     */
    def join (_r2: Table, p0: Predicate2, p: Predicate2*): Relation =
        val r2    = _r2.asInstanceOf [Relation]
        val ncols     = cols + r2.cols
        val newCName  = disambiguate (colName, r2.colName)
        val newCol    = Vector.fill [Vect] (ncols) (null)
        val newKey    = key                                                      // FIX
        val newDomain = domain + r2.domain
        val r3        = new Relation (name + "_j_" + ucount (), newCName, newCol, newKey, newDomain, null)

        var resultlist = IndexedSeq [(Int, Int)] ()
        for i <- 0 to p.size do
            var result = IndexedSeq [(Int, Int)] ()
            val p_i = if i == 0 then p0 else p(i-1)
            val cp1 = colMap (p_i._1)
            val cp2 = r2.colMap (p_i._2)
            if domain.charAt (cp1) != r2.domain.charAt (cp2) then flaw ("join", "differing domain strings")

            val psingle = p_i._3                                                  // single predicate
            result = null                                                         // FIX the next line & remove this line
            //result = col(cp1).filterPos2 (r2.col (cp2), psingle)

            debug ("join", s"after predicate $i: result = $result")
            resultlist = if i == 0 then result else resultlist intersect result
        end for

        val smallmapbig = resultlist.groupBy (_._1)
        for i <- smallmapbig.keySet.toVector.sorted do
            val t = if key < 0 then index(i) else index(indextoKey(i))
            val bigindexs = smallmapbig (i).map (x => x._2)
            for j <- bigindexs do
                val u = if r2.key < 0 then  r2.index(j) else r2.index(r2.indextoKey(j))
                r3.add (t ++ u)
            end for
        end for
        r3.materialize ()
    end join

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Join this relation and r2 by performing an "left-join".  Rows from both
     *  relations are compared requiring cName1 values to equal cName2 values.
     *  Disambiguate column names by appending "2" to the end of any duplicate column name.
     *  All rows from the left table are maintained with missing values indicators used
     *  where needed.
     *  @param cName1  the join column names of this relation (e.g., the Foreign Key)
     *  @param cName2  the join column names of relation r2 (e.g., the Primary Key)
     *  @param _r2      the rhs relation in the join operation
     */
    def leftJoin (cName1: String, cName2: String, _r2: Table): Relation =
        val r2 = _r2.asInstanceOf [Relation]
        leftJoin (colMap (cName1), colMap (cName2), r2)
    end leftJoin

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Join this relation and r2 by performing a "left join".  Rows from both
     *  relations are compared requiring cp1 values to equal cp2 values.
     *  This method returns all the rows from this relation, and the matched rows
     *  from relation r2.  It adds a null tuples for the unmatched rows of relation r2
     *  FIX: It requires relations this and r2 to be sorted on column cp1 and cp2 resp., as it uses Sort-Merge join
     *  @param cp1  the position of the join column of this relation
     *  @param cp2  the position of the join column of r2 relation
     *  @param r2   the rhs relation in the join operation
     */
    def leftJoin (cp1: Int, cp2: Int, r2: Relation): Relation =
        val r3 = Relation (name + "_leftJoin_" + r2.name, key, domain + r2.domain, colName ++ r2.colName)
        val absentTuple = nullTuple (r2.domain)
        var j = 0
        for i <- 0 until rows do
            val t = row(i)
            val t_cp1 = t(cp1)
            while j < r2.rows-1 && r2.col(cp2)(j) < t_cp1 do j += 1
            val j_aux = j
            if t_cp1 == r2.row(j)(cp2) then
                while j < r2.rows && r2.col(cp2)(j) == t_cp1 do
                    val u = r2.row(j)
                    r3.add_ni (t ++ u)
                    j += 1
                end while
                j = j_aux
            else
                r3.add_ni (t ++ absentTuple)
            end if
        end for
        r3.materialize ()
    end leftJoin

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Join this relation and r2 by performing a "right join".  Rows from both
     *  relations are compared requiring cp1 values to equal cp2 values.
     *  This method returns all the rows from this relation, and the matched rows
     *  from relation r2.  It adds a null tuples for the unmatched rows of relation r2
     *  @param cp1  the position of the join column of this relation
     *  @param cp2  the position of the join column of r2 relation
     *  @param r2   the rhs relation in the join operation
     */
    def rightJoin (cp1: Int, cp2: Int, r2: Relation): Relation =
        r2.leftJoin (cp2, cp1, this)
    end rightJoin

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Join this relation and r2 by performing an "approximate left-join".  Rows from both
     *  relations are compared requiring cName1 values to apprximately equal cName2 values.
     *  Disambiguate column names by appending "2" to the end of any duplicate column name.
     *  All rows from the left table are maintained with missing values indicators used
     *  where needed.
     *  @param thres   the approximate equality threshold
     *  @param cName1  the join column names of this relation (e.g., the Foreign Key)
     *  @param cName2  the join column names of relation r2 (e.g., the Primary Key)
     *  @param _r2     the rhs relation in the join operation
     */
    def leftJoinApx (thres: Double = 0.001) (cName1: String, cName2: String, _r2: Table): Relation =
        val r2 = _r2.asInstanceOf [Relation]
//      setThreshold (thres)
        leftJoinApx (colMap (cName1), colMap (cName2), r2)
    end leftJoinApx

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Join this relation and r2 by performing a "left join".  Rows from both
     *  relations are compared requiring cp1 values to approximately equal cp2 values.
     *  This method returns all the rows from this relation, and the matched rows
     *  from relation r2.  It adds a null tuples for the unmatched rows of relation r2
     *  FIX: It requires relations this and r2 to be sorted on column cp1 and cp2 resp.,
     *  as it uses Sort-Merge join
     *  @param cp1  the position of the join column of this relation
     *  @param cp2  the position of the join column of r2 relation
     *  @param r2   the rhs relation in the join operation
     */
    def leftJoinApx (cp1: Int, cp2: Int, r2: Relation): Relation =
        val r3 = Relation (name + "_leftJoinApx_" + r2.name, 1, domain + r2.domain, colName ++ r2.colName)
        val absentTuple = nullTuple (r2.domain)
        var j = 0
/*******
        for i <- 0 until rows do
            val t = row(i)
            val t_cp1 = t(cp1)
            while j < r2.rows-1 && !=~ (Vec (r2.col(cp2), j), t_cp1) && r2.col(cp2)(j) < t_cp1 do j += 1
            val j_aux = j
            if =~ (t_cp1, r2.row(j)(cp2)) then
                while j < r2.rows && =~ (Vec (r2.col(cp2), j), t_cp1) do
                    val u = r2.row(j)
                    r3.add_ni (t ++ u)
                    j += 1
                end while
                j = j_aux
            else
                r3.add_ni (t ++ absentTuple)
            end if
        end for
*******/
        r3.materialize ()
    end leftJoinApx

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Join this relation and r2 by performing a "right join".  Rows from both
     *  relations are compared requiring cp1 values to approximately equal cp2 values.
     *  This method returns all the rows from this relation, and the matched rows
     *  from relation r2.  It adds a null tuples for the unmatched rows of relation r2
     *  @param cp1  the position of the join column of this relation
     *  @param cp2  the position of the join column of r2 relation
     *  @param r2   the rhs relation in the join operation
     *
    def rightJoinApx (cp1: Int, cp2: Int, r2: Relation): Relation =
        r2.leftJoinApx (cp2, cp1, this)
    end rightJoinApx
     */

    // ================================================================ GROUP BY

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Group this relation by the specified column name, returning this relation.
     *  Each value in column cName will be mapped to a vector of row numbers containing
     *  the value, e.g., *  { (a, A), (b, C), (a, T) } makes map a -> (0, 2), b -> (1).
     *  @param cName  the group column name
     */
    def groupBy (cName: String): Relation =
        if ! (colName contains cName) then
            flaw ("groupBy", s"cName = $cName is not contained in colName")
        end if

        val _col = col(colMap (cName))                                           // the cName column
        for i <- indices do
            val key = _col(i).asInstanceOf [ValueType]
            val loc = groupMap.getOrElseUpdate (key, VEC [Int] ())
            loc += i                                                             // add index/row num i
        end for
        this
    end groupBy

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Group this relation by the specified column names, returning this relation.
     *  @param cName  the group column names
     *
    def groupBy (cName: String*): Relation =
        if ! cName.map (c => colName contains(c)).reduceLeft (_ && _) then
            flaw ("groupBy", "groupbyName used to groupby doesn't exist in the cName")
        end if
        val equivCol = Vector.fill [Vect] (colName.length)(null)
        if rows == 0 then return this

        val cPos = cName.map (colMap (_))

        //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /*  Sort on the given columns.
         *  @param sortColumn  the set of columns to sort on
         */
        def sortcol (sortColumn: Set [ValueType]): Vect =
            println (s"sortCol: sortColumn = $sortColumn")
            var colcol: Vect = null
            val domain = null
//          for x <- sortColumn do colcol = Vec.:+ (colcol, x, domain, 0)
            for x <- sortColumn do colcol = Vec.:+ (colcol, x)

            val sortcol = colcol; sortcol.sort (); sortcol
        end sortcol

        var groupIndexMap = Map [ValueType, Vector [ValueType]] ()
        val tempIndexMap  = Map [ValueType, Vector [ValueType]] ()
        var sortlst: Vect = null

        for i <- cPos.indices do
            if i == 0 then
                index.foreach (indexmap => {
                    val key   = indexmap._2(cPos(i)).toString
                    val value = indexmap._1
                    if groupIndexMap contains key then groupIndexMap += key -> (groupIndexMap(key) :+ value)
                    else groupIndexMap += key -> Vector(value)
                }) // foreach
            else
                tempIndexMap.clear ()
                groupIndexMap.foreach (groupindexmap => {
                    val tempidxlist = groupindexmap._2
                    for idx <- tempidxlist do
                        val key   = groupindexmap._1.toString + "," + index(idx)(cPos(i))
                        val value = idx
                        if tempIndexMap contains(key) then tempIndexMap += key -> (tempIndexMap(key) :+ value)
                        else tempIndexMap += key -> Vector(value)
                    end for
                }) // for each
                groupIndexMap = tempIndexMap
            end if

            if i == cPos.size - 1 then
                orderedIndex = Vector ()
                grouplist    = Vector [Int] ()
                sortlst      = sortcol (groupIndexMap.keySet.toSet)
                for k <- 0 until sortlst.size do
                    val indexes  = groupIndexMap(Vec(sortlst, k))
                    orderedIndex = orderedIndex ++ indexes
                    grouplist    = grouplist :+ orderedIndex.length
                end for
            end if
        end for
        this
    end groupBy
     */

    // ================================================================= ORDER BY

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Order (ascending) the rows in the relation by the selected columns _cName.
     *  A stable sorting is used to allow sorting on multiple columns.
     *  @param _cName  the column names that are to be sorted
     */
    def orderBy (_cName: String*): Relation =
        val cName = _cName.distinct
        if ! cName.map (c => colName contains (c)).reduceLeft (_ && _) then
            flaw ("orderBy", "cName used to orderBy does not exist in relation")
        end if

        val newCol = Vector.fill [Vect] (cols)(null)
        val r2 = new Relation (name + "_j_" + ucount (), colName, newCol, key, domain)

        val perm = orderByHelper (VEC (cName.map (colMap (_)) :_*), rows)
        for i <- perm do r2.add (row(i))
        r2.materialize ()
    end orderBy

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Order (descending) the rows in the relation by the selected columns _cName.
     *  A stable sorting is used to allow sorting on multiple columns.
     *  @param _cName  the column names that are to be sorted
     */
    def reverseOrderBy (_cName: String*): Relation =
        val cName = _cName.distinct
        if ! cName.map (c => colName contains (c)).reduceLeft (_ && _) then
            flaw ("reverseOrderBy", "cName used to orderBy does not exist in relation")
        end if

        val newCol = Vector.fill [Vect] (cols) (null)
        val r2 = new Relation (name + "_j_" + ucount (), colName, newCol, key, domain)

        val perm = orderByHelper (VEC (cName.map (colMap (_)) :_*), rows)
        for i <- perm.reverse do r2.add (row(i))
        r2.materialize ()
    end reverseOrderBy

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Helper method for orderBy and reverseOrderBy.  Performs indirect merge sort.
     *  @param cPos  sequence of column positions to sort
     *  @param n     total number of rows in this Relation
     */
    private def orderByHelper (cPos: VEC [Int], n: Int = rows): Array [Int] =
        var perm: Array [Int] = null

        for i <- cPos.indices do
            val col_i = col (cPos(i)).toArray
/* FIX - add MergeSortIndirect to scalation pcakage
            perm = if i == 0 then (new MergeSortIndirect (col_i)()).isort ()
                   else           (new MergeSortIndirect (col_i)(perm)).isort ()
*/
        end for
        perm
    end orderByHelper

    // ================================================================= UPDATES 

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add a tuple to this relation as a new row using col2 as a temp col to improve
     *  performance.  After a batch of adds, call materialize.
     *  @param tuple  an aggregation of columns values (new row)
     */
    @throws (classOf [Exception])
    def add (tuple: Row): Unit =
        try
            if tuple == null then throw new Exception ("Relation.add method: tuple is null")
            val rowIdx   = col2(0).length
            val newkey   = if key < 0 then rowIdx else tuple(key)
            index       += newkey -> tuple
            keytoIndex  += newkey -> rowIdx
            orderedIndex = orderedIndex :+ newkey
            indextoKey  += rowIdx -> newkey
            for j <- tuple.indices do addElem (j, rowIdx, tuple(j))
        catch
            case ex: NullPointerException =>
                println ("tuple'size is: " + tuple.size)
                println ("col'size is:   " + col.size)
                throw ex
        end try
    end add

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add a tuple to this relation as a new row, materialize and return updated table.
     *  May call for last tuple in a batch of tuples to add
     *  @param tuple  an aggregation of columns values (new row)
     */
    def addm (tuple: Row): Relation =
        add (tuple)
        materialize ()
//      println ("addm: updated relation"); show ()
        this
    end addm

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add an element into col2, the holding area for input.  If the types
     *  of column domains are specified, the types are checked.
     *  @param j       the j-th column of col2
     *  @param rowIdx  the row index
     *  @param elem    the element to added
     */
    private def addElem (j: Int, rowIdx: Int, elem: ValueType): Unit =
        val typ = if domain == null then 'X' else domain(j)
        try
            //col2(j)(rowIdx) = elem
            col2(j) += elem
        catch
            case ex: ClassCastException =>
                if typ == 'S' then
//                  println (s"warning in addElem: colIdx j = $j, rowIdx = $rowIdx, elem = $elem, class = ${elem.getClass}, typ = $typ")
                    //col2(j)(rowIdx) = elem.toString                           // anything can be a string
                    col2(j) += elem.toString
                else if elem.isInstanceOf [String] || elem.isInstanceOf [Char] then
                    println (s"warning in addElem: colIdx j = $j, rowIdx = $rowIdx, elem = $elem, class = ${elem.getClass}, typ = $typ")
                else
                    println (s"exception in addElem: name = $name, colIdx j = $j, rowIdx = $rowIdx, elem = $elem, class = ${elem.getClass}, typ = $typ")
                    throw ex
                end if
        end try
    end addElem

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add a tuple into the col2, without maintaining the index (No Index (ni),
     *  orderedIndex, keytoIndex and indextoKey.
     *  @param tuple  the tuple to add
     */
    private def add_ni (tuple: Row): Unit =
        val rowIdx = col2(0).length
        for j <- tuple.indices do addElem (j, rowIdx, tuple(j))
    end add_ni

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Materialize the relation by copying the temporary col2 into col.
     *  It needs to be called by the end of the relation construction.
     */
    def materialize (): Relation =
        if domain == null || domain == "" then materialize1 () else materialize2 ()
    end materialize

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Materialize the relation by copying the temporary col2 into col.
     *  It needs to be called by the end of the relation construction.
     *  This version uses the type/domain of the first value to transform the col2 to col.
     */
    private [relation] def materialize1 (): Relation =

        //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /*  Transform the j-th column of col2 to the appropriate vector type.
         *  @param j  the j-th column index in the relation
         */
        def transform1 (j: Int): Vect =
            val first       = col2(j)(0)
            var col_j: Vect = null

            try
                col_j = first match
                case _: Double  => VectorD (col2(j).asInstanceOf [VEC [Double]])
                case _: Int     => VectorI (col2(j).asInstanceOf [VEC [Int]])
                case _: Long    => VectorL (col2(j).asInstanceOf [VEC [Long]])
                case _: String  => VectorS (col2(j).asInstanceOf [VEC [String]])
            catch
                case ex: java.lang.ClassCastException =>
                flaw ("transform1", s"ClassCastException - name = $name, j = $j, first = $first, col2(j) = ${col2(j)}")

            col_j
        end transform1

        //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /*  Transform the j-th column of col2 to the appropriate vector type and append.
         *  @param j  the j-th column index in the relation
         */
        def transform1b (j: Int): Vect =
            val first       = col2(j)(0)
            var col_j: Vect = null

            try
                col_j = first match
                case _: Double => VectorD (col(j).asInstanceOf [VectorD] ++ col2(j).asInstanceOf [VEC [Double]])
                case _: Int    => VectorI (col(j).asInstanceOf [VectorI] ++ col2(j).asInstanceOf [VEC [Int]])
                case _: Long   => VectorL (col(j).asInstanceOf [VectorL] ++ col2(j).asInstanceOf [VEC [Long]])
                case _: String => VectorS (col(j).asInstanceOf [VectorS] ++ col2(j).asInstanceOf [VEC [String]])
            catch
                case ex: java.lang.ClassCastException =>
                flaw ("transform1b", s"ClassCastException - name = $name, j = $j, first = $first, col2(j) = ${col2(j)}")

            col_j
        end transform1b

        debug ("materialize1", s"col2 = $col2")
        if col2(0).size == 0 then
            flaw ("materialize1", "no rows in col2 to materialize")
        else if colEmpty then
            col = (for j <- col2.indices yield transform1(j)).toVector
            for j <-col2.indices do col2(j).clear ()
        else
            col = (for j <- col.indices yield transform1b(j)).toVector
            for j <-col2.indices do col2(j).clear ()
        end if
        this
    end materialize1

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Materialize the relation by copying the temporary col2 into col.
     *  It needs to be called by the end of the relation construction.
     *  This version uses domain to transform the col2 to col according to the domain indicator.
     */
    private [relation] def materialize2 (): Relation =

        //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /*  Transform the j-th column of col2 to the appropriate vector type.
         *  @param j  the j-th column index in the relation
         */
        def transform2 (j: Int): Vect =
            val domain_j    = domain(j)
            var col_j: Vect = null

            try
                col_j = domain_j match
                case 'D' => VectorD (col2(j).asInstanceOf [VEC [Double]])
                case 'I' => VectorI (col2(j).asInstanceOf [VEC [Int]])
                case 'L' => VectorL (col2(j).asInstanceOf [VEC [Long]])
                case 'S' => VectorS (col2(j).asInstanceOf [VEC [String]])
                case 'X' => VectorS (col2(j).asInstanceOf [VEC [String]])
                case  _  => flaw ("materialize2.transform2", s"($j) vector type not supported domain ($domain_j)"); null
            catch
                case ex: java.lang.ClassCastException =>
                flaw ("transform2", s"ClassCastException - name = $name, j = $j, domain_j = $domain_j, col2(j) = ${col2(j)}")

            col_j
        end transform2

        //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /*  Transform the j-th column of col2 to the appropriate vector type and append.
         *  @param j  the j-th column index in the relation
         */
        def transform2b (j: Int): Vect =
            val domain_j    = domain(j)
            var col_j: Vect = null

            try
            col_j = domain_j match
            case 'D' => col(j).asInstanceOf [VectorD] ++ col2(j).asInstanceOf [VEC [Double]]
            case 'I' => col(j).asInstanceOf [VectorI] ++ col2(j).asInstanceOf [VEC [Int]]
            case 'L' => col(j).asInstanceOf [VectorL] ++ col2(j).asInstanceOf [VEC [Long]]
            case 'S' => col(j).asInstanceOf [VectorS] ++ col2(j).asInstanceOf [VEC [String]]
            case 'X' => col(j).asInstanceOf [VectorS] ++ col2(j).asInstanceOf [VEC [String]]
            case  _  => flaw ("materialize2.transform2b", s"($j) vector type not supported domain ($domain_j)"); null
            catch
                case ex: java.lang.ClassCastException =>
                flaw ("transform2b", s"ClassCastException - name = $name, j = $j, domain_j = $domain_j, col2(j) = ${col2(j)}")

            col_j
        end transform2b

        debug ("materialize2", s"col2 = $col2")
        if col2(0).size == 0 then
            flaw ("materialize2", "no rows in col2 to materialize")
        else if colEmpty then
            col = (for j <- col2.indices yield transform2(j)).toVector
            for j <-col2.indices do col2(j).clear ()
        else
            col = (for j <- col.indices yield transform2b(j)).toVector
            for j <-col2.indices do col2(j).clear ()
        end if
        this
    end materialize2

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether all of the columns in the relation are empty.
     */
    def colEmpty: Boolean =
        for column <- col if column != null do return false
        true
    end colEmpty

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Update the column named cName using function func for elements with
     *  value matchStr.
     *  @param cName     the name of the column to be updated
     *  @param newVal    the value used to assign updated values
     *  @param matchVal  the value to be matched to elements
     */
    def update (cName: String, newVal: ValueType, matchVal: ValueType): Unit =
        val col_j = col(colMap(cName))
        for i <- col_j.indices if col_j(i) == matchVal do assign (col_j, i, newVal)
    end update

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Assign the value newVal to column/vector c at index position i.
     *  @param c       the name of the column/vector to be assigned
     *  @param i       the index position to be assigned
     *  @param newVal  the value used for assignment
     */
    def assign (col_j: Vect, i: Int, newVal: ValueType): Unit =
        val first = col_j(0)
        first match
        case _: Double  => (col_j.asInstanceOf [VectorD])(i) = newVal.asInstanceOf [Double]
        case _: Int     => (col_j.asInstanceOf [VectorI])(i) = newVal.asInstanceOf [Int]
        case _: Long    => (col_j.asInstanceOf [VectorL])(i) = newVal.asInstanceOf [Long]
        case _: String  => (col_j.asInstanceOf [VectorS])(i) = newVal.asInstanceOf [String]
        case _          => flaw ("assign", s"vector type ($first) not supported")
        end match
    end assign

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Update the column named cName using function func for elements with
     *  value matchStr.
     *  @param cName     the name of the column to be updated
     *  @param func      the function used to assign updated values
     *  @param matchVal  the value to be matched to elements
     */
    def update (cName: String, func: (ValueType) => ValueType, matchVal: ValueType): Unit =
        val col_j = col (colMap(cName))
        for i <- col_j.indices if col_j(i) == matchVal do assign (col_j, i, func (col_j(i)))
    end update

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Update the column named cName using function func for elements where
     *  the predicate pred evaluates to true.
     *  @param cName  the name of the column to be updated
     *  @param func   the function used to assign updated values
     *  @param pred   the predicated used to select elements for update
     */
    def update (cName: String, func: (ValueType) => ValueType, pred: (ValueType) => Boolean): Unit =
        val col_j = col (colMap(cName))
        for i <- col_j.indices if pred (col_j(i)) do assign (col_j, i, func (col_j(i)))
    end update

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Delete the rows from this relation that satisfy the predicates.
     *  @param  p  tuple(1): column name, tuple(2): predicate (T => `Boolean`)
     *  @tparam T  the predicate type
     */
    def delete (p: Predicate*): Relation =
        null
/*
        var pos = VEC [Int] ()
        for i <- p.indices do
            domain (colMap(p(i)._1)) match
            case 'D' => val pos1 = col (colMap(p(i)._1)).asInstanceOf [VectorD].filterPos (p(i)._2.asInstanceOf [Double => Boolean])
                        if i > 0 then pos = pos intersect pos1 else pos ++= pos1
            case 'I' => val pos1 = col (colMap(p(i)._1)).asInstanceOf [VectorI].filterPos (p(i)._2.asInstanceOf [Int => Boolean])
                        if i > 0 then pos = pos intersect pos1 else pos ++= pos1
            case 'L' => val pos1 = col (colMap(p(i)._1)).asInstanceOf [VectorL].filterPos (p(i)._2.asInstanceOf [Long => Boolean])
                        if i > 0 then pos = pos intersect pos1 else pos ++= pos1
            case 'S' => val pos1 = col (colMap(p(i)._1)).asInstanceOf [VectorS].filterPos (p(i)._2.asInstanceOf [String => Boolean])
                        if i > 0 then pos = pos intersect pos1 else pos ++= pos1
            case 'X' => val pos1 = col (colMap(p(i)._1)).asInstanceOf [VectorS].filterPos (p(i)._2.asInstanceOf [String => Boolean])
                        if i > 0 then pos = pos intersect pos1 else pos ++= pos1
            case _   => flaw ("delete", "predicate type not supported")
                        null
            end match
        end for
        val indices = Set (0 to rows-1 :_*) diff pos.toSet
        for i <- 0 until cols do Vec.delete (col(i), pos.asInstanceOf [VEC [Int]])
        selectAt (indices.toArrayBuffer.sorted)
*/
    end delete

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert this relation into a string column by column.
     */
    override def toString: String =
        var sb = new StringBuilder (s"Relation(name = $name, key = $key, domain = $domain,\ncolName = $colName,\n")
        for i <- col.indices do sb.append (s"${col(i)} \n")
        sb.replace (sb.length-1, sb.length, ")").mkString
    end toString

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Show this relation row by row.  Fixed with columns.
     *  @param limit  the limit on the number of rows to display
     */
    def show0 (limit: Int = Int.MaxValue): Unit =
        val wid   = 18                                                          // column width
        val rep   = math.max (wid, wid * colName.length)                        // repetition = width * # columns
        val title = s"| Relation name = $name, key-column = $key "

        println (s"|-${"-"*rep}-|")
        println (title + " "*math.max (0, rep-title.length) + "   |")
        println (s"|-${"-"*rep}-|")
        print ("| "); for cn <- colName do print (s"%${wid}s".format (cn)); println (" |")
        println (s"|-${"-"*rep}-|")
        for i <- 0 until math.min (rows, limit) do
            print ("| ")
            for cv <- row(i) do
                if cv.isInstanceOf [Double] then print (s"%${wid}g".format (cv))
                else print (s"%${wid}s".format (cv))
            end for
            println (" |")
        end for
        println (s"|-${"-"*rep}-|")
    end show0

    private def lineLen (domain: String, clen: Int): Int =
        math.max (clen, clen * colName.length) + (clen - 1) * domain.count (_ == 'X')
    end lineLen

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Show this relation row by row.  Handles extendes width strings.
     *  @param limit  the limit on the number of rows to display
     */
    def show (limit: Int = Int.MaxValue): Unit =
        val wid   = 15                                                          // column gap
        val wid1  = wid + 1                                                     // column width
        val rep   = lineLen (domain, wid1)                                      // repetition = line length
        val title = s"| Relation name = $name, key-column = $key "

        println (s"|-${"-"*rep}-|")
        println (title + " "*math.max (0, rep-title.length) + "   |")
        println (s"|-${"-"*rep}-|")

        print ("| ")
        for j <- 0 until colName.length do                                      // for each column
            val cn = colName(j)                                                 // column name
            if domain(j) == 'X' then
                print (s"%${2*wid+1}s".format (cn))
            else
                print (s"%${wid1}s".format (cn))
        end for
        println (" |")
        println (s"|-${"-"*rep}-|")

        for i <- 0 until math.min (rows, limit) do                              // for each row
            print ("| ")
            for j <- 0 until colName.length do                                  // for each column
                val cv = col(j)(i)                                              // column value
                if domain(j) == 'X' then
                    print (s" %${2*wid}s".format (cv))
                else if domain(j) == 'D' then
                    print (s" %${wid}g".format (cv))
                else
                    print (s" %${wid}s".format (cv))
            end for
            println (" |")
        end for
        println (s"|-${"-"*rep}-|")
    end show

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Show this relation's foreign keys.
     */
    def showFkey (): Unit =
        val wid    = 18                                                         // column width
        val rep    = wid * colName.length                                       // repetition = width * # columns
        val title  = s"| Relation name = $name, foreign keys = "
        val fkline = s"| $fKeys "

        println (s"|-${"-"*rep}-|")
        println (title + " "*(rep-title.length) + "   |")
        println (s"|-${"-"*rep}-|")
        println (fkline + " "*(rep-fkline.length) + "   |")
        println (s"|-${"-"*rep}-|")
    end showFkey

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert this relation into a matrix of doubles, e.g., in the regression
     *  equation: xb = y create matrix xy.
     *  @param colPos  the column positions to use for the matrix
     */
    def toMatrixD (colPos: VEC [Int]): MatrixD =
        val colVec = for x <- project (colPos).col yield x.toDouble
        MatrixD (colVec).transpose
    end toMatrixD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert this relation into a matrix of doubles and a vector of doubles,
     *  e.g., in the regression equation: xb = y create matrix x and vector y.
     *  @param colPos   the column positions to use for the matrix
     *  @param colPosV  the column position to use for the vector
     */
    def toMatrixDD (colPos: VEC [Int], colPosV: Int): (MatrixD, VectorD) =
        val colVec = for x <- project (colPos).col yield x.toDouble
        (MatrixD (colVec).transpose, col(colPosV).toDouble)
    end toMatrixDD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert this relation into a matrix of doubles, e.g., in the regression
     *  equation: xb = y create matrix xy.  It will convert strings to doubles.
     *  @param colPos  the column positions to use for the matrix
     */
    def toMatrixD2 (colPos: VEC [Int] = null): MatrixD =
        val cp = if colPos == null then VEC.range (0, cols) else colPos
        val colVec =
        for x <- project (cp).col yield {
            try x.toDouble
            catch case num: NumberFormatException => map2Int (x.asInstanceOf [VectorS])._1.toDouble
        } // for
        MatrixD (colVec).transpose
    end toMatrixD2

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert the colPos column of this relation into a vector of doubles.
     *  @param colPos  the column position to use for the vector
     */
    def toVectorD (colPos: Int = 0): VectorD = col(colPos).toDouble
    def toVectorI (colPos: Int = 0): VectorI = col(colPos).toInt
    def toVectorL (colPos: Int = 0): VectorL = col(colPos).toLong
    def toVectorS (colPos: Int = 0): VectorS = col(colPos).toString2

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert the colName column of this relation into a vector of doubles.
     *  @param colName  the column name to use for the vector
     */
    def toVectorD (colName: String): VectorD = col(colMap(colName)).toDouble
    def toVectorI (colName: String): VectorI = col(colMap(colName)).toInt
    def toVectorL (colName: String): VectorL = col(colMap(colName)).toLong
    def toVectorS (colName: String): VectorS = col(colMap(colName)).toString2

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert the given columns within this relation to a map: keyColPos -> valColPos.
     *  @param keyColPos  the key column positions
     *  @param valColPos  the value column positions
     */
    def toMap (keyColPos: VEC [Int], valColPos: Int): Map [VEC [ValueType], ValueType] =
        val map = Map [VEC [ValueType], ValueType] ()
        for i <- indices do
            val tuple = row(i)
            map += keyColPos.map (tuple(_)) -> tuple(valColPos)
        end for
        map
    end toMap

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert the given columns within this relation to a map: keyColName -> valColName.
     *  @param keyColName  the key column names
     *  @param valColname  the value column names
     */
    def toMap (keyColName: VEC [String], valColName: String): Map [VEC [ValueType], ValueType] =
        toMap (keyColName.map (colMap(_)), colMap(valColName))
    end toMap

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Save this relation in a file using serialization.
     */
    def save (): Unit =
        val oos = new ObjectOutputStream (new FileOutputStream (STORE_DIR + name + SER))
        oos.writeObject (this)
        oos.close ()
    end save

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Write this relation into a CSV file with each row written to a line.
     *  @param fileName  the file name of the data file
     */
    def writeCSV (fileName: String): Unit =
        val out = new PrintWriter (BASE_DIR + fileName)
        out.println (colName.toString.drop (5).dropRight (1))
        for i <- 0 until rows do out.println (row(i).toString.drop (7).dropRight (1))
        out.close
    end writeCSV

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Write this relation into a JSON file.
     *  @param fileName  the file name of the data file
     */
    def writeJSON (fileName: String): Unit =
        val out = new PrintWriter (BASE_DIR + fileName)

        out.println ("[")
        for i <- 0 until rows do
            out.println ("{")
            val row_i = row(i)
            for j <- 0 until cols do
                out.print ("\"" + colNames(j) + "\"" + ":")
                out.print (row_i(j))
                if j != cols-1 then out.print (",")
                out.println ()
            end for
            out.println ("},")
        end for
        out.println ("]")
        out.close ()
    end writeJSON

    // ============================================ BUILT-IN AGGREGATE FUNCTIONS

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the mean of the values in column cName.
     *  @param cName  the column name
     */
    def avg (cName: String): Double  = col(colMap(cName)).toDouble.mean
    def mean (cName: String): Double = col(colMap(cName)).toDouble.mean

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the number of values in column cName.
     *  @param cName  the column name
     */
    def count (cName: String): Int = rows

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the maximum value in column cName.
     *  @param cName  the column name
     */
    def max (cName: String): ValueType =
        val cl = col(colMap(cName))
        cl(0) match
        case _: Double => cl.asInstanceOf [VectorD].max
        case _: Int    => cl.asInstanceOf [VectorI].max
        case _: Long   => cl.asInstanceOf [VectorL].max
        case _: String => cl.asInstanceOf [VectorS].max
        end match
    end max

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the minimum value in column cName.
     *  @param cName  the column name
     */
    def min (cName: String): ValueType =
        val cl = col(colMap(cName))
        cl(0) match
        case _: Double => cl.asInstanceOf [VectorD].min
        case _: Int    => cl.asInstanceOf [VectorI].min
        case _: Long   => cl.asInstanceOf [VectorL].min
        case _: String => cl.asInstanceOf [VectorS].min
        end match
    end min

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the sum of the values in column cName.
     *  @param cName  the column name
     */
    def sum (cName: String): ValueType =
        val cl = col(colMap(cName))
        cl(0) match
        case _: Double => cl.asInstanceOf [VectorD].sum
        case _: Int    => cl.asInstanceOf [VectorI].sum
        case _: Long   => cl.asInstanceOf [VectorL].sum
        case _: String => flaw ("sum", s"String not supported"); null
        end match
    end sum

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the variance of the values in column cName.
     *  @param cName  the column name
     */
    def variance (cName: String): Double = col(colMap(cName)).toDouble.variance

/** As seen from class Relation, the missing signatures are as follows.
 *  For convenience, these are usable as stub implementations.
 */
//    def leftJoin (thres: Double) (cName1: String, cName2: String, r2: Table): Table = ???

end Relation


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `relationTest` main function tests the operations provided by `Relation`.
 *  on the `Ex_Days` schema.
 *  > runMain scalation.database.relation.relationTest
 */
@main def relationTest (): Unit =

    import Ex_Days._

    weekdays.generateIndex ()
    weekend.generateIndex ()

    banner ("weekdays"); weekdays.show ()
    banner ("weekend");  weekend.show ()

    println (">>>>> project")
    test ("weekdays.project (\"day\")",  weekdays.project ("day"))
    test ("weekdays.project (\"time\")", weekdays.project ("time"))

    println (">>>>> selproject")
    test ("weekdays.selproject (\"day\", _ == \"Mon\")", weekdays.selproject ("day", _ == "Mon"))

    println (">>>>> select")
    test ("weekdays.select (\"day\",  _ == \"Mon\")", weekdays.select ("day",  _ == "Mon"))
    test ("weekdays.select (\"day\",  _ > \"Mon\")",  weekdays.select ("day",  _ > "Mon"))
    test ("weekdays.select (\"day\",  _ < \"Wed\")",  weekdays.select ("day",  _ < "Wed"))
    test ("weekdays.select (\"time\", _ == 5.00)",    weekdays.select ("time", _ == 5.00))
    test ("weekdays.select (\"time\", _ > 5.00)",     weekdays.select ("time", _ > 5.00))

    test ("weekdays.select (\"day\", _ > \"Mon\").select (\"time\", _ > 7.00)",
           weekdays.select ("day", _ > "Mon").select ("time", _ > 7.00))

    println (">>>>> union")
    val week = weekdays union weekend
    test ("weekdays union weekend", week)

    println (">>>>> intersect")
    test ("week intersect weekend", week intersect weekend)

    println (">>>>> addm")
    test ("weekend addm (\"Zday\", 1.00)", weekend addm Vector ("Zday", 1.00))

    println (">>>>> minus")
    test ("week minus weekend", week minus weekend)

    val cross = week product weekend
    println (">>>>> product")
    test ("week product weekend", cross)
    count (cross, "day").show ()

    println (">>>>> join")
    test ("week.join (\"day\", \"day\" weekend)", week.join ("day", "day", weekend))
    test ("week join weekend", week join weekend)

//  week.writeCSV ("relation" + ⁄ + "week.csv")

end relationTest


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `relationTest2` main function tests the operations provided by `Relation`.
 *  The relational algebra operators are given using Unicode.
 *  @see en.wikipedia.org/wiki/List_of_Unicode_characters
 *  > runMain scalation.database.relation.relationTest2
 */
@main def relationTest2 (): Unit =

    import Ex_Days._

    weekdays.generateIndex ()
    weekend.generateIndex ()

    banner ("weekdays"); weekdays.show ()
    banner ("weekend");  weekend.show ()

    println (">>>>> project")
    test ("weekdays.π (\"day\")",  weekdays.π ("day"))
    test ("weekdays.π (\"time\")", weekdays.π ("time"))

    println (">>>>> selproject")
    test ("weekdays.σπ (\"day\", _ == \"Mon\")", weekdays.selproject ("day", _ == "Mon"))

    println (">>>>> select")
    test ("weekdays.σ (\"day\",  _ == \"Mon\")", weekdays.σ ("day",  _ == "Mon"))
    test ("weekdays.σ (\"day\",  _ > \"Mon\")",  weekdays.σ ("day",  _ > "Mon"))
    test ("weekdays.σ (\"day\",  _ < \"Wed\")",  weekdays.σ ("day",  _ < "Wed"))
    test ("weekdays.σ (\"time\", _ == 5.00)",    weekdays.σ ("time", _ == 5.00))
    test ("weekdays.σ (\"time\", _ > 5.00)",     weekdays.σ ("time", _ > 5.00))

    test ("weekdays.σ (\"day\", _ > \"Mon\").σ (\"time\", _ > 7.00)",
           weekdays.σ ("day", _ > "Mon").σ ("time", _ > 7.00))

    println (">>>>> union")
    val week = weekdays ⋃ weekend
    test ("weekdays ⋃ weekend)", week)

    println (">>>>> intersect")
    test ("week ⋂ weekend", week ⋂ weekend)

    println (">>>>> addm")
    test ("weekend addm (\"Zday\", 1.00)", weekend addm Vector ("Zday", 1.00))

    println (">>>>> minus")
    test ("week - weekend", week - weekend)

    println (">>>>> product")
    test ("week × weekend", week × weekend)

    println (">>>>> join")
    test ("week ⋈ weekend", week ⋈ weekend)

end relationTest2


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `relationTest3` main function tests the operations provided by `Relation`.
 *  It test various aggregate/OLAP operations on a simple data warehouse fact table.
 *  @see www.codeproject.com/Articles/652108/Create-First-Data-WareHouse
 *  FIX - allow entering doubles as "13" rather than "13.0"
 *  > runMain scalation.database.relation.relationTest3
 */
@main def relationTest3 (): Unit =

    import Relation.{max, min}
    import Ex_ProductSales._

    val costVprice = productSales.project ("ProductActualCost", "SalesTotalCost")

    productSales.show ()

    println ("productSales = " + productSales)
    println ("productSales.project (\"ProductActualCost\", \"SalesTotalCost\") = " + costVprice)

    banner ("Test count")
    println ("count (productSales) = " + count (productSales))
    println ("-" * 60)
    println ("count (costVprice)   = " + count (costVprice))

    banner ("Test min")
    println ("min (productSales)   = " + min (productSales))
    println ("-" * 60)
    println ("min (costVprice)     = " + min (costVprice))

    banner ("Test max")
    println ("max (productSales)   = " + max (productSales))
    println ("-" * 60)
    println ("max (costVprice)     = " + max (costVprice))

    banner ("Test sum")
    println ("sum (productSales)   = " + sum (productSales))
    println ("-" * 60)
    println ("sum (costVprice)     = " + sum (costVprice))

    banner ("Test expectation/mean")
    println ("mean (productSales)  = " + mean (productSales))
    println ("-" * 60)
    println ("mean (costVprice)    = " + mean (costVprice))

    banner ("Test variance")
    println ("variance (productSales) = " + variance (productSales))
    println ("-" * 60)
    println ("variance (costVprice)   = " + variance (costVprice))

end relationTest3


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `relationTest4` main function tests conversion `Relation` to a matrix.
 *  > runMain scalation.database.relation.relationTest4
 */
@main def relationTest4 (): Unit =

    import Ex_ProductSales._

    val (mat, vec) = productSales.toMatrixDD (VEC.range (0, 11), 11)

    banner ("productSales")
    productSales.show ()

    banner ("mat and vec")
    println ("mat = " + mat)
    println ("vec = " + vec)

end relationTest4


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `relationTest5` main function tests the interoperability between Relations and Matrices.
 *  > runMain scalation.database.relation.relationTest5
 */
@main def relationTest5 (): Unit =

    val sales_item1 = Relation ("Sales_Item1", VEC ("Date", "FL", "GA", "NC", "SC"),
        VEC [Row] (Vector ("20130101", 10, 5, 5, 4),
                   Vector ("20130102", 20, 30, 40, 25),
                   Vector ("20130103", 8, 6, 9, 9),
                   Vector ("20130104", 6, 7, 9, 10),
                   Vector ("20130105", 4, 7, 9, 10)),
        0,"SIIII")

    val price_item1 = Relation ("Price_Item1", VEC ("Date", "FL", "GA", "NC", "SC"),
        VEC [Row] (Vector ("20130101", 1.6, 1.6, 1.5, 1.3),
                   Vector ("20130102", 1.6, 1.6, 1.5, 1.2),
                   Vector ("20130103", 1.5, 1.6, 1.5, 1.4),
                   Vector ("20130104", 1.4, 1.7, 1.5, 1.4),
                   Vector ("20130105", 1.4, 1.7, 1.4, 1.4)),
        0,"SDDDD")
    val revenue     = Relation ("Revenue", -1, null, "Item", "FL", "GA", "NC", "SC")

    sales_item1.show ()
    price_item1.show ()

    val x = sales_item1.toMatrixD (VEC.range (1, 5))
    val y = price_item1.toMatrixD (VEC.range (1, 5))
    println (s"x = $x")
    println (s"y = $y")
    val z = x *~ y
    println (s"z = $z")
    val colsums = z.sumV
    val row: Row = "Item1" +: colsums.toVector
    println (s"row = $row")
    revenue.add (row)      // FIX
    revenue.materialize ()

    banner ("revenue")
    revenue.show ()

end relationTest5


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `relationTest6` main function tests indexjoin, groupby and eproject
 *  (aggregate operator).
 *  > runMain scalation.database.relation.relationTest6
 */
@main def relationTest6 (): Unit =

    banner ("database")

    val professor = Relation ("professor", 0, "ISS", "pid", "name", "prodeptid")
    TableGen.popTable (professor, 50)  // 10
    professor.generateIndex ()
    professor.show ()

    val course = Relation ("course", 0, "ISS", "cid","crsname", "descr")
    TableGen.popTable (course, 50)   // 20
    course.generateIndex ()
    course.show ()

    val teaching = Relation ("teaching", 0, "IISI", "tid", "cid", "semester", "pid")
    teaching.fKeys = VEC (("cid", "course", 0), ("pid", "professor", 0))
    TableGen.popTable (teaching, 50, VEC (course, professor))
    teaching.generateIndex ()
    teaching.show ()
    teaching.showFkey ()

// FIX - fails when 10, 20 < 50

//  def count1 (r: Table, c: String): Vect = ???

    banner ("joinindex")
    teaching.joinindex (VEC ("pid"), VEC ("pid"), professor).show ()
    banner ("groupBy.eproject")
//  teaching.groupBy ("cid").eproject ((count, "pid_count", "pid"))("tid", "semester").show ()   // FIX
//  teaching.groupBy ("cid").eproject ("count", "cid_count", "cid").show ()
//  teaching.groupBy ("cid").eproject (count1, "cid_count", "cid").show ()

    val a = (count1, "cid_count", "cid").asInstanceOf [AggColumn]
    teaching.groupBy ("cid").eproject (a).show ()

end relationTest6


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `relationTest7` main function tests join method.
 *  > runMain scalation.database.relation.relationTest7
 */
@main def relationTest7 (): Unit =

    val professor = Relation ("professor",
        VEC("pid", "name", "department", "title"),
        VEC [Row] (Vector (1, "jackson", "pharm", 4),
                   Vector (2, "ken", "cs", 2),
                   Vector (3, "pan", "pharm", 0),
                   Vector (4, "yang", "gis", 3),
                   Vector (5, "zhang", "cs", 0),
                   Vector (6, "Yu", "cs", 0)),
        -1, "ISSI")

    val professor2 = Relation ("professor",
        VEC ("pid", "name", "department", "title"),
        VEC [Row] (Vector (7, "LiLy", "gis", 5),
                   Vector (8, "Marry", "gis", 5),
                   Vector (0, "Kate", "cs", 5)),
        0, "ISSI")

    professor.generateIndex ()
    professor2.generateIndex ()

    banner ("professor")
    professor.show ()
    banner ("professor2")
    professor2.show ()

    banner ("join")
    //professor.join (professor2, ("pid", "pid", (x: Int, y: Int) => x < y)).show ()   // FIX
    //professor.join (professor2, ("pid", "pid", _ < _)).show ()                       // FIX

end relationTest7


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `relationTest8` main function tests save method.
 *  > runMain scalation.database.relation.relationTest8
 */
@main def relationTest8 (): Unit =

    val professor = Relation ("professor",
        VEC("pid", "name", "department", "title"),
        VEC [Row] (Vector (1, "jackson", "pharm", 4),
                   Vector (2, "ken", "cs", 2),
                   Vector (3, "pan", "pharm", 0),
                   Vector (4, "yang", "gis", 3),
                   Vector (5, "zhang", "cs", 0),
                   Vector (6, "Yu", "cs", 0)),
        -1, "ISSI")

    val professor2 = Relation ("professor2",
        VEC("pid", "name", "department", "title"),
        VEC [Row] (Vector (1, "jackson", "pharm", 4),
                   Vector (2, "ken", "cs", 2)),
        -1, "ISSI")

    professor.show ()
    professor.save ()
    professor2.show ()
    professor2.save ()

end relationTest8


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `relationTest9` main function tests apply method to load a saved relation.
 *  > runMain scalation.database.relation.relationTest9
 */
@main def relationTest9 (): Unit =

    Relation ("professor").show ()

end relationTest9


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `relationTest10` main function tests the orderBy method.
 *  > runMain scalation.database.relation.relationTest10
 */
@main def relationTest10 (): Unit =

    import Ex_ProductSales._

    productSales.orderBy ("SalesTotalCost", "Deviation").show ()

end relationTest10


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `relationTest11` main function tests the `Relation` on the traffic schema.
 *  > runMain scalation.database.relation.relationTest11
 */
@main def relationTest11 (): Unit =

    val sensor  = Relation ("sensor",
                            VEC ("sensorID", "model", "latitude", "longitude", "on"),
                            VEC [Row] (),
                            0, "ISDDI")
    val road    = Relation ("road",
                            VEC ("roadID", "rdName", "lat1", "long1", "lat2", "long2"),
                            VEC [Row] (),
                            0, "ISDDDD")
    val mroad   = Relation ("road",
                            VEC ("roadID", "rdName", "lanes", "lat1", "long1", "lat2", "long2"),
                            VEC [Row] (),
                            0, "ISIDDDD")
    val traffic = Relation ("traffic",
                            VEC ("time", "sensorID", "count", "speed"),
                            VEC [Row] (),
                            0, "LIID")
    val wsensor = Relation ("sensor",
                            VEC ("sensorID", "model", "latitude", "longitude"),
                            VEC [Row] (),
                            0, "ISDD")
    val weather = Relation ("weather", VEC ("time", "sensorID", "precipitation", "wind"),
                            VEC [Row] (),
                            0, "LIID")

    sensor.show ()
    road.show ()
    mroad.show ()
    traffic.show ()
    wsensor.show ()
    weather.show ()

end relationTest11


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `relationTest12` main function tests the `Relation` class on JSON data.
 *  @see www.learningcontainer.com/sample-json-file
 *  FIX - does not work for Scala 2.13
 *  > runMain scalation.database.relation.relationTest12
 *
@main def relationTest12 (): Unit =

    val fname    = BASE_DIR + "employee.json"
    println (s"fname = $fname")
    val employee = Relation (fname, "employee")

    employee.show ()

end relationTest12
 */

