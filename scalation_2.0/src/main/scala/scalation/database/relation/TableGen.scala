
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Thu Nov 17 20:55:43 EST 2016
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Relational Table Generation
 */

package scalation
package database
package relation

import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks.{break, breakable}

import scalation.mathstat._
import scalation.random._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TableGen` object generates data for `Table` classes (e.g., `Relation`).
 */
object TableGen:

    private val debug = debugf ("TableGen", true)              // debug flag
    private val flaw  = flawf ("TableGen")                     // debug flag

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Populate the columns in columnar table 'table'.
     *  @param table     the table/relation to populate
     *  @param refTable  the tables that are referenced by 'table' via its foreign keys
     */
    def popTable (table: Relation, nRows: Int, refTables: ArrayBuffer [Relation] = null): Unit =
        val ranD  = RandomVecD (dim = nRows, max = 2 * nRows)
        val ranI  = RandomVecI (dim = nRows, max = 2 * nRows, unique = false)
        val ranS  = RandomVecS (dim = nRows, unique = false)
        val uranI = RandomVecI (dim = nRows, max = 2 * nRows)
        val uranS = RandomVecS (dim = nRows)

        val n     = table.cols                                 // number of columns
        val cn    = table.colName                              // column names
        val dn    = table.domain                               // columns  domains
        val pk    = table.key                                  // position of primary key
        val fkey  = table.fKeys                                // sequence of foreign key specifications
        var cols  = Vector.empty [Vect]

        for j <- cn.indices do                                 // for jth column
            val fk_i = find (cn(j), fkey)                      // which foreign key, if any
            val colj = if j == pk then genUnique (dn(j))       // generate values for jth column
                       else if fk_i >= 0 then genFkey (fk_i)
                       else genVal (dn(j))
            cols = cols :+ colj                                // FIX - need more efficient approach
        end for

        debug ("popTable", s"cols = $cols")
        table.col = cols

        //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /*  Generate values for column 'j' by extracting random values from
         *  the column in the referenced table specified by 'fk_j'.
         *  @param fk_i  the relevant foreign key constraint specification
         */
        def genFkey (fk_i: Int): Vect =
            val refTName = fkey(fk_i)._2
            val refCol   = fkey(fk_i)._3
            var iref     = -1
            debug ("genFkey", s"refTName = $refTName")

            breakable {
                for k <- refTables.indices if refTName == refTables(k).name do
                    iref = k
                    break ()
                end for
            } // breakable

            if iref >= 0 then
                val refTable = refTables(iref) 
                val rCol     = refTable.col(refCol)
                val ranK     = RandomVecI (dim = nRows, max = refTable.rows - 1, unique = false)
                rCol.asInstanceOf [VectorI].set (ranK.igen)         // FIX - generalize
                rCol
            else
                flaw ("genFkey", s"reference table $refTName not matched")
                null
            end if
        end genFkey

        //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /*  Randomly Generate unique values for column 'j'.
         *  @param dn  the domain (datatype)
         */
        def genUnique (dn: Char): Vect =
            dn match
            case 'D' => { flaw ("genUnique", "type `Double` should not be a primary key"); null }
            case 'I' => uranI.igen
            case 'L' => uranI.igen
            case 'S' => uranS.sgen
            case _  => { flaw ("genUnique", "type not supported"); null }
            end match
        end genUnique

        //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /*  Randomly Generate values for column 'j'.
         *  @param dn  the domain (datatype)
         */
        def genVal (dn: Char): Vect =
            dn match
            case 'D' => ranD.gen
            case 'I' => ranI.igen
            case 'L' => ranI.igen
            case 'S' => ranS.sgen
            case _  => { flaw ("genVal", "type not supported"); null }
            end match
        end genVal

    end popTable 

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the column with name 'cn' in a foreign key constraint, returning
     *  the which constraint is matched, -1 otherwise.
     *  @param cn    the name of the column
     *  @param fkey  the sequence of foreign keys contraint specifications
     */
    def find (cn: String, fkey: ArrayBuffer [(String, String, Int)]): Int =
        if fkey == null then return -1
        for i <- fkey.indices if cn == fkey(i)._1 do return i          // matched ith contraint
        -1
    end find

end TableGen


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `tableGenTest` main function is used to test the `TableGen` object.
 *  Create unpopulated tables and use the table generator to populate their columns.
 *  > runMain scalation.database.relation.tableGenTest
 */
@main def tableGenTest (): Unit =

    val student = Relation ("student", 0, "ISSS", "sid", "name", "address", "status")
    TableGen.popTable (student, 40)
    student.show ()

    val professor = Relation ("professor", 0, "ISS", "pid", "name", "deptid")
    TableGen.popTable (professor, 10)
    professor.show ()

    val course = Relation ("course", 0, "SSSS", "cid", "deptid", "crsname", "descr")
    TableGen.popTable (course, 20)
    course.show ()

    val teaching = Relation ("teaching", 0, "ISSI", "tid", "cid", "semester", "pid")
    teaching.fKeys = ArrayBuffer (("cid", "course", 0), ("pid", "professor", 0))
    TableGen.popTable (teaching, 50, ArrayBuffer (course, professor))
    teaching.show ()
    teaching.showFkey ()

    val transript = Relation ("transript", 0, "IISSS", "trid", "sid", "trid", "grade")
    transript.fKeys = ArrayBuffer (("sid", "student", 0), ("trid", "teaching", 0))
    TableGen.popTable (transript, 70, ArrayBuffer (student, teaching))
    transript.show ()
    transript.showFkey ()

end tableGenTest

