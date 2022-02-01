
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Sat Aug 28 18:17:12 EDT 2021
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Functions to Read from Files in ScalaTion's "data" Directory
 */

package scalation

import scala.io.Source

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Read and print each line in the file.
 *  @param fileName  the file name (not the complete path)
 */
def readFile (fileName: String): Int =
    val path   = DATA_DIR + fileName
    println (s"readFile: $path")
    val buffer = Source.fromFile (path)
    val lines  = buffer.getLines
    var i = 0
    for line <- lines do
        println (line)
        i += 1
    end for
    buffer.close ()
    i
end readFile

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Read each line in the file and put each one into a returned array.
 *  @param fileName  the file name (not the complete path)
 */
def readFileIntoArray (fileName: String): Array [String] =
    val path    = DATA_DIR + fileName
    println (s"readFileIntoArray: $path")
    val buffer  = Source.fromFile (path)
    val lineArr = buffer.getLines.toArray
    buffer.close
    lineArr
end readFileIntoArray


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `readFileTest` main function is used to test the file reading functions.
 *  > runMain scalation.readFileTest
 */
@main def readFileTest (): Unit =

    println (s"readFile: number of lines = ${readFile ("travelTime.csv")}")

    println (s"readFileIntoArray: number of lines = ${readFileIntoArray ("travelTime.csv").length}")
 
end readFileTest

