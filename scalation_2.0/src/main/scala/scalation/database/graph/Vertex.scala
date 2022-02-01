
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Sat Oct  2 18:36:01 EDT 2021
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Vertex in a Property Graph
 */

package scalation
package database
package graph

import scalation.mathstat.VectorD

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Vertex` class maintains properties for a vertex, e.g., a person.
 *  A vertex is analogous to a tuple in an RDBMS.
 *  @param _name  the name of this vertex ('name' from `Identifiable`)
 *  @param prop   maps vertex's property names into property values
 *  @param _pos   the position (Euclidean coordinates) of this vertex ('pos' from `Spatial)
 */
class Vertex (_name: String, val prop: Property, _pos: VectorD = null)
      extends Identifiable (_name)
         with Spatial (_pos)
         with PartiallyOrdered [Vertex]
         with Serializable:

    val tokens = Set [Topological] ()                     // topological objects/tokens at this vertex                     

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compare two spatial objects based on their space coordinates.
     *  @param other  the other item to compare with this item
     */
    override def tryCompareTo [B >: Vertex : AsPartiallyOrdered] (other: B): Option [Int] =
        val oth = other.asInstanceOf [Vertex]
        pos tryCompareTo oth.pos
    end tryCompareTo

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert this vertex object to a string.
     */
    override def toString: String =
        val propStr = if prop == null then "()" else prop.mkString (", ")
        s"Vertex($id, $name, $propStr, $pos)"
    end toString

end Vertex


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Vertex` companion object provides a factory function for creating vertices.
 */
object Vertex:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create an object of type `Vertex`.
     *  @param prop  maps vertex's property names into property values
     *  @param pos   the position (Euclidean coordinates) of this vertex
     */
    def apply (prop: Property, pos: VectorD = null): Vertex = 
        new Vertex (null, prop, pos)
    end apply

end Vertex

