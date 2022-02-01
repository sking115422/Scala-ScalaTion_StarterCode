
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Sat Oct  2 18:36:01 EDT 2021
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Edge in a Property Graph
 */

package scalation
package database
package graph

import scala.math.{atan2, cos, Pi, sin}

import scalation.mathstat._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Edge` class maintains the edge's connections between vertices as well as its own properites.
 *  An edge is roughly analogous to implicit relationship manifest via foreign key-primary key pairs.
 *  The parameters may be thought of like a triple, e.g., (h, r, t) or (s, p, o).
 *  @param _name  the name of this edge ('name' from `Identifiable`)
 *  @param from   this edge's source/from vertex
 *  @param prop   maps edge's property names into property values
 *  @param to     this edge's target/to vertex
 *  @param shift  number of units to shift to accomodate a bundle of egdes in a composite edge
 */
class Edge (_name: String, val from: Vertex, val prop: Property, val to: Vertex, val shift: Int = 0)
      extends Identifiable (_name)
         with Spatial (if from == null then to.pos else from.pos)
         with Serializable:

    val tokens = Set [Topological] ()                     // topological objects/tokens at this edge

    private val GAP = 5                                   // gap distance between edges in a bundle

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert this edge object to a string.
     */
    override def toString: String =
        val propStr = if prop == null then "()" else prop.mkString (", ")
        val fromStr = if from == null then "null-vertex" else s"Vertex(${from.id}, ${from.name})"
        val toStr   = if to   == null then "null-vertex" else s"Vertex(${to.id}, ${to.name})"
        s"Edge($id, $name, $fromStr, $propStr, $toStr, $shift)"
    end toString

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** When shift is non-zero, the endpoints for the edge must be shifted by shift * GAP
     *  in the direction orthogonal to the edge.
     */
    def calcEndPoints: (VectorD, VectorD) = 
        val p1 = from.pos(0 to 2) + from.pos(2 to 4) / 2.0
        val p2 = to.pos(0 to 2)   + to.pos(2 to 4) / 2.0
        val an = atan2 (p2(1) - p1(1), p2(0) - p1(0)) + Pi / 2
        val del = (shift * GAP * cos (an), shift * GAP * sin (an))
        (VectorD (p1(0) + del._1, p1(1) + del._2),
         VectorD (p2(0) + del._1, p2(1) + del._2))
    end calcEndPoints

end Edge


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Edge` companion object provides a factory function for creating edges.
 */
object Edge:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create an object of type `Edge`.
     *  @param from  the source vertex
     *  @param prop  maps edge's property names into property values
     *  @param to    the target vertex
     */
    def apply (from: Vertex, prop: Property, to: Vertex): Edge =
        new Edge (null, from, prop, to)
    end apply

end Edge

