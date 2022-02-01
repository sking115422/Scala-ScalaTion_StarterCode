
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Sat Aug 29 14:14:32 EDT 2020
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   EdgeType - a collection of edges of the same type
 */

package scalation
package database
package graph

import scala.collection.mutable.Map
//import scala.collection.immutable.{Vector => VEC}
import scala.collection.mutable.{ArrayBuffer => VEC}

import scalation.scala2d.Colors.{Color, blue}
import scalation.scala2d.{CurvilinearShape, Arrow, QArrow}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `EdgeType` class collects edges of the same type, e.g., knows relationship type.
 *  An edge type is analogous to a relation with foreign keys in an RDBMS.
 *  @param _name   the name of this edge type ('name' from `Identifiable`)
 *  @param from    the source vertex
 *  @param schema  the property names for this edge type
 *  @param to      the target vertex
 *  @param edges   the set of edges having this edge type (extension)
 *  @param color   the display color for edges of this type
 *  @param shape   the display shape template for edges of this type
 */
class EdgeType (_name: String, val from: VertexType, val schema: VEC [String], val to: VertexType,
                val edges: VEC [Edge] = VEC [Edge] (),
                val color: Color = blue, val shape: CurvilinearShape = Arrow ())
      extends Identifiable (_name)
         with Serializable:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return a new shape object based on the shape template.
     */
    def newShapeObj: CurvilinearShape =
        shape match
        case s: Arrow => Arrow ()
        case _        => QArrow ()
        end match
    end newShapeObj

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check that the properties names are in the schema for this edge type, returning
     *  whether they match the schema.
     */
    def check: Boolean = true                                           // FIX - implement

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Expand this edge type with its to vertex type.
     */
    def expandTo: EdgeType =
        val edgez = for e <- edges yield Edge (e.from, e.prop +++ e.to.prop, null)
        EdgeType (name + "_et", from, schema ++ to.schema, null, edgez)
    end expandTo

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Expand this edge type with its from vertex type.
     */
    def expandFrom: EdgeType =
        val edgez = for e <- edges yield Edge (null, e.from.prop +++ e.prop, e.to)
        EdgeType (name + "_ef", null, from.schema ++ schema, to, edgez)
    end expandFrom

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Expand this edge type with both its from and to vertex types.
     */
    def expand: VertexType =
        val vertices = for e <- edges yield Vertex (e.from.prop +++ e.prop +++ e.to.prop)
        VertexType (name + "_e", from.schema ++ schema ++ to.schema, vertices)
    end expand

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Project each edge in this edge type down to the given subschema of properties.
     *  @param subschema  the subset of properies to project onto
     */
    def project (subschema: VEC [String]): EdgeType =
        EdgeType (name + "_p", from, subschema, to,
                  for e <- edges yield Edge (e.from, e.prop.filter (subschema contains _._1), e.to))
    end project

    inline def project (subschema: String*): EdgeType = project (VEC (subschema :_*))

    inline def π (subschema: String*): EdgeType = project (VEC (subschema :_*))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Select the edges in this edge type that satisfy the predicate.
     *  @param pred  the predicate to satisfy
     */
    def select (pred: Property => Boolean): EdgeType =
        EdgeType (name + "_s", from, schema, to,
                  for e <- edges if pred (e.prop) yield e)
    end select

    inline def σ (pred: Property => Boolean): EdgeType = select (pred)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Union this edge type with a second edge type.
     *  @param et2  the second edge type
     */
    def unionAll (et2: EdgeType): EdgeType =
        EdgeType (name + "_ua_" + et2.name, from, schema, to, edges ++ et2.edges)
    end unionAll

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Union this edge type with a second edge type with no duplication.
     *  @param et2  the second edge type
     */
    def union (et2: EdgeType): EdgeType =
        EdgeType (name + "_u_" + et2.name, from, schema, to, (edges ++ et2.edges).distinct)
    end union

    inline def ∪ (et2: EdgeType): EdgeType = union (et2)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Intersect this edge type with a second edge type.
     *  @param et2  the second edge type
     */
    def intersect (et2: EdgeType): EdgeType =
        EdgeType (name + "_u_" + et2.name, from, schema, to, edges intersect et2.edges)
    end intersect

    inline def ∩ (et2: EdgeType): EdgeType = intersect (et2)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Make all the edges in this edge type distinct, i.e., no duplicate edges.
     */
    def distinct: EdgeType =
        EdgeType (name + "_d", from, schema, to, edges.distinct)
    end distinct

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Subtract (minus) second edge type from this edge type.
     *  @param et2  the second edge type
     */
    def minus (et2: EdgeType): EdgeType =
        EdgeType (name + "_m_" + et2.name, from, schema, to, edges diff et2.edges)
    end minus

    inline def - (et2: EdgeType): EdgeType = minus (et2)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Rename the old property name with the new property name.
     *  @param pOld  existing property name
     *  @param pNew  new property name
     */
    def rename (pOld: String, pNew: String): EdgeType =
        EdgeType (name + "_r", from, schema.updated (schema.indexOf(pOld), pNew), to,
                  for e <- edges yield Edge (e.from, e.prop.map (p => if p._1 == pOld then pNew -> p._2 else p), e.to) )
    end rename

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Order the edges within this edge type by the values of the given property name.
     *  @param pname  the property name on which to sort
     */
    def orderBy (pname: String): EdgeType =
        EdgeType (name + "_o", from, schema, to, edges.sortWith (_.prop(pname) < _.prop(pname)))
    end orderBy

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Limit the number of edges to n after skipping the first s.
     *  @param n  the number of edges to keep
     *  @param s  the number of edges to skip
     */
    def limit (n: Int, s: Int = 0): EdgeType =
        EdgeType (name + "_l", from, schema, to, edges.slice (s, s + n))
    end limit

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Insert a single edge into the existing list of edges in this edge type.
     *  returning the updated edge type/set.
     *  @param e  the edge to insert
     */
    def insert (e: Edge): EdgeType =
        edges :+ e
        this
    end insert

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add edges into the existing list of edges in this edge type.
     *  @param es  the edges to add
     */
    def add (es: Edge*): Unit = edges ++= es

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Delete a single edge from the existing list of edges in this edge type.
     *  returning the updated edge type/set.
     *  @param e  the edge to delete
     */
    def delete (e: Edge): EdgeType =
        edges -= e
        this
    end delete

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Print this edge type.
     */
    def print (): Unit =
        banner ("Edge Type:")
        println (s"et = $this")
    end print

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert this edge type to a string.
     */
    override def toString: String =
        val edgesStr = if edges == null then "()\n"
                       else edges.map ("\n\t\t" + _.toString)
        s"EdgeType (id = $id, name = $name, schema = $schema,\n edges = $edgesStr \n)"
                   .replace ("ArrayBuffer", "VEC")
    end toString

end EdgeType

