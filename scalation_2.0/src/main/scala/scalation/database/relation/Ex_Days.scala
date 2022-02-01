
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Sat Dec 11 18:35:14 EST 2021
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Example Database: Days of the Week
 */

package scalation
package database
package relation

import scala.collection.mutable.{ArrayBuffer => VEC}

object Ex_Days:

    val weekdays = Relation ("weekdays",
                             VEC ("day", "time"),
                             VEC [Row] (Vector ("Mon", 5.00),
                                        Vector ("Tue", 8.15),
                                        Vector ("Wed", 6.30),
                                        Vector ("Thu", 9.45),
                                        Vector ("Fri", 7.00)),
                             0, "SD")

    val weekend  = Relation ("weekend",
                             VEC ("day", "time"),
                             VEC [Row] (Vector ("Sat", 3.00),
                                        Vector ("Sun", 4.30)),
                             0, "SD")

end Ex_Days

