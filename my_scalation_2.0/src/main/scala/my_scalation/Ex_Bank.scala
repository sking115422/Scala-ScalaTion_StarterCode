
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Wed Dec 30 14:48:41 EST 2009
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Example Model: Bank - Event-Scheduling
 */

package my_scalation

import scalation.mathstat.Statistic
import scalation.random.Exponential
import scalation.random.RandomSeeds.N_STREAMS
import scalation.simulation.event._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `runEx_Bank` function is used to launch the `BankModel` class.
 *  > runMain my_scalation.runEx_Bank
 */
@main def runEx_Bank (): Unit = new BankModel () 


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BankModel` class defines a simple Event-Scheduling model of a Bank where
 *  service is provided by one teller and models an M/M/1 queue.
 *  @param name    the name of the simulation model
 *  @param reps    the number of independent replications to run
 *  @param nStop   the number arrivals before stopping
 *  @param stream  the base random number stream (0 to 999)
 */
class BankModel (name: String = "Bank", reps: Int = 1, nStop: Int = 100, stream: Int = 0)
      extends Model (name, reps):

    //--------------------------------------------------
    // Initialize Model Constants

    val lambda = 6.0                                  // customer arrival rate (per hr)
    val mu     = 7.5                                  // customer service rate (per hr)

    //--------------------------------------------------
    // Create Random Variables (RVs)

    val HOUR       = 60.0
    val iArrivalRV = Exponential (HOUR / lambda, stream)
    val serviceRV  = Exponential (HOUR / mu, (stream + 1) % N_STREAMS)

    //--------------------------------------------------
    // Create State Variables

    var nArr      = 0.0                               // number of customers that have arrived
    var nIn       = 0.0                               // number of customers in the bank
    var nOut      = 0.0                               // number of customers that finished & left

    val t_ia_stat = new Statistic ("t_ia")            // time between Arrivals statistics
    val t_s_stat  = new Statistic ("t_s")             // time in Service statistics
    val waitQueue = WaitQueue (this)                  // waiting queue that collects stats
    addStats (t_ia_stat, t_s_stat)

    //--------------------------------------------------
    // Specify Logic for each Type of Simulation Event

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** `Arrival` is a subclass of `Event` for handling arrival events.
     *  The 'occur' method triggers future events and updates the current state.
     *  @param customer  the entity that arrives, in this case a bank customer
     *  @param delay     the time delay for this event's occurrence
     */
    case class Arrival (customer: Entity, delay: Double)
         extends Event (customer, this, delay, t_ia_stat):

        def occur (): Unit =
            if nArr < nStop - 1 then
                val toArrive = Entity (iArrivalRV.gen, serviceRV.gen, BankModel.this)
                schedule (Arrival (toArrive, toArrive.iArrivalT))
            end if
            if nIn == 0 then
                schedule (Departure (customer, customer.serviceT))
            else   
                waitQueue.enqueue (customer)                  // collects time in Queue statistics
            end if
            nArr += 1                                         // update the current state
            nIn  += 1
        end occur

    end Arrival

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** `Departure` is a subclass of `Event` for handling departure events.
     *  The 'occur' method triggers future events and updates the current state.
     *  @param customer  the entity that departs, in this case a bank customer
     *  @param delay     the time delay for this event's occurrence
     */
    case class Departure (customer: Entity, delay: Double)
         extends Event (customer, this, delay, t_s_stat):

        def occur (): Unit =
            leave (customer)                                  // collects time in sYstem statistics
            if ! waitQueue.isEmpty then
                val nextService = waitQueue.dequeue ()        // first customer in queue
                schedule (Departure (nextService, nextService.serviceT))
            end if
            nIn  -= 1                                         // update the current state
            nOut += 1
        end occur

    end Departure

    //--------------------------------------------------
    // Start the simulation after scheduling the first priming event

    val firstArrival = Entity (iArrivalRV.gen, serviceRV.gen, this)
    schedule (Arrival (firstArrival, firstArrival.iArrivalT))     // first priming event
    simulate ()                                                   // start simulating

    report (("nArr", nArr), ("nIn", nIn), ("nOut", nOut))
    reportStats ()
    waitQueue.summary (nStop)

end BankModel

