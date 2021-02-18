package pipedsl.common


object LockInterface {

  /*
   *  Locks:
   *    reserve(R)
   *    reserve(W)
   *    block(lock)
   *    read(R_lock)
   *    write(W_lock)
   *    release(lock)
   *
   *  General Memories (no address):
   * 
   *   isEmpty(); (lock free)
   *   reserve();
   *   owns(id);
   *   write(id);
   *   read(id);
   *   release(id);
   *
   *  Combinational Memories:
   * 
   *   readName(addr); (R)
   *   allocName(addr); (W)
   *   isValid(name); (R/W)
   *   read(name); (R)
   *   write(name); (W)
   *   commit(name); (R/W)
   *   
   *  Asynchronous Memories:
   *
   *   reserveRead(addr); (R)
   *   reserveWrite(addr); (RW)
   *   isValid(name); (R/W)
   *   read(name); (R)
   *   write(name); (W)
   *   commitRead(name); (R)
   *   commitWrite(name); (W)
   */

 ////////////////////////////////////////

  /*
   *  In general, we need to be able to translate between
   *  lock operations and the low-level implementation interfaces.
   * 
   * Functionality we need for this:
   * 
   *  (1) checkConflicts
   * 
   *  Given a set of lock operations which are scheduled for
   *  a given stage, return True is schedulable, else False.
   *  e.g., A renaming registerfile can 'reserve(R), block, read, and free'
   *  in a single-cycle. 
   *  On the other hand, they cannot support same-cycle 'reserve(W)', and 'free'.
   * 
   *  --- Ideally checkConflicts is implemented automatically given some kind of schedule.
   *      Since there is a strict ordering that each thread will use to call these methods,
   *      we can specify it simply as a list of sets. Each set represents non-conflicting operations.
   *
   *  (2) mergeOps
   *
   *  Given a set of lock operations scheduled for a given stage, return the
   *  merged set of operations. It is convenient to specify this separately from
   *  conflicts since it can be assumed that anything "not merged" is translated
   *  obviously.
   *  
   *  E.g., the General Lock Memory (w/o addresses) merges "reserve, read + free"
   *  into "isEmpty, read"
   * 
   *  (3) assignPorts
   *  
   *  Given a set of _translated_ operations, assign port numbers to each one
   *  or return ERROR if the implementation doesn't support the necessary number of ports.
   * 
   *  E.g., the Renaming Register file supports 2 readNames()
   *  per cycle. If there are 2 readNames() w/ different arguments, they will be assigned
   *  different ports; if there are >2, then an error is returned.
   * 
   */

}
