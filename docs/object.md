# Object type system for PDL

## Things to do
 + locks will potentially have atomic reads and writes
 + latencies on lock operations, and another latency on reads and writes. pass
   results of lock operations forward, so that latencies can be checked.
 + externs have latencies on methods.
 + don't need lock name in pipeline. Check usage constraints on
   instantiation.
   
## To figure out
 - Can we represent these things in bluespec?
 - how to make nicer to combine different locks and different memories (blows up
   n^2).



maybe externs are a good place to start w/ implementation, though Drew already
implemented a similar thing to make branch predictors work. How much to base off
of this?

## General
 - should primitives be objects? (like in scala?)
   + this seems like a bad idea, but idk
 - every type would have an "interface" of methods
 - each method would have argument types, a return type, as well as a latency
	 * adding a latency to each method type would help with typechecking
       different lock/extern types when they are swapped out.
	 * latency would be kind of annoying
		 - could be that combinational <: sequential <: asynchronous
		 - this would be something of an issue since certain combinational
           things might HAVE to be done combinationally (i.e. won't stick around
           to the next stage)? 
		 - one approach could be to define another latency for things that are
           only available for one stage, but that doesn't really make sense as
		   I'm pretty sure that this is only about reserve + block + use +
           release all at once. See the lock section about wrapping stuff in an
           acquired block.
		 - this would mean that the base lock interface would have to have
           everything asynchronous, which is kinda just stupid.
		 - there could also be no base lock interface, or at least the latency
           wouldn't be a part of it. This might imply that latency is a
           different part of the type system that has a special relation with
           inheritance.
 - currently don't have a great distinction between instances and classes
 - pipelines will be objects with a single implicit method? Could also include
   multiple methods but that seems a bit strange for pipelines...
 - it might make more sense to think of pipes as asynch methods, one stage pipes
   as sequential methods, and then functions will just be combinational
   methods.
    + this is a seemingly strange distinction between asynch and
      sequential. Perhaps there is a good solution to this? although asynch and
      sequential already have a lot of similar behavior since there is an
      implicit block to make asynch things look sequential (don't run the next
      stage till the asynch stuff is ready).

#### Code generation
 - compile objects to bluespec modules, using bluespec methods?
 - need some way of doing inheritance
   * PDL could figure out the inheritance, and write in all of the methods, so
     that the modules would work.
   * this seems like a hacky/bad solution though. How to leverage typeclasses?
   * maybe if we want to have a class/object system that's different enough from
     bluespec we can just do it in house, and generate the specialized
     versions...
 
## Locks
 - read
 - write
 - reserve
 - block
 - release
 - acquire - this would only be different from reserve + block on some
   implementations, and would just be sugar on others
 - also the issue of when you can do all three operations in one cycle. Perhaps
   there could be some sort of "acquired" block that can be wrapped around some
   commands within a stage such that
   ```
   lock.acquired(addr, r/w)
     {
	   cmds;
     };
   ```
   is functionally equivalent to
   ```
   lock.reserve(addr, r/w);
   lock.block(addr);
   cmds;
   lock.release(addr);
   ```
 - Since this is essentially a new syntax because of certain implementations,
   are there more options? If this is it, that seems pretty reasonable, but if
   there are too many different quirks of different implementations the current
   merging system looks a little more attractive.
 - The other thing that we might want to do is to keep the acquire function as
   it is described above, since it is possible that for some implementations
   reserve + block can be done better all at once.
 - This would also be a clearer syntax since it looks more like a provision for
   a region rather than the user having to track down blocks and releases.
 - address based and general locks are just two different things
 - perhaps reads and writes should be pulled out in a different way as well,
   such as
   ```
   lock.read.reserve(addr)
   lock.read.release(addr)
   ```
 - another option is encoding it in the method name, such as
   ```
   lock.reserve_read(addr)
   ```
 - this is very long :( could always shorten it to something like
   `lock.reserve_r(addr)`
 - in this case should the r/w be encoded in every operation? this seems like it
   would make sense even though it COULD be determined by the compiler.
 - this doesn't really reflect the structure of the lock that well though, as
   it's not like there is a read lock and a write lock inside, they really are
   two ways of interacting with the same structure
 - could have different latency based on different operations (r/w)
 - the LockImplementation.scala file could perhaps be greatly simplified by
   this? Providing an interface would take care of most of typechecking, though
   code generation might be a problem
 
 ^^ on the PDL level
   
### FAQueue&lt;regfile&gt;
 - `reserve[comb](addr, r/w)`
 - `block[comb](addr)`
 - `release[comb](addr)`
 - `read[comd](addr)`
 - `write[seq](addr, data)`
 
## Extern
These would just be objects w/ methods as defined above 

 + how to check that the interface that the user writes down actually matches
   that of the verilog/bluespec file they want to link with? trust?

## Inheritance
Just support basic single parent inheritance for now.
You inherit methods of the parent, and if you want to override them use the
"override" keyword
