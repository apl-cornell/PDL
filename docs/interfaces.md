# Runtime Interfaces

This document describes the 'runtime' structures that our surface language relies on for correctness.
Additionally, we discuss how modules in this language interact and can be typed.

## Memories

In hardware, there are two primary memory interfaces: synchronous, and _asynchronous_.
Register files are an example of synchronous interfaces, which typically export a combinational
(same cycle) _read_ operation and a sequential (visible next cycle) _write_ operation.
Asynchronous memories have a _request_ and _response_ interface; these two interactions
are either linked through request IDs or via FIFO correspondance.

The current version of the language requires memories to be accessed via the `<-` operator,
for example:

```
arg1 <- rf[rs1];
---
out = arg1 + 3;
```

And the type system ensures that `arg1` cannot be used until the next stage (as in the example above).
This reflects the restrictions on _asynchronous__ memories accurately, since requests must be sent
in one stage and responses received in another. Similarly, the type system ensures that locks
are not released until after a request is sent (these can happen in the same stage since
we assume that in-flight writes are visible to future reads).

However, this restriction is unnecessary for synchronous memories with a _combinational_ (same cycle)
read operation. This suggests that our type system needs to differentiate between read and write latencies.
Read latencies are relevant for when values can be next used, and write latencies are relevant for when
locks can be released.

All memories _induce lock restrictions_ so that data operations happen in the correct order.
Memories with a combinational read must be accessed (for reads) using the normal `=` operator instead
of the `<-` operator, but memory writes and asynchronous memory operations do not complete until a future cycle and thus
_do_ require the `<-` operator.

### Memory Types

Currently, memory variable declarations look like this: `rf: int<32>[5]`.
That expression defines a variable `rf` which is a memory with 5-bit addresses and 32-bit integer data.

We could imagine memory types that also describe read and write latencies:

`rf: Memory(addr: bit<5>, data: int<32>, readlatency: 1, writelatency: N)`

This is a bit verbose so I imagine we would make a few predefined type macros, like:

`RegFile<int<32>>[5] === Memory(addr: bit<5>, data: int<32>, readlatency: 0, writelatency: 1)`

I imagine supporting the following latencies:
 - 0 = combinationally available, writes can be observed by concurrent reads, writes nor reads use the req/resp interface.
 - 1 = results available next cycle, writes don't need a response (since they will be visible by next cycle reads).
 - N = asynchronous request, both writes and reads need request responses.


The main restrictions memory types will enforce are the following:
 - Locks must be acquired in the same stage (or earlier) as read or write operations.
 - Locks cannot be released until reads and writes complete (so a latency > 0 op and lock release cannot be in the same stage).
 - Non-combinational reads must be accessed with the `<-` operator; combinational reads must be executed with the '=' operator.
 - Passing the `---` stage barrier implies that a response has been received and thus latency = 1 and latency = N are treated the same by the type system. The distinction between 1 and N is primarily for potential compiler optimizations.

## Locks

Every memory implicitly has a lock structure associated with it.
Locks are used to dynamically enforce the sequential ordering of relevant reads and
writes and the type system ensures that lock operations are safely placed.


Locks support the following operations:

 - `Check(ID id)`: returns True if thread `id` currently owns the lock,
    or the lock is unowned, else returns False. This is a combinational interface,
    so the reads are visible in the same cycle.
 - `Acquire(ID id)`: checks that thread `id` can acquire the lock (according to `Check`)
    and if it can, updates the lock state to acquire the lock, otherwise block.    
 - `Reserve(ID id)`: if this thread can acquire the lock (according to `Check`) then acquire
    it, else put this id into a lock acquisition queue. If the queue is full then block.
 - `Release(ID id)`: check that thread `id` currently owns the lock (according to `Check`)
    and updates the lock state to release the lock, otherwise block. If some other thread
    is in the acquisition queue, it now owns the lock.

### Surface Syntax

In the surface language, we don't expose `Check` since it is only needed
during target code generation. Additionally, we don't use thread IDs and have
the compiler generate those automatically. While we can optimize thread IDs more,
the simplest algorithm is to assume that all stages could be filled with unique
threads and thus the number of bits for the thread id we need is `log_2(num stages)`
and this data gets passed between any stage transition.

Therefore, in a real program we might see the following code:

```
acquire(rf); # call rf's lock's acquire method, ID is implicit.
arg1 <- rf[rs1];
---
release(rf);
```

### Location Specific Locks:

The locks we've described so far exist at the memory level, so each memory
has only a single lock associated with it and thus threads which don't actually
have a data dependency can block each other's execution. Consider the following
instruction stream (not code in our language):

```
add r1, r2, r3
add r4, r5, r6
```

These two instructions have no data dependence and thus their reads
and writes may safely interleave. These are implemented in the actual
CPU pipeline with the following segments:

```
...
acquire(rf);
arg1 <- rf[rs1];
arg2 <- rf[rs2];
---
...
---
rf[rd] <- result;
release(rf);
```

You can see that the acquire and release commands create
a critical section that will prevent any interleaving
of reads and writes between instructions.

Our location specific locks allow us to write a slightly
different program that _will_ allow some interleaving of reads
and writes to different memory addresses.

```
acquire(rf[rs1]);
acquire(rf[rs2]);
reserve(rf[rd]); #needed to ensure that locks to rd are acquired in the correct order, but doesn't block execution.

arg1 <- rf[rs1];
arg2 <- rf[rs2];
---
release(rf[rs1]);
release(rf[rs2]);
...
---
acquire(rf[rd]); #check that we've acquired the lock we reserved earlier.
rf[rd] <- result;
release(rf);
```

However, these introduce a few new problems: lock aliasing and runtime efficiency.

### Lock Aliasing

When looking at this circuit:

```
acquire(rf[rs1]);
reserve(rf[rd]);
---
release(rf[rs1]);
...
---
acquire(rf[rd]);
release(rf[rd]);
```

we do not know whether `rs1 == rd` or not. This causes problems since
we want to release the `rs1` lock in the second stage, but not the `rd` lock.

Really, we want the dynamic semantics to reflect the following behavior:
```
acquire(rf[rs1]);
if (rs1 != rd) reserve(rf[rd]); # perhaps use a special syntactic form instead of if
---
if (rs1 != rd) release(rf[rs1]);
...
---
acquire(rf[rd]);
release(rf[rd]);
```
This complicates the typing rules a bit but can be summed up with the following restrictions:
 - You can only acquire/reserve/release a lock if it doesn't alias with other acquired or reserved locks
 - You can only acquire/reserve a lock if this thread has not already released an alias (this is more relevant
   to the next section on inter-thread aliasing)

It is unclear what the right mechanism for enforcing these requirements is, but likely
either a special syntactic form for location checks OR a simple abstract analysis over
constant and variable (in)equality will suffice.

Depending on the precise dynamic semantics of locks, we may be able to loosen restrictions a bit
as well. For instance, if at run-time calling `acquire` is idempotent, then we don't need to include
the repeated checks on aliasing during acquire calls since the runtime is effectively doing that already.

#### Inter-Thread Aliasing

In addition to lock aliasing within a thread (as we just discussed), aliasing affects
how we check that locks are acquired in order. The type system enforces that locks are acquired
in-order by checking that there is a single in-order stage at which any lock is acquired (or reserved).
With location specific locks, we don't actually loosen this restriction at all.

For instance, the following does not typecheck:

```
if (test) {
 ---
 acquire(rf);
} else {
 ---
 acquire(rf);
}
```
Since we can't guarantee that the two lock acquisitions happen in the original thread
order (`if` induces out of order execution since it creates a branch in the pipeline)
and this won't typecheck.

Similarly, the following doesn't typecheck:

```
if (test) {
 ---
 acquire(rf[rs1]);
} else {
 ---
 acquire(rf[rs2]);
}
```

Because we can't guarantee that there is no alias between `rs1` or `rs2` from one
thread and those locations from another. If we could guarantee that `rs1 != rs2`
_for all execution threads_ (not just the currently executing one), then this could typecheck.
For this reason, even adding extra disjointness checks about the current execution
(like `if (rs1 != rs2)` ...) would not allow this to typecheck.

It is still unclear whether or not this is a real problem; I posit that it
isn't but I thought I'd mention it.

### Efficient Per-Location Locks

Locks, as we've described them so far, imply a runtime queue datastructure.
When there are only a handful of memories in the circuit and a pipeline of reasonable depth,
this is likely to be a very minimal overhead. However, with per-location locks,
this implementation would be incredibly inefficient to build since there may be thousands
or millions of locations; we certainly can't afford millions of queues to track each location
individually.

I see a few possible implementation strategies:

 - For each memory create a small number of locks in a fully associated array
   (meaning any of the locks can be used for any address in the memory).
   Lock operations now need to look up the relevant underlying lock structure for
   a given address, which slightly complicates the lock implementation; additionally,
   threads may block on acquisition if there are not more available slots in the array
   even if the requested lock is unacquired.
   The minimum number of locks necessary to avoid deadlock can be statically determined (by looking at
   the number of concurrent acquire statements that may alias), and increasing
   this number can potentially increase parallelism, to a point.
   This is likely to be fairly efficient, since we will probably only need a single-digit
   number of locks per memory and full associativity isn't that expensive in this case.
   
 - Locks are represented as queues of thread ids. Instead they could be queues of IDs plus
   lists of acquired locations. There is some subtlety in ensuring that these lists
   are of finite and statically determinable size, but for similar reasoning as in the prior
   case this should be doable. In this case, acquiring a lock requires searching through
   the queue to see if any earlier threads have acquired or reserved the lock already.
   This also leads to fully associative structures (i.e. acquisition requires examining
   all of the queues entries) but maybe could be built efficiently.

## Data Forwarding (Bypassing)

Forwarding (or bypassing) is a common hardware optimization where one instruction
reads data from a later stage in the pipeline (earlier instruction) instead of
reading from a shared memory. The idea is that _writes_ by earlier instructions
can be _forwarded_ to later _reads_ without waiting for synchronization on the memory.

In our language, we plan to piggy-back forwarding on top of locks since locks
are our synchronization mechanism for shared memories.

### Syntax

There are two components to bypassing:
 1) Generating the new value for the memory location.
 2) Reading the bypassed value (instead of accessing the memory).

The first needs its own syntax for promising a write will eventually be committed to the memory:

```
int<32> result = ...
commit(rf[rd], result);
---
rf[rd] <- result;
```

`commit(loc, data)` tells the compiler that the current instruction will eventually write `result` into `loc`.
The compiler then checks that such a write is actually going to be reflected in the final state of the memory
at the point at which the lock is released. For correctness, this analysis does not need to be very precise.
For instance, since variables are write-once in this language, we can syntactically check that there is a matching write statement.

The read points for these bypass values are implicitly associated with the points at which the relevant locks are `reserved`.

```
int<32> arg1 = reserve(rf[rs1]); //block until the lock can be acquired, or a bypass value is available
reserve(rf[rd]); //doesn't block, just makes the reservation
```
or, for an asynchronous memory:
```
int<32> data <- reserve(dmem[addr]);
```
As shown above, this implies new semantics for `reserve` where it can also return the value for the associated location.
In this case, `reserve` _will block_ until the  bypass data is available, but the lock is still not considered `acquired` yet at that point.

_N.B._ This seems wrong somehow -> really bypass points should be associated w/ the stage that _acquires_ the lock, not the one that reserves it. If something reserves a lock it doesn't need the bypass value right away by definition.
However, we need to do this since receiving a bypass value doesn't imply that the thread owns the lock yet.

Perhaps a better way of doing this is treating them as "read locks" vs. "write locks".
In which case, you don't need to acquire a lock to do read operations, you can _instead_ receive a bypass value.
But in order to write something you definitely have to reserve a lock.

......

Another question is "do we want bypassing to be explicit, or implicit?"
Implicit is kind of OK I think, but I'm still not sure.

Other examples of (potential) bypassing syntax:


```
int<32> arg1 = bypass(rf[rs1]); //explicitly imply we will read from the fifo if available, otherwise block and then read from mem
                                //no lock acquisition for rf[rs1] in this case, we just get a value
```

W/ more context:
```
int<32> arg1 = bypass(rf[rs1]);
reserve(rf[rd])
---
...
commit(rf[rd], result);
---
block(rf[rd]);
rf[rd] <- result;
release(rf[rd]);
```

What if we have something where we read + write same loc?
```
reserve(rf[rs1]);
---
int<32> arg1 = bypass(rf[rs1]); //either read from the lock if available, else read from memory
int<32> res = arg + 1;
commit(rf[rs1], res);
---
block(rf[rs1]);
rf[rs1] <= res;
release(rf[rs1]);
```

How do we handle bypassing w/ asynchronous memories, since bypassing happens
combinationally, but requests are not?

One option is to say that bypass(x) has the same latency as (x),
but compilation is a little tricky (i.e., we send requests only when the bypass fails,
and then need to send data b/w stages that says whether or not the bypass value was valid).

### Runtime Semantics

The main question becomes how to efficiently provide a bypass interface to locks that allows storing
state associated with the memory.

For now, we will assume we're using per-location locks, with the first suggested implementation
(a small cache of locks that each represent some memory location and are tagged w/ the appropriate address).
Each of these locks, in addition to thread IDs, could store values for the associated location.

The `commit` operation then, at runtime, translates to updating the relevant lock entry. One subtlety here
is that `commit`s can happen even when a lock has only been reserved (i.e. the thread doesn't currently own the lock).
Therefore the lock FIFOs must expose an interface to access any of their entries.
Similarly, bypassing reads must check the _newest_ reservation for a committed value, if there is one.
This only requires that readers must be able to access the _last_ valid entry in the FIFO, rather than search the whole structure.

The above implementation implies that the following snippet:
```
int<32> arg1 = reserve(rf[rs1]);
```
Will have the following runtime semantics:
```
if (can_acquire(rf[rs1]) {
  acquire(rf[rs1])
  arg1 = rf[rs1]
} else {
  reserve(rf[rs1]);
  while (last_committed(rf[rs1]).isValid) {
    block();
  }
  arg1 = last_committed(rf[rs1]).get
}
```

## User Defined Modules

So far we've only described a single modular interface: Memories.
These have implicit `read` and `write` interfaces,
with either combinational or non-combinational latencies.
There are a few approaches we can take to generalize this for user defined modules.

The simplest is to just force them to also have the same interface as memories.
In this way, every module can be called and optionally returns some value.
Right now, modules (as defined with the `pipe` keyword) don't expose any interface,
other than accepting an input value. It would be relatively straightforward to expose
a (potentially optional) return value as well. However, this doesn't allow for
the exposure of multiple methods (`read` vs. `write`).

### Pipeline Methods

In order for our language to really work, every module really needs to have
a single sequential pipeline. If we would like to expose multiple methods, these
should still be exposed as a single input where the method name is a secondary parameter.
In this way, every interaction with a module is really a "request" followed by a "response"
in a later cycle; we could introduce some syntactic sugar or macros to make this nicer to use.

Calling any method of a pipeline would require holding the lock for that module, as if it
were a memory. In order to slightly optimize these method calls, it would be nice
to verify that some methods are combinational so that we could support combinational reads (like register files).

## Speculation

entry <- speculate(var, predval); //adds entry to speculation table
...
var = realval; //update entry in table w/ Y or N
---

```
pipe cpu(pc: spec<int<32>>)[rf, imem, dmem] {
speculate(npc, pc + 4); //adds entry to speculation table
insn <- imem[pc];
call cpu(npc); //speculative call
---
...
npc = //realnpc -- updates spec table w/ correct or not based on predVal for entry
...
verify(pc); //checks that *my pc* isn't speculative *a.k.a* my parent isn't speculative
//if pc isn't spec, this is NOP. if _is_ spec, this checks the spec table
//if wrong, just end execution.
//if right, keep going.
//free entry???
rf[rd] <- result;
}
```

#### Speculation Table

Table is a circular buffer, head points at "oldest" speculation, tail points a "newest".

| predVal     | valid | correctSpec | Parent Entry |
| ----------- | ----- | ----------- | ------------ |
| 7           | 1     | 1           | 0 (self)     |
| 8           | 1     | ?           | 0 ( ^^ )     |
| ?           | 0     | ?           | ?            |


Methods:

```
speculate(predVal T, parent Maybe(idxtyp)): idxtyp
```
Adds a new entry for some prediction. If the current
thread is speculative, then it should pass Some(idxtyp) which
is the source of its own speculation.

```
isCorrect(realVal T, entry idxtyp): bool
update(isCorrect bool, entry idxtyp): void
```
isCorrect allows a thread to check if a given value
matches the predicted value for some entry.
update changes the table to reflect the correctness of that
speculative event (these should be used together).

```
checkSpec(entry idxtyp): Maybe(bool)
```
This lets a child of the speculation check
whether or not some entry is still speculative ->
None means not yet resolve, Some(True) is correct and Some(false) is misspeculated.



Options for "free-ing" entries:

1) Parent updates all child speculations once it determines itself to
 be either "correct" or "incorrect" -> this is how it works in BOOM (except
 it's a broadcast across the pipeline rather than a table update).
 Then parent entry can be freed immediately.

2) Parent updates only its own state. Once no child entries
 refer to that row as parent it can be freed.

------

Different Idea for branch speculation:

If a pipeline has a speculative label (i.e., it is "call"ed speculatively)
then "calling" it creates an entry in a table that tracks
the speculative information related to that call.

e.g.

```
id = speculate(npc = pc + 4);
call cpu(npc); //create entry { specId = id, val = npc, isMispredict = ? }
```
Then, later in the pipeline, the code:
```
update(npc, realnpc, id);
```
updates the relevant entry: {specId = id, isMispredict = (val == realnpc) }.

```
checkSpeculation();
```
is used by a thread to verify that it itself is not speculative.

#### Speculative Call

New Idea: treat forward speculation (i.e., speculation about values used _by this thread_)
differently from speculatively starting new threads.

```
spec pipe cpu(pc: int<32>)[rf, imem, dmem] {

 insn <- imem[pc];
 sid <- spec_call cpu(pc + 4); //creates entry in speculative table && sends data to beginning of pipeline
 ---
 ...
 npc = brunit(op, arg1, arg2, pc);
 speculation_barrier(); //ensure this thread is _not_ speculative by looking at my entry
                        //in the speculation table and free-ing it.
			//problem -> entry could have been created _speculatively_
 verify(npc, sid); //checks speculative table entry -> update bit marking correctness
 //and execute a "call cpu(npc)" if not correct.
 ---
 rf[rd] <- result;
}
```

Idea -> create one entry per speculative call.

The thread that *does the call* is responsible for updating the result in that entry
_and_ for re-executing the call.
The _callee_ is responsible for checking the result of that entry.

Potentially we could have two kinds of checks: non-blocking and blocking.
When the check occurs, the thread will remove the entry from the table
and either a) continue executing, or b) stop execution b/c it was a misspeculation.
Non-blocking checks don't guarantee anything about the state, but can provide
"early termination" optimizations; so any implementation must always have a
blocking check, before it can be considered non-speculative.


Question: if a _speculative_ thread makes a speculative call,
what happens? It creates an entry with its prediction.
If it is a misspeculation, it _may_ update the entry it
creates with "valid", causing its (misspeculative) child to
erroneously continue.

Therefore, we want to track speculative provenance in some way.
ONE way, is requiring speculation of _this thread_ to be resolved
before validating you're child's speculation -> this implies IN-ORDER resolution.
ANOTHER way is to make the speculation table much more complicated.


### Speculation Problems

If we allow threads to speculate while speculative (in an unbounded way)
then we can run into issues where clearly misspeculated state will
never leave the pipeline and can deadlock it.

E.g., Consider a 5-stage pipeline: `F, D, E, M, W`

Our current model for speculation has 3 operations,
2 are exeucted by the parent thread (for some given speculative call):
 1) Spawn child speculatively (spec)
 2) Verify correctness of speculation (verify)

The third is executed by the child:
 3) check speculation table and stop execution if invalid. (check)

In practice, every thread does all 3, since it can be both a parent and a child.


For some implementation, we assign each of these operations to a stage:
```
F -> spec;
E -> check, verify;
```

In this example, we predict the next instruction in the first stage,
and in the third stage we both check whether or not we're speculative
and update the speculative state of any _nested_ speculation that we caused.


Since the `check` operation happens _after_ the `spec` operation, we do
support _nested speculation_ but we also have _misspeculated threads_
executing when we _know_ they're useless. This leads to issues because
the _nesting_ never stops happening; since a thread will always speculate before
it checks its own status.

#### Solutions???

One idea is to (statically) track each stages' speculative state as "unknown" until
otherwise confirmed. Then certain operations can only be executed when the state
is explicitly "maybe speculative" or "not speculative". Then we use the idea of blocking vs. non-blocking
`check` statements to derive, for a given stage, some knowledge about the state.
Once you use a blocking `check` that information is preserved across the next stages.

In this way, we can prevent threads from speculating if they're _definitely misspeculated themselves_.
However, this still runs into a timing issue -> we may _never_ know that a thread is misspeculated
in time since we're statically limiting our speculation state updates to certain stages.


Therefore, another idea is to actually utilize some form of broadcast to
kill multiple stages at once. Since the "call speculation" is ordered,
marking a particular entry as "misspeculative" could automatically
mark _all children of that speculation_ as "misspeculated" immediately
(needs to be visible on _the same cycle_). This + the earlier restriction
solves our earlier problem.

We _could_ implement a proper broadcast that clears all of the FIFOs that we
know must be misspeculative instead of waiting for them all to execute individually
and clean themselves up. But I'm not sure if that's a good idea.


-----------------------


The above ideas end up with a pipeline that has the following checks:

```
F -> nb check, spec
E -> b. check, verify
```

Optionally, we could also add `D -> nb check` or `D -> b check` if we wanted to.


---------------

#### Killing Threads

Threads which are killed may hold _locks_.
Therefore, killing them needs to _release_ those locks.

However, they may also not actually _own_ the locks yet (i.e., they are reserved but not owned).
The current lock API doesn't support _releasing_ unacquired reservations.
This would significantly complicate the lock implementation
(since it no longer has a nice FIFO API, but would need to update internal entries, becoming more of a circular buffer).


One option is _don't_ allow any `check` operations while locks are reserved (but not acquired).
This is....somewhat problematic since it doesn't allow our example implementation
above (the `rf[rd]` lock is reserved but not necessarily acquired in the `E` stage).
With this restriction, you would have to implement it as:

```
F -> nb check, spec
D -> b check (same stage that lock reservation happens in)
E -> verify
```
