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

if (test) {
 ---
 acquire(rf[rs1]);
} else {
 ---
 acquire(rf[rs2]);
}

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
TODO

### Runtime Semantics

TODO

## User Defined Modules

TODO

## Speculation

TODO