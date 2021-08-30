# Out of Order Issue Support

The current design of PDL supports a very limited form of out-of-order execution.
Namely, branching paths of the pipeline may execute in parallel.
However, we may want to re-order instructions which are executing on the same path,
e.g. because a later instruction's operands are ready, while those of an earlier instruction are not.

## Key Insights

OoO issue is really tied to an edge in the pipeline graph; therefore,
we need a way to modfiy or replace the normal stage separator (`---`).
We also need to add abstractions for the *re-order buffer* (ROB) that store
the original sequential order of instructions and can be used to re-establish in-order execution.

### ROB

Really, the ROB only needs two functions (names TBD) that:
 - allocate an entry in the ROB (must be done in order).
 - block until entry is at the head of the ROB.

We also expose the entry identifier in the program explicitly,
specifically for use in the Issue priority function (discussed in the next section).

```
id <- rob_start(); //establish intended seq order. Must be in an in-order stage
... //out-of-order issue can happen in here
--- rob_order(id); //re-establish in-order execution
...
rob_free(id); //free the entry in the ROB, must happen after an rob_order. Separated only to make difference betweeen check and modification explicit.
```

The ROB implementation is _very_ simple; it just needs to be a circular buffer
with some ability to take checkpoints and _rollback_ (to support speculative access).
We discuss the speculative features in a later section.

### Issue Queues

For each issue queue (which is assosciated with a stage separator),
the user needs to specify two features:

- a priority function which selects the instruction to issue
- the list of "ready bits"; i.e., all of the `block` statements that the instruction would issue.

The priority function can simply be a comparator, which compares data from two
instructions and returns the one that has higher priority.
Then we can implement this as a `fold` over all of the ready entries in the issue queue.

We cannot allow `block` statements inside out-of-order regions, since this could create deadlock
scenarios and thus they must be lifted to the issue queue.
Instead of just a list of `block` statements, this must be a list of _conditional_ `block`s,
since not all instructions will actively block on all possible locks.

Here is a potential syntax:

```
--- issue when ( needrs1 => block(rf[rs1]), needrs2 => block(rf[rs2]) )
    ordered by (f(l, r) => { return l; })
```

Note that the ordering function (`f` in this case) can refer to the `id` returned
from the ROB and therefore can impose a "youngest first" or "oldest first" ordering, etc.
(Assuming we provide comparators for the opaque ROB `id`s).

### Implementation

Issue queues replace the existing FIFO abstraction with a Priority Queue.
We can fold the given comparator function (a combinational function) over all "ready"
entries to choose the next instruction to issue.

Additionally, enqueuing an instruction into the queue immediately evaluates the "ready"
functions, setting the ready bits appropriately. Every cycle, the "not ready" bits in
each entry will be re-evaluated. This implies that the locks will need _many_ ports
to support the concurrent block checks.


## Full List of Restrictions:

- Out of Order Issue causes the pipeline to enter an "out-of-order" state
that is only recoverable (i.e., can become re-ordered) via the ROB API.
- Block statements are not allowed in "out-of-order" regions.
- ROB entries can only be allocated "in-order".

## Speculation

Speculation imposes more restrictions than the above.
Specifically, our Issue Queues and Re-Order buffers need to be able to
be rolled back, based on particular checkpoints.

### Re-Order Buffer

The reorder buffer is always ordered and thus has a natural checkpoint mechanism.
When threads _verify_ speculation, if it was _mispredicted_, it will need to kill _all younger threads_,
and the ROB will need to be reset to (essentially) empty. More particularly, it rollsback
until the _verifying_ thread is at the head of the ROB.

Restriction Summary:

- `verify` must take place before a thread has released its ROB entry (or before allocating one)
- Threads must issue a `spec_check` before allocating or releasing an ROB entry.

### Issue Queues

Like with `block` statements, any `spec_barrier` statements inside
of an out-of-order region could cause deadlock problems.
If the instruction that's going to _resolve_ speculation for another is scheduled behind
one that is blocking on it, we'll have a deadlock.

The question is: how do we deal with this? More specifically, how do we imagine architectures
resolving branch instructions?

Ideas:
1. Treat the `spec_barrier()` as another ready bit in the issue queue, s.t. branches issue in-order.
2. Compute on branches out-of-order, wait for instruction retirement to issue verification.

The latter is automatically supportable, so if we allow the former then we should be good.
The only difference between speculation checks and blocks is that the thread may be killed in some
scenarios, so when the speculation result is in we need to decide whether to issue or kill based on the result
(other blocking checks cannot result in killed threads).

### Killing Speculative Threads

For _lock_ operations, we will require that any potentially modifying instructions (reserve, read/write)
are guarded by a `spec_check` so that they don't update lock state after misspeculating.
This does not depend on ordering in any way and will work for instructions that are issued OoO.

However, we will also need to be able to purge misspeculated instructions from the issue queue as
they are now potentially blocking on essentially invalid reservation handles and may deadlock.
It is unclear how we should specify this logic, if it should be customizable, etc.


## Other Outstanding Issues

Memory operations in typical ISAs are still hard to support efficiently with this API.
In particular, something like the instruction:

```
ld rd, 4(rs1);
```
Requires the following locks:
```
rf[rs1] (R),
rf[rd]  (W),
dmem[4 + rs1] (R)
```

The primary issue is that reservations need to happen in-order,
but we don't know the location of `dmem` to reserve until some computation has happened.
A simple proposal to address this is allowing reservations _without addresses_, which can
be specified later:

```
dlock <- reserve(dmem[?], R); //must be in-order
...
res_address(dlock, memaddr); // can be out-of-order, sets address, used before block()
```

Unfortunately, we still have the problem that we'd
like to issue loads out-of-order, but the `block` statement can't
happen until after accessing the execute unit.
That means we really want two sets of "ready" bits: those for
the address computation; and those for the memory access.
Right now we're not abstracting this decomposition into micro-ops and thus it isn't really possible.

We need to think about this use case more, and how to allow OoO Loads and Stores without:
 - a dedicated memory address computation unit
 - in-order execution for any part of the load/store

