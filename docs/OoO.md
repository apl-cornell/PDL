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

Speculation imposes more restrictions. TODO


## Outstanding Issues

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

