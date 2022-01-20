# Speculation

PDL currently allows for a limited form of speculation that can be used _only_ for branch prediction. In the future, we plan to support more general forms of value speculation.

Unlike most speculative implementations, when misspeculation is discovered this signal is _not_ broadcasted to any part of the circuit. Instead, it updates a table which contains the list of ongoing speculations and their status. When a speculative prediction is _verified_ to be accurate, this table must also be updated with that information. Speculatively executing threads must read the status of this table before executing effectful operations, such as writing to a memory or making their own speculative predictions.

In this way, handling speculation divides responsibility between the _parent_ thread (which makes and then verifies speculative predictions) and the _child_ thread (which checks the result of verification). Since the only speculative operation we support is spawning a child thread (via the `call` operation); there is a 1-to-1 mapping bettwen parent speculative events and child threads.


# Operations

As mentioned above, the operations supported by the Speculation Table are divided into those
used by the _Parent_, and those used by the _Child_. In practice, threads can
act as both a _parent_ and a _child_ simultaneously; since any thread that was started speculatively will be a child, and any thread making speculative predictions will be a parent.


## Parent Ops

- **Make Prediction**: Allocates a new entry in the speculation table, sets its status to Invalid (i.e., an unresolved prediction) and return the index pointing to that entry. We _don't_ need to store the values that were predicted. Since these values don't need to be communicated between threads, they can just be stored in pipeline registers.

- **Invalidate**(sid): Updates the speculation status of the prediction tracked with id `sid` to "misspeculated". This can be used to unconditionally mark a prediction as incorrect - it is safe to do this even if the prediction may have been correct. This invalidation marks _all speculation entries as new as `sid`_ as misspeculated. In this way, misspeculated threads do not need to update the speculative status of their child threads; it only takes 1 cycle to invalidate all levels of nested speculation derived from the original prediction.

- **Update**(sid, newpred): Updates the prediction to the `newpred` value if it differs from the original prediction, killing the speculative thread in the process.

- **Verify**(sid, real vals): The parent thread checks that the speculated values (stored in pipeline registers)  match the `real vals`; if they do then it updates the speculation table entry, `sid`, with Valid, else it updates that entry with Misspeculated (following the same 'nesting' protocol as described in Invalidate).

## Child Ops

- **Nonblocking Check**: The child thread looks up the entry associated with its execution in the table to ensure that it isn't explicitly "misspeculated". In other words, if the speculative status of this thread is unknown, the thread _may_ continue to execute; if it has already been set to "Valid" or "Misspeculated" then the thread _deallocates_ the speculation entry and either continues execution, or kills its execution (according to a `kill` protocol that we'll discuss separately), respectively.

- **BlockingCheck**: The child executes the same behaviors as the Nonblocking Check case, except it cannot procede if the speculation status is unknown; it blocks until a cycle on which this is known.

# Speculation Rules

Depending on the speculative status of a thread, it may or may not execute certain operations.
Additionally, we need to guarantee that all speculative predictions are eventually resolved,
and misspeculated threads are re-executed.

- `call` or speculative `call` operations: A thread may _not_ spawn another thread (speculatively or not), if it is _definitely_ misspeculated. Any stage which executes these operations must unequivocally be non-speculative or must have a non-blocking speculation check.

- `release write lock` and `atomic/unlocked write` operations: Threads may not release write locks or commit unlocked writes until they are non-speculative, since releasing write locks is an irreversible operation.

- `output` operations: If a pipeline eventually produces a value with an `output` command (as opposed to non-termination); it must be in an explicitly non-speculative state to do so.

- `verify` operations: To verify that a speculative prediction was made correctly, the thread itself must _definitely_ be non-speculative (assured via a non-blocking speculation check).
This effectively implies that speculative predictions are resolved _in order_, since the speculation that created thread _t1_ must be resolved before the speculative thread _t2_ created by _t1_.


# Kill Protocol

When a thread discovers that it has been misspeculated, it needs to terminate its own execution.
This involves the following operations:

1. Flush its state from pipeline registers (dequeing inputs but not sending them anywhere, in our implementation).
2. Free its entry in the speculation table.

This protocol is _so simple_ thanks to the fact that nested speculation is handled in one fell-swoop.
A killed thread never needs to notify _its_ children about this fact.

The only current bug in this is tracked in an issue; if the thread is in a pipeline branch then there is no
way to dequeue the appropriate entry in the control queue that the join point will read.

# Extensions To Write Ops

1. Threads create checkpoints after reserving locks, but before any other thread can reserve new ones
(essentially this can be viewed as simultaneous).
2. When a thread marks a child as 'misspeculated' it executes the 'rollback' operation on the relevant
memories to ensure that any writes ordered after its own are undone.
3. When a thread commits (releases) its writes, (which can only be done non-speculatively) the
state used to represent the checkpoint can be freed. It is possible that this may be done earlier
(e.g., when child speculation status is verified).



## Checkpoint and RollBack

The current implementation of checkpoint and rollback works as follows:

1. Only 1 checkpoint per memory may be made and it must be made in the same cycle
as the `end(mem)` statement. The semantics are that it captures the effects of all reserves
made in the same cycle.
2. Checkpoints may be made conditionally; the conditions under which they are made must
imply all of the conditions under which speculative threads are validated (`verify`, `update` and `invalidate` statements)
after the checkpoint stage. The condition must also be implied by all paths that
speculate (otherwise some thread may speculate but not make a checkpoint) after the checkpoint.
3. A checkpoint is only necessary if a stage speculatively reserves (any op)
or atomically writes a memory. In this case, if no checkpoint is ever made it is an error.
4. *Checkpoints are inserted automatically by the compiler*.
5. Rollback operations are automatically inserted at the relevant `verify`, `update`, and `invalidate` statements:
 - `verify` conditionally rollsback the lock state and releases checkpoint
 - `update` rollsback the lock state but _does not_ release the checkpoint
 - `invalidate` rollsback the lock state and releases the checkpoint
6. It is an error if the supplied lock implementation doesn't implement the checkpoint or rollback interface.

# Nested Speculation

We would like to enable speculative calls into sub pipelines and properly handling speculative
updates to any state _those_ pipelines write. This describes the protocol for implementing this,
and the restrictions on use.


## Protocol

We need to communicate the speculation identifier that the outer pipeline generates to the inner
pipeline so that it can track the speculative status of requests it is handling.

This means requests to inner pipelines now have the signature:
`req(arg1, arg2,...., argn, Maybe<specid>)`

If the specid is a `None` then the request is not speculative.

### Child Threads

Threads _inside nested pipelines_ are treated basicaly like _child threads_ from the normal speculation protocol.
This means that they are not responsible for cleaning up speculative state at all; they are just responsible
for checking their status before accessing locks or writing data, and terminating their own execution
when they find that they have misspeculated.

When a child thread _does check its speculative status_ via `spec_barrier()` or `spec_check()` it
normally `frees` the associated entry in the speculation table. However, the speculation table
is associated with the _outer pipeline_, which also has some thread that plans to `free` the same entry.

#### Spec Table Freeing Solutions:

1. Each entry has a reference counter, which is decremented by each "free-er". Once this reaches 0,
the speculation table can simply free the entry.This must also be incremented whenever
a speculative thread calls into another stateful pipeline. The increment part seems the most worrying to me.

2. Keep a local table for the inner pipeline; it adds an entry whenever a speculative request is made,
and whenever that speculative status is updates the local table as well. This could lead to extra
area overhead, but likely a much simpler design.

I personally like (2) better, as it is simpler to implement, and low-level optimizations
could likely negate the overhead.

### Parent Threads

Parent threads normally just update the status of the pipeline's speculation table for their request.
Additionally, they need to notify inner pipelines of the fact that a speculative thread has been killed;
then the inner pipeline can execute a "misspeculated" protocol:

#### Misspeculated Protocol

1. Update status of local speculation table (see option 2 for freeing solutions),
including all speculation events _ordered after_ the given identifier.
2. Rollback status of local locks to checkpoint ordered _before_ the given speculation event.
This second point means that each thread needs to know how far (or if) to rollback its memories.
Complicated. Currently thinking of a different protocol.

### Parent Threads Alternate

For checkpoints, we could treat subpipelines _just like normal memories_ and force parent
threads to take checkpoints whenever they own the lock for a subpipeline and could have speculative children.
The definition of `checkpoint` for pipelines would be to create checkpoints for _all of its memories and subpipelines, recursively_,
returning a single checkpoint identifier to the parent pipeline.

Then, the "misspeculated protocol" is _no different_ than when rolling back locks normally (i.e., the non-speculative parent
notifies the subpipeline, the speculative child does not need to do anything).

The downside of this approach is that it could cause many checkpoints to be made (e.g. even when
a child thread isn't going to actually access the given subpipeline); however, this means that we can instantiate
less efficient or smaller checkpoints and offset the cost of calling the checkpoint function frequently.

I *prefer this approach to checkpointing* because it requires less modification to the existing protocol, it simply
requires that we define `checkpoint` and `rollback` operations on pipelines themselves and then otherwise treat them *just like normal locks*.
