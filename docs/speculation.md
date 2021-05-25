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

- **Verify**(sid, real vals): The parent thread checks that the speculated values (stored in pipeline registers)  match the `real vals`; if they do then it updates the speculation table entry, `sid`, with Valid, else it updates that entry with Misspeculated (following the same 'nesting' protocol as described in Invalidate).

## Child Ops

- **Nonblocking Check**: The child thread looks up the entry associated with its execution in the table to ensure that it isn't explicitly "misspeculated". In other words, if the speculative status of this thread is unknown, the thread _may_ continue to execute; if it has already been set to "Valid" or "Misspeculated" then the thread _deallocates_ the speculation entry and either continues execution, or kills its execution (according to a `kill` protocol that we'll discuss separately), respectively.

- **BlockingCheck**: The child executes the same behaviors as the Nonblocking Check case, except it cannot procede if the speculation status is unknown; it blocks until a cycle on which this is known.

# Speculation Rules

Depending on the speculative status of a thread, it may or may not execute certain operations.
Additionally, we need to guarantee that all speculative predictions are eventually resolved,
and misspeculated threads are re-executed.

- `call` or speculative `call` operations: A thread may _not_ spawn another thread (speculatively or not), if it is _definitely_ misspeculated. Any stage which executes these operations must unequivocally be non-speculative or must have a non-blocking speculation check.

- `write lock` operations: _Current Version ONLY_. A potentially speculative thread may **not** execute any _write_ lock operations and therefore cannot mutate any shared memory. We allow read lock acquisition (for now) to allow for behavior that does not mutate shared state; reads may happen speculatively.

- `output` operations: If a pipeline eventually produces a value with an `output` command (as opposed to non-termination); it must be in an explicitly non-speculative state to do so.

- `verify` operations: To verify that a speculative prediction was made correctly, the thread itself must _definitely_ be non-speculative (assured via a non-blocking speculation check).
This effectively implies that speculative predictions are resolved _in order_, since the speculation that created thread _t1_ must be resolved before the speculative thread _t2_ created by _t1_.


# Kill Protocol

When a thread discovers that it has been misspeculated, it needs to terminate its own execution.
This involves the following operations:

1. Flush its state from pipeline registers (dequeing inputs but not sending them anywhere, in our implementation).
2. Release all held locks (this can cause problems if our implementation can't support releasing enough locks concurrently).

This protocol is _so simple_ thanks to the fact that nested speculation is handled in one fell-swoop.
A killed thread never needs to notify _its_ children about this fact.


# Extensions To Write Ops

In the future, to support speculative writes (and speculative non-interference of microarchitectural state),
we need the lock API and the speculation API to interact.
Specifically, this involves locks supporting `checkpoint`, and `rollback` operations, in addition to `commit`.
While we will not include the full details here yet, the simplest explanation for how this works is:

1. Threads create checkpoints after reserving locks, but before any other thread can reserve new ones
(essentially this can be viewed as simultaneous).
2. When a thread marks a child as 'misspeculated' it executes the 'rollback' operation on the relevant
memories to ensure that any writes ordered after its own are undone.
3. When a thread commits (releases) its writes, (which can only be done non-speculatively) the
state used to represent the checkpoint can be freed. It is possible that this may be done earlier
(e.g., when child speculation status is verified).


