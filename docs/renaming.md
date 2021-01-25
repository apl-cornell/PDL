# Renaming Abstraction

Renaming may be a more general form of the lock abstraction from the early version of PDL,
which also may be able to provide an interface over many different data dependency breaking violations.

## Operations

The notion of "explicit renaming" in hardware architecture involves the following operations:

1. Reading the current physical name for an architectural location
2. Allocating a new physical name for an architectural location
3. Checking data validity, given a name
4. Reading data, given a name
5. Writing data, given a name
6. Freeing an old physical name, once it is no longer in use


"Explicit renaming" involves maintaining a map from architectural names to physical names;
this abstraction requires a mapping function but is otherwise flexible in terms of implementation.
Names may refer to unified register files, reservation stations or other locations.
This makes it somewhat attractive as an abstraction.



## Restrictions

Like locking in our original language, there are restrictions on the ordering
of these operations necessary for correct execution:

1. Reading names and allocating new names must occur in thread order.
2. A thread should always read names before allocating new ones (i.e., a thread should read only old names, not ones it allocates)
3. Only "new names" can be used as write targets
4. Only "old names" can be used as read targets
5. Data can only be read if the name refers to valid data. (i.e., can only execute `read(name)` iff `isValid(name)`)
6. Once a name is freed, it cannot be used again (until allocated). One way to enforce this is to ensure that (1) `free`-ing a name prevents all reads + write by this thread and (2) `frees` happen in thread order.

## Relationship to Locking

Our locking idea supports the following operations:

1. Reserve a lock for an architectural location
2. Check if a lock reservation is "owned"
3. Release a lock reservation

The restrictions on locks are:

1. Locks must be reserved in thread order.
2. Locks must be "owned" before a thread can read from or write to that location.
3. Locks must be released, but only once owned.


There are some parallels between these abstractions.

### Reservation

Lock reservation returns a unique lock identifier, which names the reservation.
This is analogous to allocating a new name for a location.
The main differences are that name allocation must also return the _old_ name so
that the old name can be `freed` later (it is possible to hide this from the programmer
and just have the internals of the abstraction keep track of this association);
and that locks are required for both reads and writes - whereas name allocation
is only necessary for writes.

### Blocking

Locks require blocking all operations until they have been acquired (i.e., all
other threads have released their locks that alias the same location).
With renaming, writes are never blocked once a name for them has been allocated,
and reads are blocked until data has been written.

### Release

Lock release, once owned, indicates that a location will
never be used by that thread again.
We need both read and write locks to ensure that
writes don't run ahead of reads.

With renaming, only _old names_ need to be released,
indicating they will never be used again (read or written).
This is basically analogous to freeing locks that are used for writes,
since typically no reads happen after w/in a single thread.

Andrew: I'm confused about what ensures that there is just one
canonical value for a given renamed resource ultimately. Weren't the
locks providing some functionality there?


### Translation

We can implement the Rename API using locks _and_ vice versa.

#### Rename API implemented (=>) via Lock API

1. read name => reserve
2. allocate name => acquire (i.e., reserve;block)
3. check data validity => block
4. read data => normal memory read
5. write data => normal memory write
6. free name => release

With a slightly different implementation we could more naturally
support similar performance by allowing the locks themselves to support
reads and writes; this would allow write to not have to block and they
would only be committed to the real memory on release.

#### Lock API implemented (=>) via Rename API

1. reserve => allocate name
2. block => check data validity
3. release => free

Since locks don't have any notion of "reads" vs. "writes"
we would have to allocate a fresh name for every location
that gets accessed. This would obviously not be ideal, but
that's the limitation of translating from the less expressive
to more expressive API. Similarly, we have to "check data validity"
on both reads and writes, using the "old name" from each allocation.

### Edge Cases

What happens if a thread wants to write two locations in the same memory?
If they end up being the same location, whose responsibility is it to de-alias them?
With reads this is less important in the Renaming API since they'll just get the same
name. Ideally, I'd say that the rename semantics provide some clear ordering s.t.,
in the event of an alias, one of the writes is ordered before the other. This lets the user
de-alias themselves, or just ignore that and have some behavior that doesn't really do anything.

Andrew: are there any realistic cases where this happens?

## A NEW Lock API

Let's imagine a new lock API that has the _same_ expressivity as the Rename API;
in other words, it differentiates _reads_ and _writes_.

1. reserve(w|r) | same old reserve, but you get to specify the type of operations this lock allows you to do
2. block        | establishes that the operations this lock lets you do are do-able
3. release      | indicates you're done w/ the lock

This new API has a different set of restrictions than the old one:

1. reserves must happen in thread order, regardless of operation type
2. block is only required for reads
3. write locks must be released in thread order
4. all read locks must be released _before_ write locks the thread holds

We can translate this API into the rename API as follows:

1. reserve(r) => read physical name
2. reserve(w) => allocate new physical name
3. block(r) => check data valid
4. block(w) => nop
5. release(r) => nop -> implies static checks but no runtime behavior
6. release(w) => free physical name

Reads and writes must also obviously go through this API since
the "names" returned need to be interpreted in _some_ way.
Therefore reads and writes don't use the original address, but the lock identifier
returned from the API (a.k.a. the underlying name).

## Interacting With Speculation

An interesting question is how both our high level lock API and the low level rename API interact w/ speculation
based on how we know speculative architectures need to be built.

The lock API now needs to come with a dynamic identifier, which the requester uses to indicate
whether or not it is speculative.
In this way, reading / allocating physical names can happen speculatively and relies on the
naming layer to track speculative state.
Therefore, in order to _resolve_ speculation, we need to make further use of the "free" API call.
We can imagine "free" being split into "commit" and "abort" which the compiler must ensure are
only used when the thread is either _nonspeculative_ or _misspeculated_, respectively.

"commit" has the behavior of finalizing writes or reads; in the latter case this is necessary
for modules that might update internal state to track ordering of reads w.r.t writes.
"abort" takes a name and 'rollsback' the state affected by
that operation (including internal state, not just the memory). Likely, rollback will need to rollback
everything "newer" than that point - or we may need multiple versions (one which rolls it all back, one which doesn't).

---

### New Idea: Speculation + Checkpointing

The aforementioned commit/abort idea has some issues:
1) All read and write operations that are "reserved" need to be either committed or aborted explicitly.
   This means that, when threads "die" they need to access some set of lock modules in stages where they otherwise
   wouldn't need to. This can lead to some resource conflicts (i.e., multiple stages trying to modify lock state).
2) We also can't control the order that these "aborts" happen in easily without running into deadlock scenarios.
   Therefore, the lock modules would need to accept out of order aborts, while blocking conflicting non-speculative
   allocations and reads until the aborts are all completed.

Those two problems would require very complicated lock module implementations.

_Instead_ we add speculation-related operations to the lock interface: _checkpoint_, _resolve(id, bool)_

- _checkpoint()_: Creates a logical snapshot to which the current state can be reverted. This need
not be an actual snapshot (copy); the implementation may instead be a pointer into a reversible history.
This returns an _id_ to the caller that can be used for the other speculation methods.
- _resolve(id, bool)_: This bookends the checkpoint calls, either reverting to the checkpoint or freeing it.
If argument is _false_ then this reverts the lock state to the checkpoint associated with ID, _id_. This is used
by threads when they determine that the speculation _they_ started was incorrect. If their speculation was _correct_
then they call with a _true_ argument, which indicates the checkpoint associated with ID, _id_, will never be needed.


This solves the eariler issues since only threads that _start_ speculation need to create or free checkpoints.
If a _speculative thread_ creates a checkpoint speculatively and its parent later determines it to be misspeculative,
the parent's _resolve(id, false)_ operation will automatically free the associated checkpoint too.

### Restrictions

The natural question now is, what are the ordering and calling restrictions on these new operations?

_checkpoint_

- Calling thread must not be _explicitly misspeculated_
(i.e., it may be speculative, or non speculative but if it is speculative it must be wrapped with a non-blocking speculative check)
- After creating a checkpoint, a thread may not _reserve_ any more locks, since reverting to that checkpoint may revert it _too far_
(i.e., undo some of its reservations).
- Checkpoints must be inside a _reservation region_ so that later threads cannot make reservations before this one creates its
checkpoint.
- Checkpoint statements must be paired with a later _resolve_ statement, to avoid resource starvation. This is similar
to the requirement that locks must eventually be freed.

It's even possible that we could automatically associate the "end()" statements, which close reservation regions, with
"checkpoint" creation, assuming we could also infer where to place _resolve_ statements.

_resolve_

- Calling thread must explicitly be _non-speculative_. I'm not 100% sure this is necessary, but what we do
need is in-order resolution (since it makes no sense to 'commit' some checkpoint that will eventually be reverted).
This goes along with in-order speculation resolution. We may be able to relax this later, but it simplifies everything for now.
- Resolution may happen _before_ or _after_ freeing locks (a.k.a. commit) because those free operations are associated with
reservations made before the checkpoint (i.e., they're going to be used whether or not we revert back to the checkpoint).
Although, in practice, we should always be able to resolve this before freeing write locks (not necessarily read locks),
since we require in-order _free_ operations w/ the R/W API.

Using _checkpoint_ and _resolve_ is necessary whenever reservations may be made speculatively. If the stages containing
the relevant lock operations _may be speculative_ then checkpoints must also be used when making reservations for this memory.
This ensures that a checkpoint will always exist, should a rollback due to misspeculation be necessary.

