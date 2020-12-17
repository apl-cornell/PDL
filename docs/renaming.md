# Renaming Abstraction

Renaming may be a more general form of the lock abstraction from the early version of PDL,
which also may be able to provide an interface over lots of different data dependency breaking violations.

## Operations

The notion of "explicit renaming" in hardware architecture involves the following operations:

1. Reading the current physical name for an architectural location
2. Allocating a new physical name for an architectural location
3. Checking data validity, given a name
4. Reading data, given a name
5. Writing data, given a name
6. Free-ing an old physcial name, once it is no longer in use


"Explicit renaming" involves maintaining a map from architectural names to physical names;
this abstraction requires a mapping function but is otherwise flexible in terms of implementation.
Names may refer to unified register files, reservation stations or other locations.
This makes it somewhat attractive as an abstraction.



## Restrictions

Like locking in our original language there are restrictions on the ordering
of these operations necessary for correct execution:

1. Reading names and allocating new names must occur in thread order.
2. A thread should always read names before allocating new ones (i.e., thread should read only old names, not ones it allocates)
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
This is analagous to allocating a new name for a location.
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
This is basically analagous to freeing locks that are used for writes,
since typically no reads happen after w/in a single thread.


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