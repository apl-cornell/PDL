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

