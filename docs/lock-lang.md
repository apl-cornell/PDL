# Language Level Locking

This document covers how locks are presented in the language and the static analyses
we're going to use to supplement typechecking. This document *does not cover*
the _runtime implementation_ of locks.

## Locks as a Transaction Implementation

In transactions, conflicts are found via interesctions of _read_ and _write_ sets.
We can view lock acquisition in our language similarly: each thread will acquire some set of locks
associated with the locations that it is reading and writing.

We need to ensure that each thread gets to acquire _all_ of the locks in its read/write set before
another thread starts to acquire locks in *its* read/write set.

The following program snippet shows where this can go wrong:

```
acquire(rf[x]);
---
acquire(rf[y]);
```

We can get an execution trace that looks like: `acq(rf[0], t0), acq(rf[1], t1), acq(rf[1], t0)`
where `x = 0, y = 1` in `t0` and `x = 1, y = ?` in `t1`.

In this case, we see a deadlock happen; in other cases we might actually not deadlock but
have writes be missed or have an incorrectly linearized execution.

### Ensuring Atomcity of Transaction Set Acquisition

We can introduce _another layer of locking_ to ensure that threads don't interleave 
their locking operations. This lock must be acquired in order to _reserve_ address specific
locks.

```
start(rf);
acquire(rf[x]);
---
acquire(rf[y]);
end(rf);
```
