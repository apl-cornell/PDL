# Exceptions
## how they work
 - Introduce `except(args)` command that causes a thread to be marked as
   excepting, and provides `args` in a pipeline register for future stages
 - Create two end "possibilities", marked with `commit:` and `except:`, where
   you have to be in a commit block to release locks. Except blocks have more
   complex rules:
   + Things that are "in scope": `args` and anything from before the earliest
     point an exception can be thrown.
     -- can always start with _just `args`_ being in scope for simplicity.
   + All held locks are implicitly aborted on entry.
   + You may acquire new locks in the usual way, but (for now) may not throw
     exceptions.
   + All "future" threads have their locks aborted.
     -- The "abort" method of a lock should essentially throw out _all_ non-committed updates / locks.
 - When a thread is marked as excepting, it still has to step through the commit
   block so that previous threads have time to finish releasing their locks
 - As soon as an excepting thread gets to the commit block, it tags a register
   so that future threads will not be allowed to enter (essentially, we need to ensure that
   _every_ stage in the early part of the pipeline is killed and doesn't enter the "excepting" or "committing"
   parts of the pipeline --- the actual mechanism to do that is up for debate).

## What needs to happen:
	1. Modify typechecking for excepting pipelines to enforce the above rules.
	2. Emit code for excepting pipelines. This will involve translating a lot of the
		implicit stuff about them into explicit things, like the calls to abort, and
		a couple of registers that are used, such as the `args`.
                It also involves the tricky bits of making sure early stages are killed when an exception is handled.
	3. Update lock implementations to support an `abort` operation. This operation should throw
                out all pending updates (reads and writes) -- some lock implementations may need to
		be modified to delay writes until "release" is called to support this.
	4. Implement a risc-v processor that uses exceptions.

## Partial order on tasks:
```
1 < 2 < 4
3 < 4
```
we could get started on an excepting processor early, but testing seems important.

## Sequential semantics
The `except(args)` command acts like a jump to the except block, which is
otherwise ignored. This means the except block needs to necessarily contain one
recursive call xor output, which makes sense.

