# Exceptions
## how they work
 - introduce `except(args)` command that causes a thread to be marked as
   excepting, and provides `args` in a pipeline register for future stages
 - create two end "possibilities", marked with `commit:` and `except:`, where
   you have to be in a commit block to release locks. Except blocks have more
   complex rules:
   + things that are "in scope": `args` and anything from before the earliest
     point an exception can be thrown.
   + all held locks are implicitly aborted on entry
   + you may acquire new locks in the usual way, but (for now) may not throw
     exceptions.
   + all "future" threads have their locks aborted
 - when a thread is marked as excepting, it still has to step through the commit
   block so that previous threads have time to finish releasing their locks
 - As soon as an excepting thread gets to the commit block, it tags a register
   so that future threads will not be allowed to enter (or abort their locks
   immediately)

## What needs to happen:
	1. modify typechecking for excepting pipelines to enforce the above rules.
	2. emit code for excepting pipelines. This will involve translating a lot of the
		implicit stuff about them into explicit things, like the calls to abort, and
		a couple of registers that are used, such as the `args`.
	3. update lock implementations to support an `abort` operation.
	4. write a risc-v processor that implements exceptions

## Partial order on tasks:
```
1 < 2 < 4
3 < 4
```
we could get started on an excepting processor early, but testing seems
important.

## Sequential semantics
the `except(args)` command acts like a jump to the except block, which is
otherwise ignored. This means the except block needs to necessarily contain one
recursive call xor output, which makes sense.

