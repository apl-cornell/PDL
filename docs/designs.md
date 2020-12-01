# Design Decisions

This file documents a number of current design decisions
(in particular those related to performance) and how we may want to revisit them in the future.

## Resource Usage

Every module and memory has some limited ability to handle requests in any given cycle.
This is the problem that [Dahlia](https://rachitnigam.com/publication/dahlia/) addresses with its type system.
We could certainly address this problem using a *much* simplified version of their type system.
However, Dahlia doesn't concern itself too much with what kinds of memories are actually available
(you just have to declare them properly). We need to actually represent these memories and connect
the requests to particular ports.

Currently, our module API (basically the same for asynchronous memories as well) is the following (using BSV syntax):

```
interface module;
    method Action#Value(Handle) req(args);
    method Bool checkHandle(handle);
    method Resp peekResp();
    method Action resp();
endinterface
```

Well-behaved modules invoke them using the following sequence:
```
rule r1;
  let handle <- mod.req(arg1, arg2...);
  //store this handle somewhere for future reference
endrule

...

let respval <- mod.peekResp(); //wire up the response to a variable
//only execute this rule once the response is for us
rule rule2(mod.checkHandle(handle)); //use the stored handle and make sure its valid too
  mod.resp();
endrule
```

### Ports

Notably, the above code doesn't make any reference to ports, and therefore only one rule calling
the `resp` or `req` methods can execute per cycle. This is totally OK and not an issue.
What *is* an issue, is that some rule may want to execute _two or more requests simultaneously_.
In that case, the generated code won't be valid since those rules can never be executed.

Right now, we can simply add a pass that disallows such behaviors by applying Dahlia's typesystem
and assuming all modules can support only a single request per cycle.
However, especially for memories (and potentially for other modules as well)
we should eventually have some mechanisms for 
1) specifying declarations with more ports
2) automatically annotating accesses with port numbers
3) generating or providing implementations of the given declarations

In particular, number (2) raises some interesting issues, since we can easily
make such an assignment, but finding an optimal one (that minimizes leaving ports idle) is challenging.

### Current implementation
There are no checks that prevent bad code (i.e., trying to execute multiple `req` or `resp`
methods per cycle) from being generated.

### Response Ordering

Memories (and submodules) are guaranteed to receive conflicting requests in the
correct order - this is guaranteed by our locking mechanisms.
However, there is no guarantee that _non-conflicting_ requests are sent in any
particular order. This can lead to deadlock scenarios caused by scheduling problems.

Consider this sequence of stages (locks omitted):

```
x <- m[a];
---
y <- m[b];
```
In this case, let's imagine that the memory module can enqueue up only a single response.
Now, some thread requests `m[a]` and cannot execute stage 2 until it receives
the response. However, it also _can't execute stage 2_ until the memory module is ready
to receive another response. Currently, this module can't both receive a request and send
a response in the same cycle if it is full, so this deadlocks.

This problem _seems_ to be solved by increasing the memory's queue size s.t. it is never full.
However, it is difficult (maybe reasonable though) to determine that max possible number of outstanding requests in general.

Another possibility in the prior example is that the _next threads_ all fill up
the memory's request queue before the first thread can execute stage 2. Even if the thread in stage 2 can
dequeue its data (e.g., by separating dequeue from stage execution into two separate rules),
then its possible that it can't execute b/c it is blocking the next threads from
dequeing, but there are no spaces left in the memory queue to make a request.

The last problem related to this is that, since requests are made out-of-order (if they don't conflict)
then responses will be out-of-order and so the response 'queue' can't be a queue. It needs to
be able to dequeue items in any order. The current implementation supports this out-of-order
response by keeping a window of pending requests and valid bits (for whether or not they've been dequeued).

#### Potential Solutions

The Dynamically scheduled HLS folks generate LSQs with a single port
for each access in the design and those get serviced via some sort of arbiter (e.g., round-robin).
This would solve the problem, but is potentially a heavyweight design.

Another option is to disallow this behavior and align it with the original
port question: one thread may only access each port of a memory once.
This sounds restrictive, but is potentially reasonable and allows the designer
to "solve" the scheduling problem themselves by deciding which stages use which ports.
I don't mind this at all since modern processors tend to access main memories (or caches)
a small number of times through a small number of ports.

### Current implementation

The current implementation provides no real guarantees here
and can lead to deadlock due to resource contention, but does allow for
out-of-order response retrieval in order to alleviate this a bit.
As long as queues in the system are balanced (i.e. the memory queue is _never full_)
then deadlock will not occur.


## Locks

### Lock Ports
The dynamic locks that we generate also suffer from this port assignment problem.
Locks that are per-memory (i.e., accessed with statements like `acquire(rf)`) work OK
because we won't generate multiple requests per rule.

However, for address-specific locks (i.e., accessed via `acquire(rf[addr])`) we are likely to
run into this issue since we need to send multiple requests to the same module.

A preliminary option here is to just disallow multiple lock acquisitions in a single cycle.
A second reasonable option is to support some other static number (like 2) of acquisitions per cycle
instead of 1.

We still need some idea of what to do in the case that there are *many* locations acquired in a cycle -
perhaps we could implement an imprecise runtime that locks ranges instead of locations.
This should never deadlock (since the thread which has the "extra" locations acquired
can always progress) but may lead to unnecessary delay.

### Current Implementation

Only works with a single access per cycle. Nothing prevents generating bad code that
tries to acquire multiple addresses per cycle.

## Address-Specific Lock Implementation


### Current Implementation

It is a fully-associative structure which holds a subset of the potential addresses.
Locks can only be acquired if there is a free space in the structure to store the lock information.

## Implicit Queues

Right now, we generate stages of execution which are connected with
abstract FIFOs. In practice we may want these FIFOs to have specific sizes
or other properties (like being able to dequeue and enqueue on the same cycle
even if the FIFO is full).

Furthermore, there are instances where we KNOW that some queues must be larger
to provide potential for a 1 IPC throughput. One example are `if` statements:

```
if (cond) {
  ---
  s1
} else {
  ---
  s2
}
```

In this case there are 3 queues generated starting at the conditional:
`cond -> s1`, `cond -> s2` and `cond -> joinpoint`.

The `joinpoint` queue tells future stages which of the two branches to read inputs from.
This queue should be deeper than the other two so that the condition stage doesn't block.
If this queue is too small (say 1 element), then only 1 branch can execute at a time and we lose
some of the potential for parallelism.

### Current Implementation

Currently, we generate code for unsized queues (i.e., that can be decided during
synthesis or simulation) and we don't carry annotations
or other information around that would tell us how big to make them.

