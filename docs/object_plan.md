# Questions to answer:
  - should we move the translation to IMem from CMem earlier?
  - how much recomputation can be avoided by a refactor?
  - is there any refactoring to integrate into this to make speculation
    implementation easier?
  - should the extra arg/ret added on to lock stuff be ANOTHER annotation? or
    should it be part of the type?
	+ split read and write into two operations. The lock component and the
      memory component. These can be two (somewhat) separate method calls

# Things to do:
- #### Unify locks and extern objects (easy)
  + change some stuff around in the type system & parser/Syntax so that these
	are all represented by the same type
	- this might be mildly annoying as it will change lots of files
  + how far do we actually go here? get rid of all lock ops and replace them
    with method invocations? probably better to just consider them special cases
    of method invocations? This would be case class inheritance, which isn't
    ideal.
  + add latencies into the LockImplementation.md file; don't really do anything
	with them yet

- #### Add in the "arguments" and "returns" for lock stuff
  + should actually do this so that in code generation we can fully have
    write-once semantics
  + makes the most sense to do this in the LockOpTranslationPass, as it also is
    concerned with memory. Could add Z3 to this, or just be conservative and
    take a ⊔ of the conditions and let the other passes take care of
    correctness.
	+ need to be somewhat careful here to make sure that we keep track of the
      latest operation, so that there aren't latency issues
	+ this is where it could potentially be nice to bring in the large hammer of
      the Z3Solver, but it might be unneeded since you shouldn't be
      conditionally reserving the same thing in different cycles, and if you
      REALLY want to do this it is quite possible with an alias
  + this does pretty sad things to memory. maybe we should have another trait
- #### Check that we don't use stuff in violation of the latency
  + this is actually really easy since it just involves making sure that nothing
    that isn't combinational is used in the same cycle.
  + since this would ideally happen after the lock args and rets are filled in,
    we should look in to doing this on the stage representation.
  + since we already have all the other checks, it really should be possible
    just to look for intracycle violations

## other random stuff
 - m o r e  operators! (good distinction between the full precision ones and the
   normal ones. Also unary ops such as inversion, and folding operations)
 - release that does something should have sequential latency
