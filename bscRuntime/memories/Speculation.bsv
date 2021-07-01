package Speculation;

import Vector :: *;
import ConfigReg :: *;
import Ehr :: *;

typedef UInt#(TLog#(n)) SpecId#(numeric type n);

interface SpecTable#(type sid);
    method ActionValue#(sid) alloc();
    method Maybe#(Bool) nbcheck(sid s);   
    method Maybe#(Bool) nbcheck1(sid s);      
    method Maybe#(Bool) check(sid s);
    method Action free(sid s);
    method Action validate(sid s);
    method Action invalidate(sid s);
    method Action invalidate1(sid s);   
endinterface

module mkSpecTable(SpecTable#(SpecId#(entries)));

   //Schedule -> INVALIDATE must happen BEFORE NBCheck
   // but not before BlockingCheck (this prevents combinational loops)
   //Nothing needs to observe alloc

    Vector#(entries, Reg#(Bool)) inUse <- replicateM(mkConfigReg(False));
    Vector#(entries, Ehr#(3, Maybe#(Bool))) specStatus <- replicateM(mkEhr(tagged Invalid));

    Reg#(SpecId#(entries)) head <- mkReg(0);
    Bool full = inUse[head];

   //return true if entry a is a valid newer entry than b
   function Bool isNewer(SpecId#(entries) a, SpecId#(entries) b);
      let nohmid = a > b && !(b < head && a >= head);
      let hmid = a < head && b >= head;
      return nohmid || hmid;
   endfunction

   /*
   rule debug;
      $display("Head: %d", head);
      for (Integer i = 0; i < valueOf(entries); i = i + 1)
	 begin
	    $display("Idx %d, InUse: %b", i, inUse[fromInteger(i)]);   
	    $display("Idx %d, Status: %b", i, specStatus[fromInteger(i)]);
	 end
   endrule
    */
   
    //allocate a new entry in the table to track speculation	       
   method ActionValue#(SpecId#(entries)) alloc() if (!full);
        head <= head + 1;
        inUse[head] <= True;
        specStatus[head][2] <= tagged Invalid;
        return head;
    endmethod

    //lookup a given entry
    method Maybe#(Bool) nbcheck(SpecId#(entries) s);
       if (!inUse[s])
	  return tagged Invalid;
       else
	  return specStatus[s][1];
    endmethod

    //lookup a given entry (sees both invalidates)
    method Maybe#(Bool) nbcheck1(SpecId#(entries) s);
       if (!inUse[s])
	  return tagged Invalid;
       else
	  return specStatus[s][2];
    endmethod
   
   method Maybe#(Bool) check(SpecId#(entries) s);
       if (!inUse[s])
	  return tagged Invalid;
       else
	  return specStatus[s][0];
    endmethod

    method Action free(SpecId#(entries) s);
        inUse[s] <= False;
    endmethod

    //mark s as valid (correctly speculated)
    method Action validate(SpecId#(entries) s);
        specStatus[s][0] <= tagged Valid True;
    endmethod

    //mark s and all newer entries as invalid (misspeculated)
    method Action invalidate(SpecId#(entries) s);
       for (Integer i = 0; i < valueOf(entries); i = i + 1) begin
	  SpecId#(entries) lv = fromInteger(i);
	  if ((s == lv || isNewer(lv, s)) && inUse[lv]) specStatus[lv][0] <= tagged Valid False;
       end
    endmethod

   //same as invalidate but happens AFTER nbcheck
   method Action invalidate1(SpecId#(entries) s);
       for (Integer i = 0; i < valueOf(entries); i = i + 1) begin
	  SpecId#(entries) lv = fromInteger(i);
	  if ((s == lv || isNewer(lv, s)) && inUse[lv]) specStatus[lv][1] <= tagged Valid False;
       end
    endmethod
   
endmodule





endpackage
