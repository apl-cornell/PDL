package Speculation;

import Vector :: *;
import ConfigReg :: *;
import Ehr :: *;

typedef UInt#(TLog#(n)) SpecId#(numeric type n);

interface SpecTable#(type sid);
    method ActionValue#(sid) alloc();
    method Maybe#(Bool) check(sid s, Integer i);
    method Action free(sid s);
    method Action validate(sid s, Integer i);
    method Action invalidate(sid s, Integer i);
endinterface

module mkSpecTable(SpecTable#(SpecId#(entries)));

   // Schedule => rules need to be associated with an index
   // the _lower_ the index, the earlier they access the speculation table.
   // Rules Containing speccall > containing update > only verify
   // This ensures that speculative stages will not send data to the beginning of the pipeline IF we
   // are also sending real data to the beginning
   
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

    method Action free(SpecId#(entries) s);
        inUse[s] <= False;
    endmethod
   
   method Maybe#(Bool) check(SpecId#(entries) s, Integer i);
       if (!inUse[s])
	  return tagged Invalid;
       else
	  return specStatus[s][i];
    endmethod


    //mark s as valid (correctly speculated)
    method Action validate(SpecId#(entries) s, Integer i);
        specStatus[s][i] <= tagged Valid True;
    endmethod

    //mark s and all newer entries as invalid (misspeculated)
    method Action invalidate(SpecId#(entries) s, Integer j);
       for (Integer i = 0; i < valueOf(entries); i = i + 1) begin
	  SpecId#(entries) lv = fromInteger(i);
	  if ((s == lv || isNewer(lv, s)) && inUse[lv]) specStatus[lv][j] <= tagged Valid False;
       end
    endmethod
   
endmodule





endpackage
