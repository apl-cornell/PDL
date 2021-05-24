package Speculation;

typedef UInt#(TLog#(n)) SpecId#(numeric type n);

interface SpecTable#(type sid);
    method ActionValue#(sid) alloc();
    method Maybe#(Bool) check(sid s);
    method Action free(sid s);
    method Action validate(sid s);
    method Action invalidate(sid s);
endinterface

module mkSpecTable(SpecTable#(SpecId#(entries)));

    Vector#(entries, Reg#(Bool)) inUse <- replicateM(mkReg(False));
    Vector#(entries, Reg#(Maybe#(Bool))) specStatus <- replicateM(mkReg(tagged Invalid));

    Reg#(SpecId#(entries)) head <- mkReg(0);
    Bool full = inUse[head];

    //allocate a new entry in the table to track speculation
    method ActionValue#(SpecId#(entries)) alloc() if !full;
        head <= head + 1;
        inUse[head] <= True;
        specStatus[head] <= tagged Invalid;
        return head;
    endmethod

    //lookup a given entry
    method Maybe#(Bool) check(SpecId#(entries) s);
        return tagged Invalid;
    endmethod

    method Action free(SpecId#(entries) s);
        inUse[s] <= False;
    endmethod

    //mark s as valid (correctly speculated)
    method Action validate(SpecId#(entries) s);
        specStatus[s] <= tagged Valid True;
    endmethod

    //mark s and all newer entries as invalid (misspeculated)
    method Action invalidate(SpecId#(entries) s);
        SpecId#(entries) tmp = s;
        while (tmp != head) {
            specStatus[tmp] <= tagged Valid False;
            tmp = tmp + 1;
        }
        if (full) specStatus[tmp] <= tagged Valid False;
    endmethod

endmodule





endpackage