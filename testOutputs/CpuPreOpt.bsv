import FIFOF :: *;
import Locks :: *;
import Memories :: *;

export Cpu (..);
export mkCpu ;

typedef struct { UInt#(32) pc ; } E__input__TO_Start deriving( Bits,Eq );
typedef struct { UInt#(32) tmp; UInt#(32) npc; UInt#(5) rd; UInt#(32) alu_res; UInt#(4) _threadID ; } E_Stage__6_TO_Stage__3 deriving( Bits,Eq );
typedef struct { UInt#(32) npc; UInt#(4) _threadID ; } E_Stage__4_TO_Stage__5 deriving( Bits,Eq );
typedef struct { UInt#(32) arg1; UInt#(32) pc; UInt#(1) op_type; UInt#(32) arg2; UInt#(5) rd; UInt#(4) _threadID ; } E_Stage__0_TO_Stage__1 deriving( Bits,Eq );
typedef struct { UInt#(32) npc; UInt#(1) op_type; UInt#(5) rd; UInt#(32) alu_res; UInt#(4) _threadID ; } E_Stage__1_TO_Stage__2 deriving( Bits,Eq );
typedef struct { Bool __condStage__6; UInt#(4) _threadID ; } E_Stage__6_TO_Stage__5 deriving( Bits,Eq );
typedef struct { UInt#(32) pc; UInt#(4) _threadID ; } E_Start_TO_Stage__0 deriving( Bits,Eq );
typedef struct { UInt#(32) npc; UInt#(4) _threadID ; } E_Stage__3_TO_Stage__5 deriving( Bits,Eq );
typedef struct { UInt#(32) npc; UInt#(4) _threadID ; } E_Stage__6_TO_Stage__4 deriving( Bits,Eq );
typedef struct { UInt#(32) tmp; UInt#(32) npc; UInt#(1) op_type; UInt#(5) rd; UInt#(32) alu_res; UInt#(4) _threadID ; } E_Stage__2_TO_Stage__6 deriving( Bits,Eq );

interface Cpu;
    method Action start ( UInt#(32) pc ) ;
endinterface

module mkStage__5 ( FIFOF#( E_Stage__3_TO_Stage__5 ) s_Stage__3, FIFOF#( E_Stage__4_TO_Stage__5 ) s_Stage__4, FIFOF#( E_Stage__6_TO_Stage__5 ) s_Stage__6, FIFOF#( E__input__TO_Start ) s_Start_to, MemCombRead#(UInt#(32),UInt#(5)) rf, AsyncMem#(UInt#(16),UInt#(32)) imem, Lock#( UInt#(4) ) rf_lock, Lock#( UInt#(4) ) imem_lock, Empty _unused_ );
    Bool __condStage__6 = s_Stage__6.first().__condStage__6;
    UInt#(4) _threadID = s_Stage__6.first()._threadID;
    UInt#(32) npc = ( ( ! __condStage__6 ) ? s_Stage__4.first().npc : ( __condStage__6 ? s_Stage__3.first().npc : ? ) );
    UInt#(32) carg = npc;
    rule execute ;
        s_Start_to.enq(E__input__TO_Start { pc : carg });
        rf_lock.rel(_threadID);
        s_Stage__6.deq();
        if ( __condStage__6)
        begin
            s_Stage__3.deq();
        end
        if ( ( ! __condStage__6 ))
        begin
            s_Stage__4.deq();
        end
    endrule
endmodule
module mkStage__1 ( FIFOF#( E_Stage__0_TO_Stage__1 ) s_Stage__0, FIFOF#( E_Stage__1_TO_Stage__2 ) s_Stage__2, FIFOF#( E__input__TO_Start ) s_Start_to, MemCombRead#(UInt#(32),UInt#(5)) rf, AsyncMem#(UInt#(16),UInt#(32)) imem, Lock#( UInt#(4) ) rf_lock, Lock#( UInt#(4) ) imem_lock, Empty _unused_ );
    UInt#(32) arg1 = s_Stage__0.first().arg1;
    UInt#(32) pc = s_Stage__0.first().pc;
    UInt#(1) op_type = s_Stage__0.first().op_type;
    UInt#(32) arg2 = s_Stage__0.first().arg2;
    UInt#(5) rd = s_Stage__0.first().rd;
    UInt#(4) _threadID = s_Stage__0.first()._threadID;
    UInt#(32) offset = ( ( ( op_type == 1'b0 ) && ( arg1 == arg2 ) ) ? unpack({ pack(27'b0), pack(rd) }) : 32'd4 );
    UInt#(32) npc = ( pc + offset );
    UInt#(32) alu_res = ( arg1 + arg2 );
    rule execute ;
        s_Stage__0.deq();
        s_Stage__2.enq(E_Stage__1_TO_Stage__2 { alu_res : alu_res,rd : rd,npc : npc,op_type : op_type,_threadID : _threadID });
    endrule
endmodule
module mkStage__6 ( FIFOF#( E_Stage__2_TO_Stage__6 ) s_Stage__2, FIFOF#( E_Stage__6_TO_Stage__3 ) s_Stage__3, FIFOF#( E_Stage__6_TO_Stage__4 ) s_Stage__4, FIFOF#( E_Stage__6_TO_Stage__5 ) s_Stage__5, FIFOF#( E__input__TO_Start ) s_Start_to, MemCombRead#(UInt#(32),UInt#(5)) rf, AsyncMem#(UInt#(16),UInt#(32)) imem, Lock#( UInt#(4) ) rf_lock, Lock#( UInt#(4) ) imem_lock, Empty _unused_ );
    UInt#(32) tmp = s_Stage__2.first().tmp;
    UInt#(32) npc = s_Stage__2.first().npc;
    UInt#(1) op_type = s_Stage__2.first().op_type;
    UInt#(5) rd = s_Stage__2.first().rd;
    UInt#(32) alu_res = s_Stage__2.first().alu_res;
    UInt#(4) _threadID = s_Stage__2.first()._threadID;
    Bool __condStage__6 = ( op_type == 1'b1 );
    rule execute ;
        s_Stage__2.deq();
        if ( __condStage__6)
        begin
            s_Stage__3.enq(E_Stage__6_TO_Stage__3 { tmp : tmp,alu_res : alu_res,rd : rd,npc : npc,_threadID : _threadID });
        end
        if ( ( ! __condStage__6 ))
        begin
            s_Stage__4.enq(E_Stage__6_TO_Stage__4 { npc : npc,_threadID : _threadID });
        end
        s_Stage__5.enq(E_Stage__6_TO_Stage__5 { __condStage__6 : __condStage__6,_threadID : _threadID });
    endrule
endmodule
module mkStart ( FIFOF#( E_Start_TO_Stage__0 ) s_Stage__0, FIFOF#( E__input__TO_Start ) s__input_, MemCombRead#(UInt#(32),UInt#(5)) rf, AsyncMem#(UInt#(16),UInt#(32)) imem, Lock#( UInt#(4) ) rf_lock, Lock#( UInt#(4) ) imem_lock, Empty _unused_ );
    Reg#( UInt#(4) ) _threadID <- mkReg ( 0 );
    UInt#(32) pc = s__input_.first().pc;
    rule execute (imem_lock.owns(_threadID));
        imem.readReq( pc );
        _threadID <= ( _threadID + 1 );
        s__input_.deq();
        s_Stage__0.enq(E_Start_TO_Stage__0 { pc : pc,_threadID : _threadID });
    endrule
endmodule
module mkStage__4 ( FIFOF#( E_Stage__4_TO_Stage__5 ) s_Stage__5, FIFOF#( E_Stage__6_TO_Stage__4 ) s_Stage__6, FIFOF#( E__input__TO_Start ) s_Start_to, MemCombRead#(UInt#(32),UInt#(5)) rf, AsyncMem#(UInt#(16),UInt#(32)) imem, Lock#( UInt#(4) ) rf_lock, Lock#( UInt#(4) ) imem_lock, Empty _unused_ );
    UInt#(32) npc = s_Stage__6.first().npc;
    UInt#(4) _threadID = s_Stage__6.first()._threadID;
    rule execute ;
        s_Stage__6.deq();
        s_Stage__5.enq(E_Stage__4_TO_Stage__5 { npc : npc,_threadID : _threadID });
    endrule
endmodule
module mkStage__0 ( FIFOF#( E_Stage__0_TO_Stage__1 ) s_Stage__1, FIFOF#( E_Start_TO_Stage__0 ) s_Start, FIFOF#( E__input__TO_Start ) s_Start_to, MemCombRead#(UInt#(32),UInt#(5)) rf, AsyncMem#(UInt#(16),UInt#(32)) imem, Lock#( UInt#(4) ) rf_lock, Lock#( UInt#(4) ) imem_lock, Empty _unused_ );
    UInt#(32) pc = s_Start.first().pc;
    UInt#(4) _threadID = s_Start.first()._threadID;
    UInt#(16) insn = imem.peekRead();
    UInt#(1) op_type = unpack( pack( insn ) [ 0 : 0 ] );
    UInt#(5) rs1 = unpack( pack( insn ) [ 5 : 1 ] );
    UInt#(5) rs2 = unpack( pack( insn ) [ 10 : 6 ] );
    UInt#(5) rd = unpack( pack( insn ) [ 15 : 11 ] );
    UInt#(32) arg1 = rf.read(rs1);
    UInt#(32) arg2 = rf.read(rs2);
    rule execute (rf_lock.owns(_threadID));
        imem.readResp();
        rf_lock.acq(_threadID);
        s_Start.deq();
        s_Stage__1.enq(E_Stage__0_TO_Stage__1 { pc : pc,rd : rd,_threadID : _threadID,arg1 : arg1,arg2 : arg2,op_type : op_type });
    endrule
endmodule
module mkStage__3 ( FIFOF#( E_Stage__3_TO_Stage__5 ) s_Stage__5, FIFOF#( E_Stage__6_TO_Stage__3 ) s_Stage__6, FIFOF#( E__input__TO_Start ) s_Start_to, MemCombRead#(UInt#(32),UInt#(5)) rf, AsyncMem#(UInt#(16),UInt#(32)) imem, Lock#( UInt#(4) ) rf_lock, Lock#( UInt#(4) ) imem_lock, Empty _unused_ );
    UInt#(32) tmp = s_Stage__6.first().tmp;
    UInt#(32) npc = s_Stage__6.first().npc;
    UInt#(5) rd = s_Stage__6.first().rd;
    UInt#(32) alu_res = s_Stage__6.first().alu_res;
    UInt#(4) _threadID = s_Stage__6.first()._threadID;
    UInt#(32) msg = ( alu_res + tmp );
    rule execute ;
        rf.write( rd , msg );
        s_Stage__6.deq();
        s_Stage__5.enq(E_Stage__3_TO_Stage__5 { npc : npc,_threadID : _threadID });
    endrule
endmodule
module mkStage__2 ( FIFOF#( E_Stage__1_TO_Stage__2 ) s_Stage__1, FIFOF#( E_Stage__2_TO_Stage__6 ) s_Stage__6, FIFOF#( E__input__TO_Start ) s_Start_to, MemCombRead#(UInt#(32),UInt#(5)) rf, AsyncMem#(UInt#(16),UInt#(32)) imem, Lock#( UInt#(4) ) rf_lock, Lock#( UInt#(4) ) imem_lock, Empty _unused_ );
    UInt#(32) npc = s_Stage__1.first().npc;
    UInt#(1) op_type = s_Stage__1.first().op_type;
    UInt#(5) rd = s_Stage__1.first().rd;
    UInt#(32) alu_res = s_Stage__1.first().alu_res;
    UInt#(4) _threadID = s_Stage__1.first()._threadID;
    UInt#(32) tmp = 32'b1;
    rule execute ;
        s_Stage__1.deq();
        s_Stage__6.enq(E_Stage__2_TO_Stage__6 { tmp : tmp,alu_res : alu_res,rd : rd,npc : npc,op_type : op_type,_threadID : _threadID });
    endrule
endmodule

module mkCpu ( MemCombRead#(UInt#(32),UInt#(5)) rf, AsyncMem#(UInt#(16),UInt#(32)) imem, Cpu _unused_ );
    FIFOF#( E_Stage__6_TO_Stage__3 ) fifo_Stage__6_Stage__3 <- mkFIFOF (  );
    FIFOF#( E_Stage__4_TO_Stage__5 ) fifo_Stage__4_Stage__5 <- mkFIFOF (  );
    FIFOF#( E_Stage__0_TO_Stage__1 ) fifo_Stage__0_Stage__1 <- mkFIFOF (  );
    FIFOF#( E_Stage__1_TO_Stage__2 ) fifo_Stage__1_Stage__2 <- mkFIFOF (  );
    FIFOF#( E_Stage__6_TO_Stage__5 ) fifo_Stage__6_Stage__5 <- mkFIFOF (  );
    FIFOF#( E__input__TO_Start ) fifo__input__Start <- mkFIFOF (  );
    FIFOF#( E_Start_TO_Stage__0 ) fifo_Start_Stage__0 <- mkFIFOF (  );
    FIFOF#( E_Stage__3_TO_Stage__5 ) fifo_Stage__3_Stage__5 <- mkFIFOF (  );
    FIFOF#( E_Stage__6_TO_Stage__4 ) fifo_Stage__6_Stage__4 <- mkFIFOF (  );
    FIFOF#( E_Stage__2_TO_Stage__6 ) fifo_Stage__2_Stage__6 <- mkFIFOF (  );
    Lock#( UInt#(4) ) rf_lock <- mkLock (  );
    Lock#( UInt#(4) ) imem_lock <- mkLock (  );
    Reg#( Bool ) busy <- mkReg ( False );
    Empty s_Start <- mkStart ( fifo_Start_Stage__0, fifo__input__Start, rf, imem, rf_lock, imem_lock );
    Empty s_Stage__0 <- mkStage__0 ( fifo_Stage__0_Stage__1, fifo_Start_Stage__0, fifo__input__Start, rf, imem, rf_lock, imem_lock );
    Empty s_Stage__1 <- mkStage__1 ( fifo_Stage__0_Stage__1, fifo_Stage__1_Stage__2, fifo__input__Start, rf, imem, rf_lock, imem_lock );
    Empty s_Stage__2 <- mkStage__2 ( fifo_Stage__1_Stage__2, fifo_Stage__2_Stage__6, fifo__input__Start, rf, imem, rf_lock, imem_lock );
    Empty s_Stage__6 <- mkStage__6 ( fifo_Stage__2_Stage__6, fifo_Stage__6_Stage__3, fifo_Stage__6_Stage__4, fifo_Stage__6_Stage__5, fifo__input__Start, rf, imem, rf_lock, imem_lock );
    Empty s_Stage__3 <- mkStage__3 ( fifo_Stage__3_Stage__5, fifo_Stage__6_Stage__3, fifo__input__Start, rf, imem, rf_lock, imem_lock );
    Empty s_Stage__4 <- mkStage__4 ( fifo_Stage__4_Stage__5, fifo_Stage__6_Stage__4, fifo__input__Start, rf, imem, rf_lock, imem_lock );
    Empty s_Stage__5 <- mkStage__5 ( fifo_Stage__3_Stage__5, fifo_Stage__4_Stage__5, fifo_Stage__6_Stage__5, fifo__input__Start, rf, imem, rf_lock, imem_lock );
    method Action start ( UInt#(32) pc ) if( ( ! busy ) );
        fifo__input__Start.enq(E__input__TO_Start { pc : pc });
        busy <= True;
    endmethod
endmodule
