import FIFOF :: *;
import Locks :: *;
import Memories :: *;
import Functions :: *;
import Multi_stg_mul :: *;
import Multi_stg_div :: *;

export Cpu (..);
export mkCpu ;

typedef struct { UInt#(16) pc; UInt#(4) _threadID ; } E__input__TO_Start deriving( Bits,Eq );
typedef struct { Bool __condStage__16; UInt#(4) _threadID ; } E_Stage__5_TO_Stage__15 deriving( Bits,Eq );
typedef struct { UInt#(2) _request_2; UInt#(5) rd; Maybe#( LockId#(4) ) _lock_id_rf_rd; UInt#(4) _threadID ; } E_Stage__5_TO_Stage__10 deriving( Bits,Eq );
typedef struct { UInt#(32) res; UInt#(5) rd; Maybe#( LockId#(4) ) _lock_id_rf_rd; UInt#(4) _threadID ; } E_Stage__7_TO_Stage__15 deriving( Bits,Eq );
typedef struct { UInt#(2) op; UInt#(32) rf2; UInt#(32) rf1; Maybe#( LockId#(4) ) _lock_id_rf_rd; UInt#(5) rd; UInt#(4) _threadID ; } E_Stage__4_TO_Stage__5 deriving( Bits,Eq );
typedef struct { UInt#(32) res; UInt#(5) rd; Maybe#( LockId#(4) ) _lock_id_rf_rd; UInt#(4) _threadID ; } E_Stage__10_TO_Stage__13 deriving( Bits,Eq );
typedef struct { UInt#(32) res; UInt#(5) rd; Maybe#( LockId#(4) ) _lock_id_rf_rd; UInt#(4) _threadID ; } E_Stage__12_TO_Stage__13 deriving( Bits,Eq );
typedef struct { MemId#(8) _request_1; UInt#(4) _threadID ; } E_Start_TO_Stage__4 deriving( Bits,Eq );
typedef struct { UInt#(1) _request_3; UInt#(5) rd; Maybe#( LockId#(4) ) _lock_id_rf_rd; UInt#(4) _threadID ; } E_Stage__5_TO_Stage__12 deriving( Bits,Eq );
typedef struct { UInt#(4) _threadID ; } E_Stage__15_TO_Stage__17 deriving( Bits,Eq );
typedef struct { UInt#(32) rf2; UInt#(32) rf1; Maybe#( LockId#(4) ) _lock_id_rf_rd; UInt#(5) rd; UInt#(4) _threadID ; } E_Stage__5_TO_Stage__7 deriving( Bits,Eq );
typedef struct { UInt#(32) res; UInt#(5) rd; Maybe#( LockId#(4) ) _lock_id_rf_rd; UInt#(4) _threadID ; } E_Stage__13_TO_Stage__15 deriving( Bits,Eq );
typedef struct { Bool __condStage__14; UInt#(4) _threadID ; } E_Stage__5_TO_Stage__13 deriving( Bits,Eq );
typedef struct { UInt#(4) handle ; } OutputQueueInfo deriving( Bits,Eq );

interface Cpu;
    method ActionValue#(UInt#(4)) req ( UInt#(16) pc ) ;
    method Action resp (  ) ;
    method Bool checkHandle ( UInt#(4) handle ) ;
endinterface


module mkCpu ( CombMem#( UInt#(32), UInt#(5) ) rf, AsyncMem#( UInt#(32), UInt#(16), MemId#(8) ) imem, Multi_stg_mul m, Multi_stg_div f, Cpu _unused_ );
    FIFOF#( E_Stage__5_TO_Stage__15 ) fifo_Stage__5_TO_Stage__15 <- mkFIFOF (  );
    FIFOF#( E_Stage__5_TO_Stage__10 ) fifo_Stage__5_TO_Stage__10 <- mkFIFOF (  );
    FIFOF#( E_Stage__7_TO_Stage__15 ) fifo_Stage__7_TO_Stage__15 <- mkFIFOF (  );
    FIFOF#( E_Stage__4_TO_Stage__5 ) fifo_Stage__4_TO_Stage__5 <- mkFIFOF (  );
    FIFOF#( E_Stage__10_TO_Stage__13 ) fifo_Stage__10_TO_Stage__13 <- mkFIFOF (  );
    FIFOF#( E_Stage__12_TO_Stage__13 ) fifo_Stage__12_TO_Stage__13 <- mkFIFOF (  );
    FIFOF#( E_Start_TO_Stage__4 ) fifo_Start_TO_Stage__4 <- mkFIFOF (  );
    FIFOF#( E_Stage__5_TO_Stage__12 ) fifo_Stage__5_TO_Stage__12 <- mkFIFOF (  );
    FIFOF#( E__input__TO_Start ) fifo__input__TO_Start <- mkFIFOF (  );
    FIFOF#( E_Stage__15_TO_Stage__17 ) fifo_Stage__15_TO_Stage__17 <- mkFIFOF (  );
    FIFOF#( E_Stage__5_TO_Stage__7 ) fifo_Stage__5_TO_Stage__7 <- mkFIFOF (  );
    FIFOF#( E_Stage__13_TO_Stage__15 ) fifo_Stage__13_TO_Stage__15 <- mkFIFOF (  );
    FIFOF#( E_Stage__5_TO_Stage__13 ) fifo_Stage__5_TO_Stage__13 <- mkFIFOF (  );
    AddrLock#( LockId#(4), UInt#(5), 4 ) rf_lock <- mkFAAddrLock (  );
    Lock#( LockId#(4) ) imem_lock <- mkLock (  );
    Lock#( LockId#(4) ) m_lock <- mkLock (  );
    Lock#( LockId#(4) ) f_lock <- mkLock (  );
    Reg#( Bool ) rf_lock_region <- mkReg ( True );
    Reg#( Bool ) imem_lock_region <- mkReg ( True );
    Reg#( Bool ) m_lock_region <- mkReg ( True );
    Reg#( Bool ) f_lock_region <- mkReg ( True );
    Reg#( Bool ) busyReg <- mkReg ( False );
    FIFOF#( OutputQueueInfo ) outputQueue <- mkFIFOF (  );
    Reg#( UInt#(4) ) _threadID <- mkReg ( 0 );
    UInt#(4) _Stage__17__threadID = fifo_Stage__15_TO_Stage__17.first()._threadID;
    UInt#(2) _Stage__5_op = fifo_Stage__4_TO_Stage__5.first().op;
    UInt#(32) _Stage__5_rf2 = fifo_Stage__4_TO_Stage__5.first().rf2;
    UInt#(32) _Stage__5_rf1 = fifo_Stage__4_TO_Stage__5.first().rf1;
    Maybe#( LockId#(4) ) _Stage__5__lock_id_rf_rd = fifo_Stage__4_TO_Stage__5.first()._lock_id_rf_rd;
    UInt#(5) _Stage__5_rd = fifo_Stage__4_TO_Stage__5.first().rd;
    UInt#(4) _Stage__5__threadID = fifo_Stage__4_TO_Stage__5.first()._threadID;
    Bool _Stage__5___condStage__16 = ?;
    Bool _Stage__5___condStage__14 = ?;
    UInt#(32) _Stage__5_carg_5 = ?;
    UInt#(32) _Stage__5_carg_6 = ?;
    UInt#(32) _Stage__5_carg_7 = ?;
    UInt#(32) _Stage__5_carg_8 = ?;
    UInt#(32) _Stage__5_carg_9 = ?;
    UInt#(32) _Stage__5_carg_10 = ?;
    UInt#(5) _Stage__5_carg_11 = ?;
    _Stage__5___condStage__16 = ( _Stage__5_op == 2'd0 );
    if ( ( ! _Stage__5___condStage__16 ))
    begin
        _Stage__5___condStage__14 = ( _Stage__5_op == 2'd1 );
    end
    if ( ( ( ! _Stage__5___condStage__16 ) && _Stage__5___condStage__14 ))
    begin
        _Stage__5_carg_5 = _Stage__5_rf1;
        _Stage__5_carg_6 = _Stage__5_rf2;
    end
    if ( ( ( ! _Stage__5___condStage__16 ) && ( ! _Stage__5___condStage__14 ) ))
    begin
        _Stage__5_carg_7 = _Stage__5_rf1;
        _Stage__5_carg_8 = _Stage__5_rf2;
        _Stage__5_carg_9 = 32'd0;
        _Stage__5_carg_10 = 32'd0;
        _Stage__5_carg_11 = 5'd0;
    end
    UInt#(1) _Stage__12__request_3 = fifo_Stage__5_TO_Stage__12.first()._request_3;
    UInt#(5) _Stage__12_rd = fifo_Stage__5_TO_Stage__12.first().rd;
    Maybe#( LockId#(4) ) _Stage__12__lock_id_rf_rd = fifo_Stage__5_TO_Stage__12.first()._lock_id_rf_rd;
    UInt#(4) _Stage__12__threadID = fifo_Stage__5_TO_Stage__12.first()._threadID;
    UInt#(32) _Stage__12_res = ?;
    _Stage__12_res = f.peek();
    MemId#(8) _Stage__4__request_1 = fifo_Start_TO_Stage__4.first()._request_1;
    UInt#(4) _Stage__4__threadID = fifo_Start_TO_Stage__4.first()._threadID;
    UInt#(32) _Stage__4_insn = ?;
    UInt#(2) _Stage__4_op = ?;
    UInt#(5) _Stage__4_rs1 = ?;
    UInt#(5) _Stage__4_rs2 = ?;
    UInt#(5) _Stage__4_rd = ?;
    UInt#(32) _Stage__4_rf1 = ?;
    UInt#(32) _Stage__4_rf2 = ?;
    _Stage__4_insn = imem.peekResp(_Stage__4__request_1);
    _Stage__4_op = unpack( pack( _Stage__4_insn ) [ 1 : 0 ] );
    _Stage__4_rs1 = unpack( pack( _Stage__4_insn ) [ 6 : 2 ] );
    _Stage__4_rs2 = unpack( pack( _Stage__4_insn ) [ 11 : 7 ] );
    _Stage__4_rd = unpack( pack( _Stage__4_insn ) [ 16 : 12 ] );
    _Stage__4_rf1 = rf.read(_Stage__4_rs1);
    _Stage__4_rf2 = rf.read(_Stage__4_rs2);
    UInt#(16) _Start_pc = fifo__input__TO_Start.first().pc;
    UInt#(4) _Start__threadID = fifo__input__TO_Start.first()._threadID;
    Bool _Start___condStage__3 = ?;
    UInt#(16) _Start_carg_4 = ?;
    _Start___condStage__3 = ( _Start_pc < 16'd14 );
    if ( _Start___condStage__3)
    begin
        _Start_carg_4 = ( _Start_pc + 16'd1 );
    end
    Bool _Stage__13___condStage__14 = fifo_Stage__5_TO_Stage__13.first().__condStage__14;
    UInt#(4) _Stage__13__threadID = fifo_Stage__5_TO_Stage__13.first()._threadID;
    UInt#(32) _Stage__13_res = ( ( ! _Stage__13___condStage__14 ) ? fifo_Stage__12_TO_Stage__13.first().res : ( _Stage__13___condStage__14 ? fifo_Stage__10_TO_Stage__13.first().res : ? ) );
    UInt#(5) _Stage__13_rd = ( ( ! _Stage__13___condStage__14 ) ? fifo_Stage__12_TO_Stage__13.first().rd : ( _Stage__13___condStage__14 ? fifo_Stage__10_TO_Stage__13.first().rd : ? ) );
    Maybe#( LockId#(4) ) _Stage__13__lock_id_rf_rd = ( ( ! _Stage__13___condStage__14 ) ? fifo_Stage__12_TO_Stage__13.first()._lock_id_rf_rd : ( _Stage__13___condStage__14 ? fifo_Stage__10_TO_Stage__13.first()._lock_id_rf_rd : ? ) );
    Bool _Stage__15___condStage__16 = fifo_Stage__5_TO_Stage__15.first().__condStage__16;
    UInt#(4) _Stage__15__threadID = fifo_Stage__5_TO_Stage__15.first()._threadID;
    UInt#(32) _Stage__15_res = ( ( ! _Stage__15___condStage__16 ) ? fifo_Stage__13_TO_Stage__15.first().res : ( _Stage__15___condStage__16 ? fifo_Stage__7_TO_Stage__15.first().res : ? ) );
    UInt#(5) _Stage__15_rd = ( ( ! _Stage__15___condStage__16 ) ? fifo_Stage__13_TO_Stage__15.first().rd : ( _Stage__15___condStage__16 ? fifo_Stage__7_TO_Stage__15.first().rd : ? ) );
    Maybe#( LockId#(4) ) _Stage__15__lock_id_rf_rd = ( ( ! _Stage__15___condStage__16 ) ? fifo_Stage__13_TO_Stage__15.first()._lock_id_rf_rd : ( _Stage__15___condStage__16 ? fifo_Stage__7_TO_Stage__15.first()._lock_id_rf_rd : ? ) );
    UInt#(2) _Stage__10__request_2 = fifo_Stage__5_TO_Stage__10.first()._request_2;
    UInt#(5) _Stage__10_rd = fifo_Stage__5_TO_Stage__10.first().rd;
    Maybe#( LockId#(4) ) _Stage__10__lock_id_rf_rd = fifo_Stage__5_TO_Stage__10.first()._lock_id_rf_rd;
    UInt#(4) _Stage__10__threadID = fifo_Stage__5_TO_Stage__10.first()._threadID;
    UInt#(32) _Stage__10_res = ?;
    _Stage__10_res = m.peek();
    UInt#(32) _Stage__7_rf2 = fifo_Stage__5_TO_Stage__7.first().rf2;
    UInt#(32) _Stage__7_rf1 = fifo_Stage__5_TO_Stage__7.first().rf1;
    Maybe#( LockId#(4) ) _Stage__7__lock_id_rf_rd = fifo_Stage__5_TO_Stage__7.first()._lock_id_rf_rd;
    UInt#(5) _Stage__7_rd = fifo_Stage__5_TO_Stage__7.first().rd;
    UInt#(4) _Stage__7__threadID = fifo_Stage__5_TO_Stage__7.first()._threadID;
    UInt#(32) _Stage__7_res = ?;
    _Stage__7_res = ( _Stage__7_rf1 + _Stage__7_rf2 );
    rule s_Stage__17_execute ;
        fifo_Stage__15_TO_Stage__17.deq();
        $display("cpu:Thread %d:Executing Stage Stage__17 %t", _Stage__17__threadID,$time() );
    endrule
    rule s_Stage__5_execute (( ( ! ( ( ! _Stage__5___condStage__16 ) && _Stage__5___condStage__14 ) ) || m_lock.isEmpty() ) && ( ( ! ( ( ! _Stage__5___condStage__16 ) && ( ! _Stage__5___condStage__14 ) ) ) || f_lock.isEmpty() ));
        UInt#(2) _Stage__5__request_2 = ?;
        UInt#(1) _Stage__5__request_3 = ?;
        if ( ( ( ! _Stage__5___condStage__16 ) && _Stage__5___condStage__14 ))
        begin
            _Stage__5__request_2 <- m.req(_Stage__5_carg_5, _Stage__5_carg_6);
        end
        if ( ( ( ! _Stage__5___condStage__16 ) && ( ! _Stage__5___condStage__14 ) ))
        begin
            _Stage__5__request_3 <- f.req(_Stage__5_carg_7, _Stage__5_carg_8, _Stage__5_carg_9, _Stage__5_carg_10, _Stage__5_carg_11);
        end
        fifo_Stage__5_TO_Stage__15.enq(E_Stage__5_TO_Stage__15 { __condStage__16 : _Stage__5___condStage__16,_threadID : _Stage__5__threadID });
        if ( ( ( ! _Stage__5___condStage__16 ) && _Stage__5___condStage__14 ))
        begin
            fifo_Stage__5_TO_Stage__10.enq(E_Stage__5_TO_Stage__10 { _request_2 : _Stage__5__request_2,rd : _Stage__5_rd,_lock_id_rf_rd : _Stage__5__lock_id_rf_rd,_threadID : _Stage__5__threadID });
        end
        fifo_Stage__4_TO_Stage__5.deq();
        if ( ( ( ! _Stage__5___condStage__16 ) && ( ! _Stage__5___condStage__14 ) ))
        begin
            fifo_Stage__5_TO_Stage__12.enq(E_Stage__5_TO_Stage__12 { _request_3 : _Stage__5__request_3,rd : _Stage__5_rd,_lock_id_rf_rd : _Stage__5__lock_id_rf_rd,_threadID : _Stage__5__threadID });
        end
        if ( ( ! _Stage__5___condStage__16 ))
        begin
            fifo_Stage__5_TO_Stage__13.enq(E_Stage__5_TO_Stage__13 { __condStage__14 : _Stage__5___condStage__14,_threadID : _Stage__5__threadID });
        end
        if ( _Stage__5___condStage__16)
        begin
            fifo_Stage__5_TO_Stage__7.enq(E_Stage__5_TO_Stage__7 { rf2 : _Stage__5_rf2,rf1 : _Stage__5_rf1,rd : _Stage__5_rd,_threadID : _Stage__5__threadID,_lock_id_rf_rd : _Stage__5__lock_id_rf_rd });
        end
        $display("cpu:Thread %d:Executing Stage Stage__5 %t", _Stage__5__threadID,$time() );
    endrule
    rule s_Stage__12_execute (f.checkHandle(_Stage__12__request_3));
        f.resp();
        fifo_Stage__12_TO_Stage__13.enq(E_Stage__12_TO_Stage__13 { res : _Stage__12_res,rd : _Stage__12_rd,_lock_id_rf_rd : _Stage__12__lock_id_rf_rd,_threadID : _Stage__12__threadID });
        fifo_Stage__5_TO_Stage__12.deq();
        $display("cpu:Thread %d:Executing Stage Stage__12 %t", _Stage__12__threadID,$time() );
    endrule
    rule s_Stage__4_execute (imem.checkRespId(_Stage__4__request_1) && rf_lock.isEmpty(_Stage__4_rs1) && rf_lock.isEmpty(_Stage__4_rs2) && rf_lock.canRes(_Stage__4_rd));
        Maybe#( LockId#(4) ) _Stage__4__lock_id_rf_rd = ?;
        imem.resp(_Stage__4__request_1);
        _Stage__4__lock_id_rf_rd <- rf_lock.res(_Stage__4_rd);
        fifo_Stage__4_TO_Stage__5.enq(E_Stage__4_TO_Stage__5 { rf1 : _Stage__4_rf1,op : _Stage__4_op,_threadID : _Stage__4__threadID,_lock_id_rf_rd : _Stage__4__lock_id_rf_rd,rd : _Stage__4_rd,rf2 : _Stage__4_rf2 });
        fifo_Start_TO_Stage__4.deq();
        $display("cpu:Thread %d:Executing Stage Stage__4 %t", _Stage__4__threadID,$time() );
    endrule
    rule s_Start_execute (imem_lock.isEmpty());
        MemId#(8) _Start__request_1 = ?;
        if ( _Start___condStage__3)
        begin
            fifo__input__TO_Start.enq(E__input__TO_Start { pc : _Start_carg_4,_threadID : _Start__threadID });
        end
        _Start__request_1 <- imem.req(_Start_pc, ?, False);
        fifo__input__TO_Start.deq();
        fifo_Start_TO_Stage__4.enq(E_Start_TO_Stage__4 { _request_1 : _Start__request_1,_threadID : _Start__threadID });
        $display("cpu:Thread %d:Executing Stage Start %t", _Start__threadID,$time() );
    endrule
    rule s_Stage__13_execute ;
        if ( _Stage__13___condStage__14)
        begin
            fifo_Stage__10_TO_Stage__13.deq();
        end
        if ( ( ! _Stage__13___condStage__14 ))
        begin
            fifo_Stage__12_TO_Stage__13.deq();
        end
        fifo_Stage__13_TO_Stage__15.enq(E_Stage__13_TO_Stage__15 { res : _Stage__13_res,rd : _Stage__13_rd,_lock_id_rf_rd : _Stage__13__lock_id_rf_rd,_threadID : _Stage__13__threadID });
        fifo_Stage__5_TO_Stage__13.deq();
        $display("cpu:Thread %d:Executing Stage Stage__13 %t", _Stage__13__threadID,$time() );
    endrule
    rule s_Stage__15_execute (rf_lock.owns(fromMaybe( 0 , _Stage__15__lock_id_rf_rd ), _Stage__15_rd));
        $display(_Stage__15_res);
        rf.write(_Stage__15_rd, _Stage__15_res);
        rf_lock.rel(fromMaybe( 0 , _Stage__15__lock_id_rf_rd ), _Stage__15_rd);
        if ( _Stage__15___condStage__16)
        begin
            fifo_Stage__7_TO_Stage__15.deq();
        end
        if ( ( ! _Stage__15___condStage__16 ))
        begin
            fifo_Stage__13_TO_Stage__15.deq();
        end
        fifo_Stage__15_TO_Stage__17.enq(E_Stage__15_TO_Stage__17 { _threadID : _Stage__15__threadID });
        fifo_Stage__5_TO_Stage__15.deq();
        $display("cpu:Thread %d:Executing Stage Stage__15 %t", _Stage__15__threadID,$time() );
    endrule
    rule s_Stage__10_execute (m.checkHandle(_Stage__10__request_2));
        m.resp();
        fifo_Stage__10_TO_Stage__13.enq(E_Stage__10_TO_Stage__13 { res : _Stage__10_res,rd : _Stage__10_rd,_lock_id_rf_rd : _Stage__10__lock_id_rf_rd,_threadID : _Stage__10__threadID });
        fifo_Stage__5_TO_Stage__10.deq();
        $display("cpu:Thread %d:Executing Stage Stage__10 %t", _Stage__10__threadID,$time() );
    endrule
    rule s_Stage__7_execute ;
        fifo_Stage__7_TO_Stage__15.enq(E_Stage__7_TO_Stage__15 { res : _Stage__7_res,rd : _Stage__7_rd,_lock_id_rf_rd : _Stage__7__lock_id_rf_rd,_threadID : _Stage__7__threadID });
        fifo_Stage__5_TO_Stage__7.deq();
        $display("cpu:Thread %d:Executing Stage Stage__7 %t", _Stage__7__threadID,$time() );
    endrule
    method ActionValue#(UInt#(4)) req ( UInt#(16) pc ) if( ( ! busyReg ) );
        fifo__input__TO_Start.enq(E__input__TO_Start { pc : pc,_threadID : _threadID });
        busyReg <= True;
        _threadID <= ( _threadID + 1 );
        return _threadID;
    endmethod
    method Action resp (  ) ;
        outputQueue.deq();
    endmethod
    method Bool checkHandle ( UInt#(4) handle ) ;
        return ( handle == outputQueue.first().handle );
    endmethod
endmodule
