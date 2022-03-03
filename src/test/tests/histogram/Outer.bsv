import FIFOF :: *;
import SpecialFIFOs :: *;
import SpecialQueues :: *;
import Locks :: *;
import Memories :: *;
import VerilogLibs :: *;
import Speculation :: *;
import RegFile :: *;
import Functions :: *;
import Hist :: *;

export Outer (..);
export mkOuter ;

typedef struct { UInt#(10) counter; UInt#(2) _threadID ; } E__input__TO_Start deriving( Bits,Eq );
typedef struct { UInt#(3) _request_1; UInt#(2) _threadID ; } E_Stage__1_TO_Stage__3 deriving( Bits,Eq );
typedef struct { UInt#(1) __condStage__4; UInt#(2) _threadID ; } E_Start_TO_Stage__3 deriving( Bits,Eq );
typedef struct { UInt#(2) _threadID ; } E_Start_TO_Stage__3_1 deriving( Bits,Eq );
typedef struct { UInt#(10) counter; Maybe#( LockId#(4) ) _lock_id_h_rs; UInt#(2) _threadID ; } E_Start_TO_Stage__1 deriving( Bits,Eq );

interface Outer;
    method ActionValue#(UInt#(2)) req ( UInt#(10) counter ) ;
    method Action resp (  ) ;
    method Bool checkHandle ( UInt#(2) handle ) ;
endinterface


module mkOuter ( Hist h, Outer _unused_ ) provisos(  );
    FIFOF#( E__input__TO_Start ) fifo__input__TO_Start <- mkNBFIFOF (  );
    FIFOF#( E_Start_TO_Stage__3_1 ) fifo_Start_TO_Stage__3_1 <- mkFIFOF (  );
    FIFOF#( E_Start_TO_Stage__1 ) fifo_Start_TO_Stage__1 <- mkFIFOF (  );
    FIFOF#( E_Start_TO_Stage__3 ) fifo_Start_TO_Stage__3 <- mkFIFOF (  );
    FIFOF#( E_Stage__1_TO_Stage__3 ) fifo_Stage__1_TO_Stage__3 <- mkFIFOF (  );
    Reg#( Bool ) h_lock_region <- mkReg ( True );
    CheckpointQueueLock#( LockId#(4), LockId#(4) ) _lock_h <- mkCheckpointQueueLock (  );
    Reg#( Bool ) busyReg <- mkReg ( False );
    OutputQ#( UInt#(2), void ) outputQueue <- mkOutputFIFOF ( 0 );
    Reg#( UInt#(2) ) _threadID <- mkReg ( 0 );
    UInt#(10) _Start_counter = fifo__input__TO_Start.first.counter;
    UInt#(2) _Start__threadID = fifo__input__TO_Start.first._threadID;
    Bool _Start_done = ?;
    UInt#(1) _Start___condStage__4 = ?;
    UInt#(10) _Start_carg_332 = ?;
    _Start_done = ( _Start_counter >= 10'd1000 );
    _Start___condStage__4 = ( ( ! _Start_done ) ? 1'd0 : 1'd1 );
    if ( ( _Start___condStage__4 == 1'd0 ))
    begin
        _Start_carg_332 = ( _Start_counter + 10'd1 );
    end
    UInt#(1) _Stage__3___condStage__4 = fifo_Start_TO_Stage__3.first.__condStage__4;
    UInt#(2) _Stage__3__threadID = fifo_Start_TO_Stage__3.first._threadID;
    UInt#(3) _Stage__3__request_1 = ( ( _Stage__3___condStage__4 == 1'd1 ) ? ? : ( ( _Stage__3___condStage__4 == 1'd0 ) ? fifo_Stage__1_TO_Stage__3.first._request_1 : ? ) );
    Bool _Stage__3_x = ?;
    if ( ( _Stage__3___condStage__4 == 1'd0 ))
    begin
        _Stage__3_x = h.peek;
    end
    UInt#(10) _Stage__1_counter = fifo_Start_TO_Stage__1.first.counter;
    Maybe#( LockId#(4) ) _Stage__1__lock_id_h_rs = fifo_Start_TO_Stage__1.first._lock_id_h_rs;
    UInt#(2) _Stage__1__threadID = fifo_Start_TO_Stage__1.first._threadID;
    Maybe#( LockId#(4) ) _Stage__1__lock_id_h_aq = ?;
    UInt#(10) _Stage__1_carg_333 = ?;
    _Stage__1__lock_id_h_aq = _Stage__1__lock_id_h_rs;
    _Stage__1_carg_333 = _Stage__1_counter;
    rule s_Start_execute (( ( ! ( _Start___condStage__4 == 1'd1 ) ) || outputQueue.canWrite(_Start__threadID) ));
        Maybe#( LockId#(4) ) _Start__lock_id_h_rs = tagged Invalid;
        if ( ( _Start___condStage__4 == 1'd0 ))
        begin
            let __tmp_0 <- _lock_h.res1;
            _Start__lock_id_h_rs = tagged Valid __tmp_0;
            fifo__input__TO_Start.enq(E__input__TO_Start { counter : _Start_carg_332,_threadID : _Start__threadID });
        end
        if ( ( _Start___condStage__4 == 1'd1 ))
        begin
            busyReg <= False;
            outputQueue.enq(?);
        end
        fifo__input__TO_Start.deq;
        fifo_Start_TO_Stage__3.enq(E_Start_TO_Stage__3 { __condStage__4 : _Start___condStage__4,_threadID : _Start__threadID });
        if ( ( _Start___condStage__4 == 1'd0 ))
        begin
            fifo_Start_TO_Stage__1.enq(E_Start_TO_Stage__1 { counter : _Start_counter,_lock_id_h_rs : _Start__lock_id_h_rs,_threadID : _Start__threadID });
        end
        if ( ( _Start___condStage__4 == 1'd1 ))
        begin
            fifo_Start_TO_Stage__3_1.enq(E_Start_TO_Stage__3_1 { _threadID : _Start__threadID });
        end
    endrule
    rule s_Stage__3_execute (( ( ! ( _Stage__3___condStage__4 == 1'd0 ) ) || h.checkHandle(_Stage__3__request_1) ));
        if ( ( _Stage__3___condStage__4 == 1'd0 ))
        begin
            h.resp;
        end
        if ( ( _Stage__3___condStage__4 == 1'd0 ))
        begin
            fifo_Stage__1_TO_Stage__3.deq;
        end
        fifo_Start_TO_Stage__3.deq;
        if ( ( _Stage__3___condStage__4 == 1'd1 ))
        begin
            fifo_Start_TO_Stage__3_1.deq;
        end
    endrule
    rule s_Stage__1_execute (_lock_h.owns1(fromMaybe( ? , _Stage__1__lock_id_h_rs )));
        UInt#(3) _Stage__1__request_1 = ?;
        _lock_h.rel1(fromMaybe( ? , _Stage__1__lock_id_h_aq ));
        _Stage__1__request_1 <- h.req(_Stage__1_carg_333);
        fifo_Stage__1_TO_Stage__3.enq(E_Stage__1_TO_Stage__3 { _request_1 : _Stage__1__request_1,_threadID : _Stage__1__threadID });
        fifo_Start_TO_Stage__1.deq;
    endrule
    method ActionValue#(UInt#(2)) req ( UInt#(10) counter ) if( ( ! busyReg ) );
        fifo__input__TO_Start.enq(E__input__TO_Start { counter : counter,_threadID : _threadID });
        busyReg <= True;
        _threadID <= ( _threadID + 1 );
        return _threadID;
    endmethod
    method Action resp (  ) ;
        outputQueue.deq;
    endmethod
    method Bool checkHandle ( UInt#(2) handle ) ;
        return outputQueue.canRead(handle);
    endmethod
endmodule
