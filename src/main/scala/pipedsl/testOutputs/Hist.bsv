import FIFOF :: *;
import Locks :: *;
import Memories :: *;
import Functions :: *;

export Hist (..);
export mkHist ;

typedef struct { UInt#(10) counter; UInt#(3) _threadID ; } E__input__TO_Start deriving( Bits,Eq );
typedef struct { UInt#(10) m; UInt#(32) wt; UInt#(3) _threadID ; } E_Start_TO_Stage__1 deriving( Bits,Eq );
typedef struct { UInt#(10) m; UInt#(32) nm; Maybe#( LockId#(4) ) _lock_id_h_m; UInt#(3) _threadID ; } E_Stage__1_TO_Stage__2 deriving( Bits,Eq );
typedef struct { UInt#(3) _threadID ; } E_Stage__2_TO_Stage__3 deriving( Bits,Eq );
typedef struct { UInt#(3) handle; Bool data ; } OutputQueueInfo deriving( Bits,Eq );

interface Hist;
    method ActionValue#(UInt#(3)) req ( UInt#(10) counter ) ;
    method Action resp (  ) ;
    method Bool checkHandle ( UInt#(3) handle ) ;
    method Bool peek (  ) ;
endinterface


module mkHist ( CombMem#( UInt#(10), UInt#(10) ) feature, CombMem#( UInt#(32), UInt#(10) ) weight, CombMem#( UInt#(32), UInt#(10) ) h, Hist _unused_ );
    FIFOF#( E__input__TO_Start ) fifo__input__TO_Start <- mkFIFOF (  );
    FIFOF#( E_Start_TO_Stage__1 ) fifo_Start_TO_Stage__1 <- mkFIFOF (  );
    FIFOF#( E_Stage__1_TO_Stage__2 ) fifo_Stage__1_TO_Stage__2 <- mkFIFOF (  );
    FIFOF#( E_Stage__2_TO_Stage__3 ) fifo_Stage__2_TO_Stage__3 <- mkFIFOF (  );
    AddrLock#( LockId#(4), UInt#(10), 4 ) feature_lock <- mkFAAddrLock (  );
    AddrLock#( LockId#(4), UInt#(10), 4 ) weight_lock <- mkFAAddrLock (  );
    AddrLock#( LockId#(4), UInt#(10), 4 ) h_lock <- mkFAAddrLock (  );
    Reg#( Bool ) feature_lock_region <- mkReg ( True );
    Reg#( Bool ) weight_lock_region <- mkReg ( True );
    Reg#( Bool ) h_lock_region <- mkReg ( True );
    Reg#( Bool ) busyReg <- mkReg ( False );
    FIFOF#( OutputQueueInfo ) outputQueue <- mkFIFOF (  );
    Reg#( UInt#(3) ) _threadID <- mkReg ( 0 );
    UInt#(10) _Start_counter = fifo__input__TO_Start.first().counter;
    UInt#(3) _Start__threadID = fifo__input__TO_Start.first()._threadID;
    Bool _Start___condStage__6 = ?;
    UInt#(10) _Start_carg = ?;
    UInt#(10) _Start_m = ?;
    UInt#(32) _Start_wt = ?;
    _Start___condStage__6 = ( _Start_counter < 10'd1000 );
    if ( _Start___condStage__6)
    begin
        _Start_carg = ( _Start_counter + 10'd1 );
        _Start_m = feature.read(_Start_counter);
        _Start_wt = weight.read(_Start_counter);
    end
    UInt#(10) _Stage__1_m = fifo_Start_TO_Stage__1.first().m;
    UInt#(32) _Stage__1_wt = fifo_Start_TO_Stage__1.first().wt;
    UInt#(3) _Stage__1__threadID = fifo_Start_TO_Stage__1.first()._threadID;
    UInt#(32) _Stage__1_nm = ?;
    _Stage__1_nm = ( h.read(_Stage__1_m) + _Stage__1_wt );
    UInt#(10) _Stage__2_m = fifo_Stage__1_TO_Stage__2.first().m;
    UInt#(32) _Stage__2_nm = fifo_Stage__1_TO_Stage__2.first().nm;
    Maybe#( LockId#(4) ) _Stage__2__lock_id_h_m = fifo_Stage__1_TO_Stage__2.first()._lock_id_h_m;
    UInt#(3) _Stage__2__threadID = fifo_Stage__1_TO_Stage__2.first()._threadID;
    UInt#(3) _Stage__3__threadID = fifo_Stage__2_TO_Stage__3.first()._threadID;
    rule s_Start_execute (( ( ! ( ! _Start___condStage__6 ) ) || busyReg ) && ( ( ! _Start___condStage__6 ) || feature_lock.isEmpty(_Start_counter) ) && ( ( ! _Start___condStage__6 ) || weight_lock.isEmpty(_Start_counter) ));
        if ( _Start___condStage__6)
        begin
            fifo__input__TO_Start.enq(E__input__TO_Start { counter : _Start_carg,_threadID : _Start__threadID });
        end
        if ( ( ! _Start___condStage__6 ))
        begin
            busyReg <= False;
            outputQueue.enq(OutputQueueInfo { data : True,handle : _Start__threadID });
        end
        fifo__input__TO_Start.deq();
        if ( _Start___condStage__6)
        begin
            fifo_Start_TO_Stage__1.enq(E_Start_TO_Stage__1 { m : _Start_m,wt : _Start_wt,_threadID : _Start__threadID });
        end
    endrule
    rule s_Stage__1_execute (h_lock.canRes(_Stage__1_m) && h_lock.isEmpty(_Stage__1_m));
        Maybe#( LockId#(4) ) _Stage__1__lock_id_h_m = ?;
        $display(_Stage__1_nm);
        _Stage__1__lock_id_h_m <- h_lock.res(_Stage__1_m);
        fifo_Stage__1_TO_Stage__2.enq(E_Stage__1_TO_Stage__2 { m : _Stage__1_m,nm : _Stage__1_nm,_lock_id_h_m : _Stage__1__lock_id_h_m,_threadID : _Stage__1__threadID });
        fifo_Start_TO_Stage__1.deq();
    endrule
    rule s_Stage__2_execute ;
        h.write(_Stage__2_m, _Stage__2_nm);
        h_lock.rel(fromMaybe( 0 , _Stage__2__lock_id_h_m ), _Stage__2_m);
        fifo_Stage__1_TO_Stage__2.deq();
        fifo_Stage__2_TO_Stage__3.enq(E_Stage__2_TO_Stage__3 { _threadID : _Stage__2__threadID });
    endrule
    rule s_Stage__3_execute ;
        fifo_Stage__2_TO_Stage__3.deq();
    endrule
    method ActionValue#(UInt#(3)) req ( UInt#(10) counter ) if( ( ! busyReg ) );
        fifo__input__TO_Start.enq(E__input__TO_Start { counter : counter,_threadID : _threadID });
        busyReg <= True;
        _threadID <= ( _threadID + 1 );
        return _threadID;
    endmethod
    method Action resp (  ) ;
        outputQueue.deq();
    endmethod
    method Bool peek (  ) ;
        return outputQueue.first().data;
    endmethod
    method Bool checkHandle ( UInt#(3) handle ) ;
        return ( handle == outputQueue.first().handle );
    endmethod
endmodule
