import FIFOF :: *;
import Locks :: *;
import Memories :: *;
import VerilogLibs :: *;
import Speculation :: *;
import Functions :: *;

export Hist (..);
export mkHist ;

typedef struct { UInt#(10) counter; UInt#(3) _threadID ; } E__input__TO_Start deriving( Bits,Eq );
typedef struct { Maybe#( _lidTyp_h ) _lock_id_h_mcp; MemId#(8) _request_3; UInt#(10) mcp; Bool done; UInt#(32) wt; UInt#(3) _threadID ; } E_Stage__1_TO_Stage__2#( type _lidTyp_h ) deriving( Bits,Eq );
typedef struct { MemId#(8) _request_0; Bool done; MemId#(8) _request_1; UInt#(3) _threadID ; } E_Start_TO_Stage__1 deriving( Bits,Eq );
typedef struct { Bool done; UInt#(3) _threadID ; } E_Start_TO_Stage__5_1 deriving( Bits,Eq );
typedef struct { Bool done; UInt#(3) _threadID ; } E_Stage__3_TO_Stage__5 deriving( Bits,Eq );
typedef struct { UInt#(1) __condStage__6; UInt#(3) _threadID ; } E_Start_TO_Stage__5 deriving( Bits,Eq );
typedef struct { MemId#(8) _request_4; Bool done; UInt#(3) _threadID ; } E_Stage__2_TO_Stage__3 deriving( Bits,Eq );
typedef struct { UInt#(3) handle; Bool data ; } OutputQueueInfo deriving( Bits,Eq );

interface Hist;
    method ActionValue#(UInt#(3)) req ( UInt#(10) counter ) ;
    method Action resp (  ) ;
    method Bool checkHandle ( UInt#(3) handle ) ;
    method Bool peek (  ) ;
endinterface


module mkHist ( AddrLockAsyncMem#( UInt#(10), UInt#(16), MemId#(8), 2, _lidTyp_feature, _szParam_0_feature ) feature, AddrLockAsyncMem#( UInt#(10), UInt#(32), MemId#(8), 4, _lidTyp_weight, _szParam_0_weight ) weight, AddrLockAsyncMem#( UInt#(10), UInt#(32), MemId#(8), 4, _lidTyp_h, _szParam_0_h ) h, Hist _unused_ ) provisos( Bits#(_lidTyp_h,_sz_lidTyp_h),Bits#(_lidTyp_weight,_sz_lidTyp_weight),Bits#(_lidTyp_feature,_sz_lidTyp_feature) );
    FIFOF#( E__input__TO_Start ) fifo__input__TO_Start <- mkNBFIFOF (  );
    FIFOF#( E_Stage__1_TO_Stage__2#(_lidTyp_h) ) fifo_Stage__1_TO_Stage__2 <- mkFIFOF (  );
    FIFOF#( E_Start_TO_Stage__1 ) fifo_Start_TO_Stage__1 <- mkFIFOF (  );
    FIFOF#( E_Start_TO_Stage__5_1 ) fifo_Start_TO_Stage__5_1 <- mkFIFOF (  );
    FIFOF#( E_Stage__3_TO_Stage__5 ) fifo_Stage__3_TO_Stage__5 <- mkFIFOF (  );
    FIFOF#( E_Start_TO_Stage__5 ) fifo_Start_TO_Stage__5 <- mkFIFOF (  );
    FIFOF#( E_Stage__2_TO_Stage__3 ) fifo_Stage__2_TO_Stage__3 <- mkFIFOF (  );
    Reg#( Bool ) feature_lock_region <- mkReg ( True );
    Reg#( Bool ) weight_lock_region <- mkReg ( True );
    Reg#( Bool ) h_lock_region <- mkReg ( True );
    Reg#( Bool ) busyReg <- mkReg ( False );
    FIFOF#( OutputQueueInfo ) outputQueue <- mkFIFOF (  );
    Reg#( UInt#(3) ) _threadID <- mkReg ( 0 );
    Maybe#( _lidTyp_h ) _Stage__2__lock_id_h_mcp = fifo_Stage__1_TO_Stage__2.first()._lock_id_h_mcp;
    MemId#(8) _Stage__2__request_3 = fifo_Stage__1_TO_Stage__2.first()._request_3;
    UInt#(10) _Stage__2_mcp = fifo_Stage__1_TO_Stage__2.first().mcp;
    Bool _Stage__2_done = fifo_Stage__1_TO_Stage__2.first().done;
    UInt#(32) _Stage__2_wt = fifo_Stage__1_TO_Stage__2.first().wt;
    UInt#(3) _Stage__2__threadID = fifo_Stage__1_TO_Stage__2.first()._threadID;
    UInt#(32) _Stage__2_oldh = ?;
    UInt#(32) _Stage__2_nm = ?;
    UInt#(10) _Stage__2__tmp_h_mcp_lock_var_W = ?;
    _Stage__2_oldh = h.mem.peekResp(_Stage__2__request_3);
    _Stage__2_nm = ( _Stage__2_oldh + _Stage__2_wt );
    _Stage__2__tmp_h_mcp_lock_var_W = _Stage__2_mcp;
    MemId#(8) _Stage__3__request_4 = fifo_Stage__2_TO_Stage__3.first()._request_4;
    Bool _Stage__3_done = fifo_Stage__2_TO_Stage__3.first().done;
    UInt#(3) _Stage__3__threadID = fifo_Stage__2_TO_Stage__3.first()._threadID;
    MemId#(8) _Stage__1__request_0 = fifo_Start_TO_Stage__1.first()._request_0;
    Bool _Stage__1_done = fifo_Start_TO_Stage__1.first().done;
    MemId#(8) _Stage__1__request_1 = fifo_Start_TO_Stage__1.first()._request_1;
    UInt#(3) _Stage__1__threadID = fifo_Start_TO_Stage__1.first()._threadID;
    UInt#(16) _Stage__1_m = ?;
    UInt#(32) _Stage__1_wt = ?;
    UInt#(10) _Stage__1_m1 = ?;
    UInt#(10) _Stage__1_mcp = ?;
    UInt#(10) _Stage__1__tmp_h_m1_lock_var_R = ?;
    _Stage__1_m = feature.mem.peekResp(_Stage__1__request_0);
    _Stage__1_wt = weight.mem.peekResp(_Stage__1__request_1);
    _Stage__1_m1 = unpack( pack( _Stage__1_m ) [ 9 : 0 ] );
    _Stage__1_mcp = unpack( pack( _Stage__1_m ) [ 9 : 0 ] );
    _Stage__1__tmp_h_m1_lock_var_R = _Stage__1_m1;
    UInt#(10) _Start_counter = fifo__input__TO_Start.first().counter;
    UInt#(3) _Start__threadID = fifo__input__TO_Start.first()._threadID;
    Bool _Start_done = ?;
    UInt#(1) _Start___condStage__6 = ?;
    UInt#(10) _Start_carg = ?;
    UInt#(10) _Start__tmp_feature_counter_lock_var_R = ?;
    UInt#(10) _Start__tmp_weight_counter_lock_var_R = ?;
    _Start_done = ( _Start_counter >= 10'd1000 );
    _Start___condStage__6 = ( ( ! _Start_done ) ? 1'd0 : 1'd1 );
    if ( ( _Start___condStage__6 == 1'd0 ))
    begin
        _Start_carg = ( _Start_counter + 10'd1 );
        _Start__tmp_feature_counter_lock_var_R = _Start_counter;
        _Start__tmp_weight_counter_lock_var_R = _Start_counter;
    end
    UInt#(1) _Stage__5___condStage__6 = fifo_Start_TO_Stage__5.first().__condStage__6;
    UInt#(3) _Stage__5__threadID = fifo_Start_TO_Stage__5.first()._threadID;
    Bool _Stage__5_done = ( ( _Stage__5___condStage__6 == 1'd1 ) ? fifo_Start_TO_Stage__5_1.first().done : ( ( _Stage__5___condStage__6 == 1'd0 ) ? fifo_Stage__3_TO_Stage__5.first().done : ? ) );
    UInt#(1) _Stage__5___condStage__10 = ?;
    _Stage__5___condStage__10 = ( _Stage__5_done ? 1'd0 : 1'd1 );
    rule s_Stage__2_execute (h.mem.checkRespId(_Stage__2__request_3));
        MemId#(8) _Stage__2__request_4 = ?;
        h.mem.resp(_Stage__2__request_3);
        $display( _Stage__2_nm );
        _Stage__2__request_4 <- h.mem.req(_Stage__2__tmp_h_mcp_lock_var_W, _Stage__2_nm, '1);
        h.lock.rel(fromMaybe( ? , _Stage__2__lock_id_h_mcp ), _Stage__2_mcp);
        fifo_Stage__1_TO_Stage__2.deq();
        fifo_Stage__2_TO_Stage__3.enq(E_Stage__2_TO_Stage__3 { _request_4 : _Stage__2__request_4,done : _Stage__2_done,_threadID : _Stage__2__threadID });
    endrule
    rule s_Stage__3_execute (h.mem.checkRespId(_Stage__3__request_4));
        h.mem.resp(_Stage__3__request_4);
        fifo_Stage__2_TO_Stage__3.deq();
        fifo_Stage__3_TO_Stage__5.enq(E_Stage__3_TO_Stage__5 { done : _Stage__3_done,_threadID : _Stage__3__threadID });
    endrule
    rule s_Stage__1_execute (h.lock.isEmpty(_Stage__1_m1) && h.lock.canRes(_Stage__1_mcp) && h.lock.isEmpty(_Stage__1_mcp) && feature.mem.checkRespId(_Stage__1__request_0) && weight.mem.checkRespId(_Stage__1__request_1));
        MemId#(8) _Stage__1__request_3 = ?;
        Maybe#( _lidTyp_h ) _Stage__1__lock_id_h_mcp = tagged Invalid;
        feature.mem.resp(_Stage__1__request_0);
        weight.mem.resp(_Stage__1__request_1);
        _Stage__1__request_3 <- h.mem.req(_Stage__1__tmp_h_m1_lock_var_R, ?, 0);
        let __tmp_0 <- h.lock.res(_Stage__1_mcp);
        _Stage__1__lock_id_h_mcp = tagged Valid __tmp_0;
        fifo_Stage__1_TO_Stage__2.enq(E_Stage__1_TO_Stage__2 { wt : _Stage__1_wt,done : _Stage__1_done,_request_3 : _Stage__1__request_3,mcp : _Stage__1_mcp,_lock_id_h_mcp : _Stage__1__lock_id_h_mcp,_threadID : _Stage__1__threadID });
        fifo_Start_TO_Stage__1.deq();
    endrule
    rule s_Start_execute (( ( ! ( _Start___condStage__6 == 1'd0 ) ) || feature.lock.isEmpty(_Start_counter) ) && ( ( ! ( _Start___condStage__6 == 1'd0 ) ) || weight.lock.isEmpty(_Start_counter) ));
        MemId#(8) _Start__request_0 = ?;
        MemId#(8) _Start__request_1 = ?;
        if ( ( _Start___condStage__6 == 1'd0 ))
        begin
            _Start__request_0 <- feature.mem.req(_Start__tmp_feature_counter_lock_var_R, ?, 0);
            _Start__request_1 <- weight.mem.req(_Start__tmp_weight_counter_lock_var_R, ?, 0);
            fifo__input__TO_Start.enq(E__input__TO_Start { counter : _Start_carg,_threadID : _Start__threadID });
        end
        fifo__input__TO_Start.deq();
        fifo_Start_TO_Stage__5.enq(E_Start_TO_Stage__5 { __condStage__6 : _Start___condStage__6,_threadID : _Start__threadID });
        if ( ( _Start___condStage__6 == 1'd0 ))
        begin
            fifo_Start_TO_Stage__1.enq(E_Start_TO_Stage__1 { _request_0 : _Start__request_0,done : _Start_done,_request_1 : _Start__request_1,_threadID : _Start__threadID });
        end
        if ( ( _Start___condStage__6 == 1'd1 ))
        begin
            fifo_Start_TO_Stage__5_1.enq(E_Start_TO_Stage__5_1 { done : _Start_done,_threadID : _Start__threadID });
        end
    endrule
    rule s_Stage__5_execute (( ( ! ( _Stage__5___condStage__10 == 1'd0 ) ) || busyReg ));
        if ( ( _Stage__5___condStage__10 == 1'd0 ))
        begin
            busyReg <= False;
            outputQueue.enq(OutputQueueInfo { data : True,handle : _Stage__5__threadID });
        end
        if ( ( _Stage__5___condStage__6 == 1'd0 ))
        begin
            fifo_Stage__3_TO_Stage__5.deq();
        end
        fifo_Start_TO_Stage__5.deq();
        if ( ( _Stage__5___condStage__6 == 1'd1 ))
        begin
            fifo_Start_TO_Stage__5_1.deq();
        end
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
