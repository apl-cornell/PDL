import FIFOF :: *;
import SpecialFIFOs :: *;
import SpecialQueues :: *;
import Locks :: *;
import Memories :: *;
import VerilogLibs :: *;
import Speculation :: *;
import RegFile :: *;
import Functions :: *;

export Hist (..);
export mkHist ;

typedef struct { UInt#(10) counter; UInt#(3) _threadID ; } E__input__TO_Start deriving( Bits,Eq );
typedef struct { UInt#(10) m1; UInt#(32) wt; UInt#(3) _threadID ; } E_Start_TO_Stage__0 deriving( Bits,Eq );
typedef struct { UInt#(32) nm; Maybe#( _lidTyp_h ) _lock_id_h_mcp_rs; UInt#(10) mcp; UInt#(3) _threadID ; } E_Stage__0_TO_Stage__1#( type _lidTyp_h ) deriving( Bits,Eq );
typedef struct { UInt#(3) _threadID ; } E_Stage__1_TO_Stage__2 deriving( Bits,Eq );

interface Hist;
    method ActionValue#(UInt#(3)) req ( UInt#(10) counter ) ;
    method Action resp (  ) ;
    method Bool checkHandle ( UInt#(3) handle ) ;
    method Bool peek (  ) ;
endinterface


module mkHist ( AddrLockCombMem#( UInt#(10), UInt#(16), _lidTyp_feature, _szParam_0_feature ) feature, AddrLockCombMem#( UInt#(10), UInt#(32), _lidTyp_weight, _szParam_0_weight ) weight, AddrLockCombMem#( UInt#(10), UInt#(32), _lidTyp_h, _szParam_0_h ) h, Hist _unused_ ) provisos( Bits#(_lidTyp_h,_sz_lidTyp_h),Bits#(_lidTyp_weight,_sz_lidTyp_weight),Bits#(_lidTyp_feature,_sz_lidTyp_feature) );
    FIFOF#( E__input__TO_Start ) fifo__input__TO_Start <- mkBypassFIFOF (  );
    FIFOF#( E_Start_TO_Stage__0 ) fifo_Start_TO_Stage__0 <- mkFIFOF (  );
    FIFOF#( E_Stage__0_TO_Stage__1#(_lidTyp_h) ) fifo_Stage__0_TO_Stage__1 <- mkFIFOF (  );
    FIFOF#( E_Stage__1_TO_Stage__2 ) fifo_Stage__1_TO_Stage__2 <- mkFIFOF (  );
    Reg#( Bool ) feature_lock_region <- mkReg ( True );
    Reg#( Bool ) weight_lock_region <- mkReg ( True );
    Reg#( Bool ) h_lock_region <- mkReg ( True );
    OutputQ#( UInt#(3), Bool ) outputQueue <- mkOutputFIFOF ( 0 );
    Reg#( UInt#(3) ) _threadID <- mkReg ( 0 );
    UInt#(10) _Start_counter = fifo__input__TO_Start.first.counter;
    UInt#(3) _Start__threadID = fifo__input__TO_Start.first._threadID;
    UInt#(16) _Start_m = ?;
    UInt#(10) _Start_m1 = ?;
    UInt#(32) _Start_wt = ?;
    _Start_m = feature.atom_r(_Start_counter);
    _Start_m1 = unpack( pack( _Start_m ) [ 9 : 0 ] );
    _Start_wt = weight.atom_r(_Start_counter);
    UInt#(10) _Stage__0_m1 = fifo_Start_TO_Stage__0.first.m1;
    UInt#(32) _Stage__0_wt = fifo_Start_TO_Stage__0.first.wt;
    UInt#(3) _Stage__0__threadID = fifo_Start_TO_Stage__0.first._threadID;
    UInt#(10) _Stage__0_mcp = ?;
    UInt#(32) _Stage__0_nm = ?;
    _Stage__0_mcp = _Stage__0_m1;
    _Stage__0_nm = ( h.atom_r(_Stage__0_m1) + _Stage__0_wt );
    UInt#(32) _Stage__1_nm = fifo_Stage__0_TO_Stage__1.first.nm;
    Maybe#( _lidTyp_h ) _Stage__1__lock_id_h_mcp_rs = fifo_Stage__0_TO_Stage__1.first._lock_id_h_mcp_rs;
    UInt#(10) _Stage__1_mcp = fifo_Stage__0_TO_Stage__1.first.mcp;
    UInt#(3) _Stage__1__threadID = fifo_Stage__0_TO_Stage__1.first._threadID;
    Maybe#( _lidTyp_h ) _Stage__1__lock_id_h_mcp_aq = ?;
    Maybe#( _lidTyp_h ) _Stage__1__lock_id_h_mcp_op = ?;
    _Stage__1__lock_id_h_mcp_aq = _Stage__1__lock_id_h_mcp_rs;
    _Stage__1__lock_id_h_mcp_op = _Stage__1__lock_id_h_mcp_aq;
    UInt#(3) _Stage__2__threadID = fifo_Stage__1_TO_Stage__2.first._threadID;
    rule s_Start_execute (feature.canAtom_r1(_Start_counter) && weight.canAtom_r1(_Start_counter));
        fifo__input__TO_Start.deq;
        fifo_Start_TO_Stage__0.enq(E_Start_TO_Stage__0 { m1 : _Start_m1,wt : _Start_wt,_threadID : _Start__threadID });
    endrule
    rule s_Stage__0_execute (h.canAtom_r1(_Stage__0_m1) && h.lock.canRes1(_Stage__0_mcp));
        Maybe#( _lidTyp_h ) _Stage__0__lock_id_h_mcp_rs = tagged Invalid;
        let __tmp_0 <- h.lock.res1(_Stage__0_mcp);
        _Stage__0__lock_id_h_mcp_rs = tagged Valid __tmp_0;
        $display( _Stage__0_nm );
        fifo_Start_TO_Stage__0.deq;
        fifo_Stage__0_TO_Stage__1.enq(E_Stage__0_TO_Stage__1 { nm : _Stage__0_nm,_lock_id_h_mcp_rs : _Stage__0__lock_id_h_mcp_rs,mcp : _Stage__0_mcp,_threadID : _Stage__0__threadID });
    endrule
    rule s_Stage__1_execute (h.lock.owns1(fromMaybe( ? , _Stage__1__lock_id_h_mcp_rs ), _Stage__1_mcp));
        h.lock.rel1(fromMaybe( ? , _Stage__1__lock_id_h_mcp_op ), _Stage__1_mcp);
        h.write(_Stage__1_mcp, _Stage__1_nm);
        fifo_Stage__0_TO_Stage__1.deq;
        fifo_Stage__1_TO_Stage__2.enq(E_Stage__1_TO_Stage__2 { _threadID : _Stage__1__threadID });
    endrule
    rule s_Stage__2_execute (outputQueue.canWrite(_Stage__2__threadID));
        outputQueue.enq(True);
        fifo_Stage__1_TO_Stage__2.deq;
    endrule
    method ActionValue#(UInt#(3)) req ( UInt#(10) counter ) ;
        fifo__input__TO_Start.enq(E__input__TO_Start { counter : counter,_threadID : _threadID });
        _threadID <= ( _threadID + 1 );
        return _threadID;
    endmethod
    method Action resp (  ) ;
        outputQueue.deq;
    endmethod
    method Bool peek (  ) ;
        return outputQueue.first;
    endmethod
    method Bool checkHandle ( UInt#(3) handle ) ;
        return outputQueue.canRead(handle);
    endmethod
endmodule
