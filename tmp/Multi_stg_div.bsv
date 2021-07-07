import FIFOF :: *;
import Locks :: *;
import Memories :: *;
import VerilogLibs :: *;
import Speculation :: *;
import Functions :: *;

export Multi_stg_div (..);
export mkMulti_stg_div ;

typedef struct { UInt#(32) num; UInt#(32) denom; UInt#(32) quot; UInt#(32) acc; UInt#(5) cnt; Bool retQuot; UInt#(1) _threadID ; } E__input__TO_Start deriving( Bits,Eq );
typedef struct { UInt#(1) handle; UInt#(32) data ; } OutputQueueInfo deriving( Bits,Eq );

interface Multi_stg_div;
    method ActionValue#(UInt#(1)) req ( UInt#(32) num, UInt#(32) denom, UInt#(32) quot, UInt#(32) acc, UInt#(5) cnt, Bool retQuot ) ;
    method Action resp (  ) ;
    method Bool checkHandle ( UInt#(1) handle ) ;
    method UInt#(32) peek (  ) ;
endinterface


(* synthesize *)
module mkMulti_stg_div ( Multi_stg_div _unused_ ) provisos(  );
    FIFOF#( E__input__TO_Start ) fifo__input__TO_Start <- mkNBFIFOF (  );
    Reg#( Bool ) busyReg <- mkReg ( False );
    FIFOF#( OutputQueueInfo ) outputQueue <- mkFIFOF (  );
    Reg#( UInt#(1) ) _threadID <- mkReg ( 0 );
    UInt#(32) _Start_quot = fifo__input__TO_Start.first().quot;
    UInt#(5) _Start_cnt = fifo__input__TO_Start.first().cnt;
    Bool _Start_retQuot = fifo__input__TO_Start.first().retQuot;
    UInt#(32) _Start_acc = fifo__input__TO_Start.first().acc;
    UInt#(32) _Start_num = fifo__input__TO_Start.first().num;
    UInt#(32) _Start_denom = fifo__input__TO_Start.first().denom;
    UInt#(1) _Start__threadID = fifo__input__TO_Start.first()._threadID;
    UInt#(32) _Start_tmp = ?;
    UInt#(32) _Start_na = ?;
    UInt#(32) _Start_nq = ?;
    UInt#(32) _Start_nnum = ?;
    UInt#(1) _Start___condStage__3 = ?;
    UInt#(32) _Start_carg = ?;
    UInt#(32) _Start_carg_0 = ?;
    UInt#(32) _Start_carg_1 = ?;
    UInt#(32) _Start_carg_2 = ?;
    UInt#(5) _Start_carg_3 = ?;
    Bool _Start_carg_4 = ?;
    _Start_tmp = unpack( { pack( _Start_acc ) [ 30 : 0 ], pack( _Start_num ) [ 31 : 31 ] } );
    _Start_na = ( ( _Start_tmp >= _Start_denom ) ? ( _Start_tmp - _Start_denom ) : _Start_tmp );
    _Start_nq = ( ( _Start_tmp >= _Start_denom ) ? unpack( { pack( ( _Start_quot << 1'd1 ) ) [ 31 : 1 ], pack( 1'd1 ) } ) : ( _Start_quot << 1'd1 ) );
    _Start_nnum = ( _Start_num << 1'd1 );
    _Start___condStage__3 = ( ( _Start_cnt == 5'd31 ) ? 1'd0 : 1'd1 );
    if ( ( _Start___condStage__3 == 1'd1 ))
    begin
        _Start_carg = _Start_nnum;
        _Start_carg_0 = _Start_denom;
        _Start_carg_1 = _Start_nq;
        _Start_carg_2 = _Start_na;
        _Start_carg_3 = ( _Start_cnt + 5'd1 );
        _Start_carg_4 = _Start_retQuot;
    end
    rule s_Start_execute (( ( ! ( _Start___condStage__3 == 1'd0 ) ) || busyReg ));
        if ( ( _Start___condStage__3 == 1'd0 ))
        begin
            busyReg <= False;
            outputQueue.enq(OutputQueueInfo { data : ( _Start_retQuot ? _Start_nq : _Start_na ),handle : _Start__threadID });
        end
        if ( ( _Start___condStage__3 == 1'd1 ))
        begin
            fifo__input__TO_Start.enq(E__input__TO_Start { acc : _Start_carg_2,cnt : _Start_carg_3,num : _Start_carg,denom : _Start_carg_0,_threadID : _Start__threadID,retQuot : _Start_carg_4,quot : _Start_carg_1 });
        end
        fifo__input__TO_Start.deq();
    endrule
    method ActionValue#(UInt#(1)) req ( UInt#(32) num, UInt#(32) denom, UInt#(32) quot, UInt#(32) acc, UInt#(5) cnt, Bool retQuot ) if( ( ! busyReg ) );
        fifo__input__TO_Start.enq(E__input__TO_Start { num : num,acc : acc,quot : quot,_threadID : _threadID,denom : denom,retQuot : retQuot,cnt : cnt });
        busyReg <= True;
        _threadID <= ( _threadID + 1 );
        return _threadID;
    endmethod
    method Action resp (  ) ;
        outputQueue.deq();
    endmethod
    method UInt#(32) peek (  ) ;
        return outputQueue.first().data;
    endmethod
    method Bool checkHandle ( UInt#(1) handle ) ;
        return ( handle == outputQueue.first().handle );
    endmethod
endmodule
