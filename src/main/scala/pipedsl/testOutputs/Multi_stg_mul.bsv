import FIFOF :: *;
import Locks :: *;
import Memories :: *;
import Functions :: *;

export Multi_stg_mul (..);
export mkMulti_stg_mul ;

typedef struct { UInt#(32) a1; UInt#(32) a2; UInt#(2) _threadID ; } E__input__TO_Start deriving( Bits,Eq );
typedef struct { UInt#(32) rr; UInt#(32) lr; UInt#(32) rl; UInt#(2) _threadID ; } E_Start_TO_Stage__0 deriving( Bits,Eq );
typedef struct { UInt#(2) handle; UInt#(32) data ; } OutputQueueInfo deriving( Bits,Eq );

interface Multi_stg_mul;
    method ActionValue#(UInt#(2)) req ( UInt#(32) a1, UInt#(32) a2 ) ;
    method Action resp (  ) ;
    method Bool checkHandle ( UInt#(2) handle ) ;
    method UInt#(32) peek (  ) ;
endinterface


(* synthesize *)
module mkMulti_stg_mul ( Multi_stg_mul _unused_ );
    FIFOF#( E__input__TO_Start ) fifo__input__TO_Start <- mkFIFOF (  );
    FIFOF#( E_Start_TO_Stage__0 ) fifo_Start_TO_Stage__0 <- mkFIFOF (  );
    FIFOF#( OutputQueueInfo ) outputQueue <- mkFIFOF (  );
    Reg#( UInt#(2) ) _threadID <- mkReg ( 0 );
    UInt#(32) _Start_a1 = fifo__input__TO_Start.first().a1;
    UInt#(32) _Start_a2 = fifo__input__TO_Start.first().a2;
    UInt#(2) _Start__threadID = fifo__input__TO_Start.first()._threadID;
    UInt#(32) _Start_rr = ?;
    UInt#(32) _Start_rl = ?;
    UInt#(32) _Start_lr = ?;
    _Start_rr = unsignedMul( unpack( pack( _Start_a1 ) [ 15 : 0 ] ) , unpack( pack( _Start_a2 ) [ 15 : 0 ] ) );
    _Start_rl = unsignedMul( unpack( pack( _Start_a1 ) [ 15 : 0 ] ) , unpack( pack( _Start_a2 ) [ 31 : 16 ] ) );
    _Start_lr = unsignedMul( unpack( pack( _Start_a1 ) [ 31 : 16 ] ) , unpack( pack( _Start_a2 ) [ 15 : 0 ] ) );
    UInt#(32) _Stage__0_rr = fifo_Start_TO_Stage__0.first().rr;
    UInt#(32) _Stage__0_lr = fifo_Start_TO_Stage__0.first().lr;
    UInt#(32) _Stage__0_rl = fifo_Start_TO_Stage__0.first().rl;
    UInt#(2) _Stage__0__threadID = fifo_Start_TO_Stage__0.first()._threadID;
    UInt#(32) _Stage__0_t1 = ?;
    UInt#(32) _Stage__0_res = ?;
    _Stage__0_t1 = ( _Stage__0_rl + _Stage__0_lr );
    _Stage__0_res = ( ( _Stage__0_t1 << 5'd16 ) + _Stage__0_rr );
    rule s_Start_execute ;
        fifo__input__TO_Start.deq();
        fifo_Start_TO_Stage__0.enq(E_Start_TO_Stage__0 { rr : _Start_rr,lr : _Start_lr,rl : _Start_rl,_threadID : _Start__threadID });
        $display("multi_stg_mul:Thread %d:Executing Stage Start %t", _Start__threadID,$time() );
    endrule
    rule s_Stage__0_execute ;
        outputQueue.enq(OutputQueueInfo { data : _Stage__0_res,handle : _Stage__0__threadID });
        fifo_Start_TO_Stage__0.deq();
        $display("multi_stg_mul:Thread %d:Executing Stage Stage__0 %t", _Stage__0__threadID,$time() );
    endrule
    method ActionValue#(UInt#(2)) req ( UInt#(32) a1, UInt#(32) a2 ) ;
        fifo__input__TO_Start.enq(E__input__TO_Start { a1 : a1,a2 : a2,_threadID : _threadID });
        _threadID <= ( _threadID + 1 );
        return _threadID;
    endmethod
    method Action resp (  ) ;
        outputQueue.deq();
    endmethod
    method UInt#(32) peek (  ) ;
        return outputQueue.first().data;
    endmethod
    method Bool checkHandle ( UInt#(2) handle ) ;
        return ( handle == outputQueue.first().handle );
    endmethod
endmodule
