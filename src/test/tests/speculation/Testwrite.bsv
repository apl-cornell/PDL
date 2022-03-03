import FIFOF :: *;
import SpecialFIFOs :: *;
import SpecialQueues :: *;
import Locks :: *;
import Memories :: *;
import VerilogLibs :: *;
import Speculation :: *;
import RegFile :: *;
import Functions :: *;

export Testwrite (..);
export mkTestwrite ;

typedef struct { Int#(8) pc; UInt#(3) _threadID; Maybe#( SpecId#(4) ) _specId ; } E__input__TO_Start deriving( Bits,Eq );
typedef struct { Int#(1) op; Int#(8) _s_0; SpecId#(4) s; Int#(8) brImm; Int#(8) pc; Int#(32) a1; Bool done; UInt#(5) rd; Int#(32) a2; chk_lidTyp_rf _checkpoint_rf; Int#(32) out; Maybe#( _lidTyp_rf ) _lock_id_rf_rd_op; UInt#(3) _threadID; Maybe#( SpecId#(4) ) _specId ; } E_Stage__9_TO_Stage__18#( type chk_lidTyp_rf,type _lidTyp_rf ) deriving( Bits,Eq );
typedef struct { MemId#(8) _request_0; Int#(8) _s_0; SpecId#(4) s; Int#(8) pc; UInt#(3) _threadID; Maybe#( SpecId#(4) ) _specId ; } E_Start_TO_Stage__0 deriving( Bits,Eq );
typedef struct { Int#(1) op; Int#(8) npc; Bool done; UInt#(5) rd; Int#(32) out; Maybe#( _lidTyp_rf ) _lock_id_rf_rd_op; UInt#(3) _threadID; Maybe#( SpecId#(4) ) _specId ; } E_Stage__19_TO_Stage__28#( type _lidTyp_rf ) deriving( Bits,Eq );
typedef struct { Int#(1) op; Int#(8) _s_0; SpecId#(4) s; Maybe#( _lidTyp_rf ) _lock_id_rf_rd_rs; Int#(8) brImm; Int#(8) pc; Int#(32) a1; Bool done; UInt#(5) rd; Int#(32) a2; chk_lidTyp_rf _checkpoint_rf; UInt#(3) _threadID; Maybe#( SpecId#(4) ) _specId ; } E_Stage__0_TO_Stage__9#( type _lidTyp_rf,type chk_lidTyp_rf ) deriving( Bits,Eq );
typedef struct { Bool done; UInt#(3) _threadID; Maybe#( SpecId#(4) ) _specId ; } E_Stage__28_TO_Stage__37 deriving( Bits,Eq );
typedef struct { Int#(1) op; Int#(8) _s_0; SpecId#(4) s; Int#(8) brImm; Int#(8) pc; Int#(32) a1; Bool done; UInt#(5) rd; Int#(32) a2; chk_lidTyp_rf _checkpoint_rf; Int#(32) out; Maybe#( _lidTyp_rf ) _lock_id_rf_rd_op; UInt#(3) _threadID; Maybe#( SpecId#(4) ) _specId ; } E_Stage__18_TO_Stage__19#( type chk_lidTyp_rf,type _lidTyp_rf ) deriving( Bits,Eq );

interface Testwrite;
    method ActionValue#(UInt#(3)) req ( Int#(8) pc ) ;
    method Action resp (  ) ;
    method Bool checkHandle ( UInt#(3) handle ) ;
    method Bool peek (  ) ;
endinterface


module mkTestwrite ( CheckpointQueueLockCombMem#( UInt#(5), Int#(32), _lidTyp_rf, chk_lidTyp_rf ) rf, AsyncMem#( UInt#(8), Int#(32), MemId#(8), 4 ) imem, Testwrite _unused_ ) provisos( Bits#(_lidTyp_rf,_sz_lidTyp_rf),Bits#(chk_lidTyp_rf,chk_sz_lidTyp_rf) );
    FIFOF#( E__input__TO_Start ) fifo__input__TO_Start <- mkNBFIFOF (  );
    FIFOF#( E_Stage__9_TO_Stage__18#(chk_lidTyp_rf, _lidTyp_rf) ) fifo_Stage__9_TO_Stage__18 <- mkFIFOF (  );
    FIFOF#( E_Start_TO_Stage__0 ) fifo_Start_TO_Stage__0 <- mkFIFOF (  );
    FIFOF#( E_Stage__19_TO_Stage__28#(_lidTyp_rf) ) fifo_Stage__19_TO_Stage__28 <- mkFIFOF (  );
    FIFOF#( E_Stage__0_TO_Stage__9#(_lidTyp_rf, chk_lidTyp_rf) ) fifo_Stage__0_TO_Stage__9 <- mkFIFOF (  );
    FIFOF#( E_Stage__28_TO_Stage__37 ) fifo_Stage__28_TO_Stage__37 <- mkFIFOF (  );
    FIFOF#( E_Stage__18_TO_Stage__19#(chk_lidTyp_rf, _lidTyp_rf) ) fifo_Stage__18_TO_Stage__19 <- mkFIFOF (  );
    Reg#( Bool ) rf_lock_region <- mkReg ( True );
    Reg#( Bool ) imem_lock_region <- mkReg ( True );
    Reg#( Bool ) busyReg <- mkReg ( False );
    SpecTable#( SpecId#(4), 4 ) _specTable <- mkSpecTable (  );
    OutputQ#( UInt#(3), Bool ) outputQueue <- mkOutputFIFOF ( 0 );
    Reg#( UInt#(3) ) _threadID <- mkReg ( 0 );
    Int#(1) _Stage__19_op = fifo_Stage__18_TO_Stage__19.first.op;
    Int#(8) _Stage__19__s_0 = fifo_Stage__18_TO_Stage__19.first._s_0;
    SpecId#(4) _Stage__19_s = fifo_Stage__18_TO_Stage__19.first.s;
    Int#(8) _Stage__19_brImm = fifo_Stage__18_TO_Stage__19.first.brImm;
    Int#(8) _Stage__19_pc = fifo_Stage__18_TO_Stage__19.first.pc;
    Int#(32) _Stage__19_a1 = fifo_Stage__18_TO_Stage__19.first.a1;
    Bool _Stage__19_done = fifo_Stage__18_TO_Stage__19.first.done;
    UInt#(5) _Stage__19_rd = fifo_Stage__18_TO_Stage__19.first.rd;
    Int#(32) _Stage__19_a2 = fifo_Stage__18_TO_Stage__19.first.a2;
    chk_lidTyp_rf _Stage__19__checkpoint_rf = fifo_Stage__18_TO_Stage__19.first._checkpoint_rf;
    Int#(32) _Stage__19_out = fifo_Stage__18_TO_Stage__19.first.out;
    Maybe#( _lidTyp_rf ) _Stage__19__lock_id_rf_rd_op = fifo_Stage__18_TO_Stage__19.first._lock_id_rf_rd_op;
    UInt#(3) _Stage__19__threadID = fifo_Stage__18_TO_Stage__19.first._threadID;
    Maybe#( SpecId#(4) ) _Stage__19__specId = fifo_Stage__18_TO_Stage__19.first._specId;
    Int#(8) _Stage__19_npc = ?;
    UInt#(1) _Stage__19___condStage__27 = ?;
    UInt#(1) _Stage__19___condStage__24 = ?;
    _Stage__19_npc = ( ( _Stage__19_a1 == _Stage__19_a2 ) ? ( _Stage__19_pc + _Stage__19_brImm ) : ( _Stage__19_pc + 8'd1 ) );
    _Stage__19___condStage__27 = ( ( ! _Stage__19_done ) ? 1'd0 : 1'd1 );
    if ( ( _Stage__19___condStage__27 == 1'd0 ))
    begin
        _Stage__19___condStage__24 = ( ( _Stage__19_op != 1'd0 ) ? 1'd0 : 1'd1 );
    end
    MemId#(8) _Stage__0__request_0 = fifo_Start_TO_Stage__0.first._request_0;
    Int#(8) _Stage__0__s_0 = fifo_Start_TO_Stage__0.first._s_0;
    SpecId#(4) _Stage__0_s = fifo_Start_TO_Stage__0.first.s;
    Int#(8) _Stage__0_pc = fifo_Start_TO_Stage__0.first.pc;
    UInt#(3) _Stage__0__threadID = fifo_Start_TO_Stage__0.first._threadID;
    Maybe#( SpecId#(4) ) _Stage__0__specId = fifo_Start_TO_Stage__0.first._specId;
    Int#(32) _Stage__0_insn = ?;
    Bool _Stage__0_done = ?;
    Int#(1) _Stage__0_op = ?;
    Int#(8) _Stage__0_brImm = ?;
    UInt#(5) _Stage__0__tmp_1 = ?;
    UInt#(5) _Stage__0_rs1 = ?;
    UInt#(5) _Stage__0__tmp_2 = ?;
    UInt#(5) _Stage__0_rs2 = ?;
    UInt#(5) _Stage__0__tmp_3 = ?;
    UInt#(5) _Stage__0_rd = ?;
    UInt#(1) _Stage__0___condStage__8 = ?;
    Int#(32) _Stage__0_a1 = ?;
    Int#(32) _Stage__0_a2 = ?;
    UInt#(1) _Stage__0___condStage__5 = ?;
    _Stage__0_insn = imem.peekResp1(_Stage__0__request_0);
    _Stage__0_done = ( unpack( pack( _Stage__0_insn ) [ 31 : 31 ] ) == 1'd1 );
    _Stage__0_op = unpack( pack( _Stage__0_insn ) [ 0 : 0 ] );
    _Stage__0_brImm = unpack( { pack( 4'd0 ), pack( _Stage__0_insn ) [ 4 : 1 ] } );
    _Stage__0__tmp_1 = unpack( pack( _Stage__0_insn ) [ 9 : 5 ] );
    _Stage__0_rs1 = _Stage__0__tmp_1;
    _Stage__0__tmp_2 = unpack( pack( _Stage__0_insn ) [ 14 : 10 ] );
    _Stage__0_rs2 = _Stage__0__tmp_2;
    _Stage__0__tmp_3 = unpack( pack( _Stage__0_insn ) [ 19 : 15 ] );
    _Stage__0_rd = _Stage__0__tmp_3;
    _Stage__0___condStage__8 = ( ( ! _Stage__0_done ) ? 1'd0 : 1'd1 );
    if ( ( _Stage__0___condStage__8 == 1'd0 ))
    begin
        _Stage__0_a1 = rf.atom_r(_Stage__0_rs1);
        _Stage__0_a2 = rf.atom_r(_Stage__0_rs2);
        _Stage__0___condStage__5 = ( ( _Stage__0_op == 1'd0 ) ? 1'd0 : 1'd1 );
    end
    if ( ( _Stage__0___condStage__8 == 1'd1 ))
    begin
        _Stage__0_a1 = 32'd0;
        _Stage__0_a2 = 32'd0;
    end
    Int#(1) _Stage__28_op = fifo_Stage__19_TO_Stage__28.first.op;
    Int#(8) _Stage__28_npc = fifo_Stage__19_TO_Stage__28.first.npc;
    Bool _Stage__28_done = fifo_Stage__19_TO_Stage__28.first.done;
    UInt#(5) _Stage__28_rd = fifo_Stage__19_TO_Stage__28.first.rd;
    Int#(32) _Stage__28_out = fifo_Stage__19_TO_Stage__28.first.out;
    Maybe#( _lidTyp_rf ) _Stage__28__lock_id_rf_rd_op = fifo_Stage__19_TO_Stage__28.first._lock_id_rf_rd_op;
    UInt#(3) _Stage__28__threadID = fifo_Stage__19_TO_Stage__28.first._threadID;
    Maybe#( SpecId#(4) ) _Stage__28__specId = fifo_Stage__19_TO_Stage__28.first._specId;
    UInt#(1) _Stage__28___condStage__36 = ?;
    UInt#(1) _Stage__28___condStage__33 = ?;
    _Stage__28___condStage__36 = ( ( ! _Stage__28_done ) ? 1'd0 : 1'd1 );
    if ( ( _Stage__28___condStage__36 == 1'd0 ))
    begin
        _Stage__28___condStage__33 = ( ( _Stage__28_op == 1'd0 ) ? 1'd0 : 1'd1 );
    end
    Bool _Stage__37_done = fifo_Stage__28_TO_Stage__37.first.done;
    UInt#(3) _Stage__37__threadID = fifo_Stage__28_TO_Stage__37.first._threadID;
    Maybe#( SpecId#(4) ) _Stage__37__specId = fifo_Stage__28_TO_Stage__37.first._specId;
    UInt#(1) _Stage__37___condStage__41 = ?;
    _Stage__37___condStage__41 = ( _Stage__37_done ? 1'd0 : 1'd1 );
    Int#(8) _Start_pc = fifo__input__TO_Start.first.pc;
    UInt#(3) _Start__threadID = fifo__input__TO_Start.first._threadID;
    Maybe#( SpecId#(4) ) _Start__specId = fifo__input__TO_Start.first._specId;
    UInt#(8) _Start__tmp_0 = ?;
    Int#(8) _Start__s_0 = ?;
    _Start__tmp_0 = unpack( pack( _Start_pc ) );
    _Start__s_0 = ( _Start_pc + 8'd1 );
    Int#(1) _Stage__18_op = fifo_Stage__9_TO_Stage__18.first.op;
    Int#(8) _Stage__18__s_0 = fifo_Stage__9_TO_Stage__18.first._s_0;
    SpecId#(4) _Stage__18_s = fifo_Stage__9_TO_Stage__18.first.s;
    Int#(8) _Stage__18_brImm = fifo_Stage__9_TO_Stage__18.first.brImm;
    Int#(8) _Stage__18_pc = fifo_Stage__9_TO_Stage__18.first.pc;
    Int#(32) _Stage__18_a1 = fifo_Stage__9_TO_Stage__18.first.a1;
    Bool _Stage__18_done = fifo_Stage__9_TO_Stage__18.first.done;
    UInt#(5) _Stage__18_rd = fifo_Stage__9_TO_Stage__18.first.rd;
    Int#(32) _Stage__18_a2 = fifo_Stage__9_TO_Stage__18.first.a2;
    chk_lidTyp_rf _Stage__18__checkpoint_rf = fifo_Stage__9_TO_Stage__18.first._checkpoint_rf;
    Int#(32) _Stage__18_out = fifo_Stage__9_TO_Stage__18.first.out;
    Maybe#( _lidTyp_rf ) _Stage__18__lock_id_rf_rd_op = fifo_Stage__9_TO_Stage__18.first._lock_id_rf_rd_op;
    UInt#(3) _Stage__18__threadID = fifo_Stage__9_TO_Stage__18.first._threadID;
    Maybe#( SpecId#(4) ) _Stage__18__specId = fifo_Stage__9_TO_Stage__18.first._specId;
    Int#(1) _Stage__9_op = fifo_Stage__0_TO_Stage__9.first.op;
    Int#(8) _Stage__9__s_0 = fifo_Stage__0_TO_Stage__9.first._s_0;
    SpecId#(4) _Stage__9_s = fifo_Stage__0_TO_Stage__9.first.s;
    Maybe#( _lidTyp_rf ) _Stage__9__lock_id_rf_rd_rs = fifo_Stage__0_TO_Stage__9.first._lock_id_rf_rd_rs;
    Int#(8) _Stage__9_brImm = fifo_Stage__0_TO_Stage__9.first.brImm;
    Int#(8) _Stage__9_pc = fifo_Stage__0_TO_Stage__9.first.pc;
    Int#(32) _Stage__9_a1 = fifo_Stage__0_TO_Stage__9.first.a1;
    Bool _Stage__9_done = fifo_Stage__0_TO_Stage__9.first.done;
    UInt#(5) _Stage__9_rd = fifo_Stage__0_TO_Stage__9.first.rd;
    Int#(32) _Stage__9_a2 = fifo_Stage__0_TO_Stage__9.first.a2;
    chk_lidTyp_rf _Stage__9__checkpoint_rf = fifo_Stage__0_TO_Stage__9.first._checkpoint_rf;
    UInt#(3) _Stage__9__threadID = fifo_Stage__0_TO_Stage__9.first._threadID;
    Maybe#( SpecId#(4) ) _Stage__9__specId = fifo_Stage__0_TO_Stage__9.first._specId;
    Int#(32) _Stage__9_out = ?;
    UInt#(1) _Stage__9___condStage__17 = ?;
    UInt#(1) _Stage__9___condStage__14 = ?;
    Maybe#( _lidTyp_rf ) _Stage__9__lock_id_rf_rd_aq = ?;
    Maybe#( _lidTyp_rf ) _Stage__9__lock_id_rf_rd_op = ?;
    _Stage__9_out = ( _Stage__9_a1 + _Stage__9_a2 );
    _Stage__9___condStage__17 = ( ( ! _Stage__9_done ) ? 1'd0 : 1'd1 );
    if ( ( _Stage__9___condStage__17 == 1'd0 ))
    begin
        _Stage__9___condStage__14 = ( ( _Stage__9_op == 1'd0 ) ? 1'd0 : 1'd1 );
    end
    if ( ( ( _Stage__9___condStage__17 == 1'd0 ) && ( _Stage__9___condStage__14 == 1'd0 ) ))
    begin
        _Stage__9__lock_id_rf_rd_aq = _Stage__9__lock_id_rf_rd_rs;
        _Stage__9__lock_id_rf_rd_op = _Stage__9__lock_id_rf_rd_aq;
    end
    rule s_Stage__19_execute (( ( ! isValid( _Stage__19__specId ) ) || fromMaybe( False , _specTable.check(fromMaybe( ? , _Stage__19__specId ), 0) ) ));
        if ( isValid( _Stage__19__specId ))
        begin
            _specTable.free(fromMaybe( ? , _Stage__19__specId ));
        end
        if ( ( ( _Stage__19___condStage__27 == 1'd0 ) && ( _Stage__19___condStage__24 == 1'd0 ) ))
        begin
            if ( ( True && ( _Stage__19_npc == _Stage__19__s_0 ) ))
            begin
                _specTable.validate(_Stage__19_s, 0);
                rf.lock.rollback(_Stage__19__checkpoint_rf, False, True);
            end
            else
            begin
                _specTable.invalidate(_Stage__19_s, 0);
                fifo__input__TO_Start.enq(E__input__TO_Start { pc : _Stage__19_npc,_threadID : _Stage__19__threadID,_specId : tagged Invalid });
                rf.lock.rollback(_Stage__19__checkpoint_rf, True, True);
            end
        end
        if ( ( ( _Stage__19___condStage__27 == 1'd0 ) && ( _Stage__19___condStage__24 == 1'd1 ) ))
        begin
            if ( ( True && ( ( _Stage__19_pc + 8'd1 ) == _Stage__19__s_0 ) ))
            begin
                _specTable.validate(_Stage__19_s, 0);
                rf.lock.rollback(_Stage__19__checkpoint_rf, False, True);
            end
            else
            begin
                _specTable.invalidate(_Stage__19_s, 0);
                fifo__input__TO_Start.enq(E__input__TO_Start { pc : ( _Stage__19_pc + 8'd1 ),_threadID : _Stage__19__threadID,_specId : tagged Invalid });
                rf.lock.rollback(_Stage__19__checkpoint_rf, True, True);
            end
        end
        fifo_Stage__18_TO_Stage__19.deq;
        fifo_Stage__19_TO_Stage__28.enq(E_Stage__19_TO_Stage__28 { _threadID : _Stage__19__threadID,_lock_id_rf_rd_op : _Stage__19__lock_id_rf_rd_op,rd : _Stage__19_rd,done : _Stage__19_done,_specId : _Stage__19__specId,out : _Stage__19_out,npc : _Stage__19_npc,op : _Stage__19_op });
    endrule
    rule s_Stage__19_kill (( isValid( _Stage__19__specId ) && ( ! fromMaybe( True , _specTable.check(fromMaybe( ? , _Stage__19__specId ), 0) ) ) ));
        fifo_Stage__18_TO_Stage__19.deq;
        _specTable.free(fromMaybe( ? , _Stage__19__specId ));
    endrule
    rule s_Stage__0_execute (( ( ! isValid( _Stage__0__specId ) ) || fromMaybe( True , _specTable.check(fromMaybe( ? , _Stage__0__specId ), 2) ) ) && imem.checkRespId1(_Stage__0__request_0));
        chk_lidTyp_rf _Stage__0__checkpoint_rf = ?;
        Maybe#( _lidTyp_rf ) _Stage__0__lock_id_rf_rd_rs = tagged Invalid;
        imem.resp1(_Stage__0__request_0);
        if ( ( _Stage__0___condStage__8 == 1'd0 ))
        begin
            _Stage__0__checkpoint_rf <- rf.lock.checkpoint;
        end
        if ( ( ( _Stage__0___condStage__8 == 1'd0 ) && ( _Stage__0___condStage__5 == 1'd0 ) ))
        begin
            let __tmp_0 <- rf.lock.res1;
            _Stage__0__lock_id_rf_rd_rs = tagged Valid __tmp_0;
        end
        if ( ( _Stage__0___condStage__8 == 1'd1 ))
        begin
            _specTable.invalidate(_Stage__0_s, 2);
        end
        fifo_Start_TO_Stage__0.deq;
        fifo_Stage__0_TO_Stage__9.enq(E_Stage__0_TO_Stage__9 { pc : _Stage__0_pc,a1 : _Stage__0_a1,s : _Stage__0_s,_specId : _Stage__0__specId,_checkpoint_rf : _Stage__0__checkpoint_rf,brImm : _Stage__0_brImm,_s_0 : _Stage__0__s_0,_lock_id_rf_rd_rs : _Stage__0__lock_id_rf_rd_rs,_threadID : _Stage__0__threadID,done : _Stage__0_done,op : _Stage__0_op,rd : _Stage__0_rd,a2 : _Stage__0_a2 });
    endrule
    rule s_Stage__0_kill (( isValid( _Stage__0__specId ) && ( ! fromMaybe( True , _specTable.check(fromMaybe( ? , _Stage__0__specId ), 2) ) ) ) && imem.checkRespId1(_Stage__0__request_0));
        fifo_Start_TO_Stage__0.deq;
        imem.resp1(_Stage__0__request_0);
        _specTable.free(fromMaybe( ? , _Stage__0__specId ));
    endrule
    rule s_Stage__28_execute ;
        if ( ( ( _Stage__28___condStage__36 == 1'd0 ) && ( _Stage__28___condStage__33 == 1'd0 ) ))
        begin
            $display( _Stage__28_out );
            rf.lock.rel1(fromMaybe( ? , _Stage__28__lock_id_rf_rd_op ));
        end
        if ( ( ( _Stage__28___condStage__36 == 1'd0 ) && ( _Stage__28___condStage__33 == 1'd1 ) ))
        begin
            $display( _Stage__28_npc );
        end
        fifo_Stage__19_TO_Stage__28.deq;
        fifo_Stage__28_TO_Stage__37.enq(E_Stage__28_TO_Stage__37 { done : _Stage__28_done,_threadID : _Stage__28__threadID,_specId : _Stage__28__specId });
    endrule
    rule s_Stage__37_execute (( ( ! ( _Stage__37___condStage__41 == 1'd0 ) ) || outputQueue.canWrite(_Stage__37__threadID) ));
        if ( ( _Stage__37___condStage__41 == 1'd0 ))
        begin
            busyReg <= False;
            outputQueue.enq(True);
        end
        fifo_Stage__28_TO_Stage__37.deq;
    endrule
    rule s_Start_execute (( ( ! isValid( _Start__specId ) ) || fromMaybe( True , _specTable.check(fromMaybe( ? , _Start__specId ), 3) ) ));
        SpecId#(4) _Start_s = ?;
        MemId#(8) _Start__request_0 = ?;
        _Start_s <- _specTable.alloc;
        fifo__input__TO_Start.enq(E__input__TO_Start { pc : _Start__s_0,_threadID : _Start__threadID,_specId : tagged Valid _Start_s });
        _Start__request_0 <- imem.req1(_Start__tmp_0, ?, 0);
        fifo__input__TO_Start.deq;
        fifo_Start_TO_Stage__0.enq(E_Start_TO_Stage__0 { pc : _Start_pc,_specId : _Start__specId,_request_0 : _Start__request_0,_threadID : _Start__threadID,s : _Start_s,_s_0 : _Start__s_0 });
    endrule
    rule s_Start_kill (( isValid( _Start__specId ) && ( ! fromMaybe( True , _specTable.check(fromMaybe( ? , _Start__specId ), 3) ) ) ));
        fifo__input__TO_Start.deq;
        _specTable.free(fromMaybe( ? , _Start__specId ));
    endrule
    rule s_Stage__18_execute ;
        fifo_Stage__18_TO_Stage__19.enq(E_Stage__18_TO_Stage__19 { _checkpoint_rf : _Stage__18__checkpoint_rf,_specId : _Stage__18__specId,a2 : _Stage__18_a2,brImm : _Stage__18_brImm,_lock_id_rf_rd_op : _Stage__18__lock_id_rf_rd_op,out : _Stage__18_out,a1 : _Stage__18_a1,rd : _Stage__18_rd,_threadID : _Stage__18__threadID,op : _Stage__18_op,done : _Stage__18_done,s : _Stage__18_s,pc : _Stage__18_pc,_s_0 : _Stage__18__s_0 });
        fifo_Stage__9_TO_Stage__18.deq;
    endrule
    rule s_Stage__9_execute (( ( ! isValid( _Stage__9__specId ) ) || fromMaybe( True , _specTable.check(fromMaybe( ? , _Stage__9__specId ), 1) ) ) && ( ( ! ( ( _Stage__9___condStage__17 == 1'd0 ) && ( _Stage__9___condStage__14 == 1'd0 ) ) ) || rf.lock.owns1(fromMaybe( ? , _Stage__9__lock_id_rf_rd_rs )) ));
        if ( ( ( _Stage__9___condStage__17 == 1'd0 ) && ( _Stage__9___condStage__14 == 1'd0 ) ))
        begin
            rf.write(_Stage__9_rd, _Stage__9_out);
        end
        fifo_Stage__0_TO_Stage__9.deq;
        fifo_Stage__9_TO_Stage__18.enq(E_Stage__9_TO_Stage__18 { _specId : _Stage__9__specId,op : _Stage__9_op,out : _Stage__9_out,a2 : _Stage__9_a2,_threadID : _Stage__9__threadID,done : _Stage__9_done,pc : _Stage__9_pc,s : _Stage__9_s,rd : _Stage__9_rd,brImm : _Stage__9_brImm,_checkpoint_rf : _Stage__9__checkpoint_rf,_lock_id_rf_rd_op : _Stage__9__lock_id_rf_rd_op,_s_0 : _Stage__9__s_0,a1 : _Stage__9_a1 });
    endrule
    rule s_Stage__9_kill (( isValid( _Stage__9__specId ) && ( ! fromMaybe( True , _specTable.check(fromMaybe( ? , _Stage__9__specId ), 1) ) ) ));
        fifo_Stage__0_TO_Stage__9.deq;
        _specTable.free(fromMaybe( ? , _Stage__9__specId ));
    endrule
    method ActionValue#(UInt#(3)) req ( Int#(8) pc ) if( ( ! busyReg ) );
        fifo__input__TO_Start.enq(E__input__TO_Start { pc : pc,_threadID : _threadID,_specId : tagged Invalid });
        busyReg <= True;
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
