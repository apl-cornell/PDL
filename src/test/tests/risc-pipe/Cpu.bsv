import FIFOF :: *;
import SpecialFIFOs :: *;
import SpecialQueues :: *;
import Locks :: *;
import Memories :: *;
import VerilogLibs :: *;
import Speculation :: *;
import RegFile :: *;
import Functions :: *;
import Multi_stg_div :: *;

export Cpu (..);
export mkCpu ;

typedef struct { Int#(16) pc; UInt#(3) _threadID; Maybe#( SpecId#(4) ) _specId ; } E__input__TO_Start deriving( Bits,Eq );
typedef struct { MemId#(8) _request_0; Int#(16) _s_0; SpecId#(4) s; Int#(16) pc; UInt#(3) _threadID; Maybe#( SpecId#(4) ) _specId ; } E_Start_TO_Stage__0 deriving( Bits,Eq );
typedef struct { Int#(32) rf2; Int#(32) rf1; Int#(32) immU; Bool isMul; Maybe#( _lidTyp_rf ) _lock_id_rf_rd_rs; Int#(16) pc; Bool isLui; UInt#(3) funct3; Int#(32) linkpc; Bool isStore; Bool isDiv; Bool done; Bool isJalr; UInt#(5) rd; Bool isJal; Int#(32) mulres; Bool writerd; Int#(32) insn; Int#(32) alu_res; Bool isLoad; UInt#(3) _threadID; Maybe#( SpecId#(4) ) _specId ; } E_Stage__0_TO_Stage__37#( type _lidTyp_rf ) deriving( Bits,Eq );
typedef struct { UInt#(2) __condStage__50; Bool writerd; MemId#(8) _request_3; UInt#(2) boff; UInt#(1) __condStage__45; Int#(16) pc; MemId#(8) _request_4; UInt#(3) funct3; Bool invertRes; Bool done; UInt#(5) rd; Int#(32) wdata; Int#(32) rddata; Maybe#( _lidTyp_rf ) _lock_id_rf_rd_op; Maybe#( _lidTyp_rf ) _lock_id_rf_rd_rs; UInt#(32) udivout; Bool isDiv; Int#(32) insn; UInt#(1) _request_2; Bool isLoad; UInt#(3) _threadID; Maybe#( SpecId#(4) ) _specId ; } E_Stage__37_TO_Stage__51#( type _lidTyp_rf ) deriving( Bits,Eq );

interface Cpu;
    method ActionValue#(UInt#(3)) req ( Int#(16) pc ) ;
    method Action resp (  ) ;
    method Bool checkHandle ( UInt#(3) handle ) ;
    method Bool peek (  ) ;
endinterface


module mkCpu ( BypassLockCombMem#( UInt#(5), Int#(32), _lidTyp_rf, _szParam_0_rf ) rf, QueueLockAsyncMem#( UInt#(16), Int#(32), MemId#(8), 4, _lidTyp_imem ) imem, AsyncMem#( UInt#(16), Int#(32), MemId#(8), 4 ) dmem, Multi_stg_div div, Cpu _unused_ ) provisos( Bits#(_lidTyp_rf,_sz_lidTyp_rf),Bits#(_lidTyp_imem,_sz_lidTyp_imem) );
    FIFOF#( E__input__TO_Start ) fifo__input__TO_Start <- mkNBFIFOF (  );
    FIFOF#( E_Start_TO_Stage__0 ) fifo_Start_TO_Stage__0 <- mkFIFOF (  );
    FIFOF#( E_Stage__0_TO_Stage__37#(_lidTyp_rf) ) fifo_Stage__0_TO_Stage__37 <- mkFIFOF (  );
    FIFOF#( E_Stage__37_TO_Stage__51#(_lidTyp_rf) ) fifo_Stage__37_TO_Stage__51 <- mkFIFOF (  );
    Reg#( Bool ) rf_lock_region <- mkReg ( True );
    Reg#( Bool ) imem_lock_region <- mkReg ( True );
    Reg#( Bool ) dmem_lock_region <- mkReg ( True );
    Reg#( Bool ) div_lock_region <- mkReg ( True );
    CheckpointQueueLock#( LockId#(4), LockId#(4) ) _lock_div <- mkCheckpointQueueLock (  );
    Reg#( Bool ) busyReg <- mkReg ( False );
    SpecTable#( SpecId#(4), 2 ) _specTable <- mkSpecTable (  );
    OutputQ#( UInt#(3), Bool ) outputQueue <- mkOutputFIFOF ( 0 );
    Reg#( UInt#(3) ) _threadID <- mkReg ( 0 );
    Int#(16) _Start_pc = fifo__input__TO_Start.first.pc;
    UInt#(3) _Start__threadID = fifo__input__TO_Start.first._threadID;
    Maybe#( SpecId#(4) ) _Start__specId = fifo__input__TO_Start.first._specId;
    UInt#(16) _Start__tmp_0 = ?;
    UInt#(16) _Start_pcaddr = ?;
    Int#(16) _Start__s_0 = ?;
    _Start__tmp_0 = unpack( pack( _Start_pc ) );
    _Start_pcaddr = _Start__tmp_0;
    _Start__s_0 = ( _Start_pc + 16'd1 );
    MemId#(8) _Stage__0__request_0 = fifo_Start_TO_Stage__0.first._request_0;
    Int#(16) _Stage__0__s_0 = fifo_Start_TO_Stage__0.first._s_0;
    SpecId#(4) _Stage__0_s = fifo_Start_TO_Stage__0.first.s;
    Int#(16) _Stage__0_pc = fifo_Start_TO_Stage__0.first.pc;
    UInt#(3) _Stage__0__threadID = fifo_Start_TO_Stage__0.first._threadID;
    Maybe#( SpecId#(4) ) _Stage__0__specId = fifo_Start_TO_Stage__0.first._specId;
    Int#(32) _Stage__0_insn = ?;
    Bool _Stage__0_done = ?;
    Int#(7) _Stage__0_opcode = ?;
    UInt#(5) _Stage__0__tmp_1 = ?;
    UInt#(5) _Stage__0_rs1 = ?;
    UInt#(5) _Stage__0__tmp_2 = ?;
    UInt#(5) _Stage__0_rs2 = ?;
    UInt#(5) _Stage__0__tmp_3 = ?;
    UInt#(5) _Stage__0_rd = ?;
    UInt#(7) _Stage__0__tmp_4 = ?;
    UInt#(7) _Stage__0_funct7 = ?;
    UInt#(3) _Stage__0__tmp_5 = ?;
    UInt#(3) _Stage__0_funct3 = ?;
    Int#(1) _Stage__0_flipBit = ?;
    Int#(32) _Stage__0__tmp_6 = ?;
    Int#(32) _Stage__0_immI = ?;
    Int#(32) _Stage__0__tmp_7 = ?;
    Int#(32) _Stage__0_immS = ?;
    Int#(13) _Stage__0_immBTmp = ?;
    Int#(16) _Stage__0__tmp_8 = ?;
    Int#(16) _Stage__0_immB = ?;
    Int#(21) _Stage__0_immJTmp = ?;
    Int#(32) _Stage__0__tmp_9 = ?;
    Int#(32) _Stage__0_immJ = ?;
    Int#(12) _Stage__0_immJRTmp = ?;
    Int#(16) _Stage__0__tmp_10 = ?;
    Int#(16) _Stage__0_immJR = ?;
    Int#(32) _Stage__0_immU = ?;
    UInt#(3) _Stage__0_doAdd = ?;
    Bool _Stage__0_isOpImm = ?;
    Bool _Stage__0_flip = ?;
    Bool _Stage__0_isLui = ?;
    Bool _Stage__0_isAui = ?;
    Bool _Stage__0_isOp = ?;
    Bool _Stage__0_isJal = ?;
    Bool _Stage__0_isJalr = ?;
    Bool _Stage__0_isBranch = ?;
    Bool _Stage__0_isStore = ?;
    Bool _Stage__0_isLoad = ?;
    Bool _Stage__0_isMDiv = ?;
    Bool _Stage__0_isDiv = ?;
    Bool _Stage__0_isMul = ?;
    Bool _Stage__0_needrs1 = ?;
    Bool _Stage__0_needrs2 = ?;
    Bool _Stage__0_writerd = ?;
    Bool _Stage__0_notBranch = ?;
    UInt#(1) _Stage__0___condStage__4 = ?;
    UInt#(1) _Stage__0___condStage__8 = ?;
    Int#(32) _Stage__0_rf1 = ?;
    UInt#(1) _Stage__0___condStage__12 = ?;
    Int#(32) _Stage__0_rf2 = ?;
    UInt#(1) _Stage__0___condStage__16 = ?;
    Bool _Stage__0_take = ?;
    UInt#(1) _Stage__0___condStage__28 = ?;
    Int#(16) _Stage__0_offpc = ?;
    Int#(16) _Stage__0_npc = ?;
    UInt#(1) _Stage__0___condStage__26 = ?;
    Int#(32) _Stage__0__tmp_11 = ?;
    Int#(32) _Stage__0_npc32 = ?;
    UInt#(1) _Stage__0___condStage__24 = ?;
    UInt#(1) _Stage__0___condStage__36 = ?;
    UInt#(1) _Stage__0___condStage__33 = ?;
    Int#(16) _Stage__0_carg_340 = ?;
    Int#(32) _Stage__0_alu_arg1 = ?;
    Int#(32) _Stage__0_alu_arg2 = ?;
    Bool _Stage__0_alu_flip = ?;
    UInt#(3) _Stage__0_alu_funct3 = ?;
    Int#(32) _Stage__0_alu_res = ?;
    Int#(16) _Stage__0_tmppc = ?;
    Int#(32) _Stage__0_linkpc = ?;
    Int#(32) _Stage__0_mulres = ?;
    _Stage__0_insn = imem.mem.peekResp1(_Stage__0__request_0);
    _Stage__0_done = ( _Stage__0_insn == 32'h6f );
    _Stage__0_opcode = unpack( pack( _Stage__0_insn ) [ 6 : 0 ] );
    _Stage__0__tmp_1 = unpack( pack( _Stage__0_insn ) [ 19 : 15 ] );
    _Stage__0_rs1 = _Stage__0__tmp_1;
    _Stage__0__tmp_2 = unpack( pack( _Stage__0_insn ) [ 24 : 20 ] );
    _Stage__0_rs2 = _Stage__0__tmp_2;
    _Stage__0__tmp_3 = unpack( pack( _Stage__0_insn ) [ 11 : 7 ] );
    _Stage__0_rd = _Stage__0__tmp_3;
    _Stage__0__tmp_4 = unpack( pack( _Stage__0_insn ) [ 31 : 25 ] );
    _Stage__0_funct7 = _Stage__0__tmp_4;
    _Stage__0__tmp_5 = unpack( pack( _Stage__0_insn ) [ 14 : 12 ] );
    _Stage__0_funct3 = _Stage__0__tmp_5;
    _Stage__0_flipBit = unpack( pack( _Stage__0_insn ) [ 30 : 30 ] );
    _Stage__0__tmp_6 = signExtend( unpack( pack( _Stage__0_insn ) [ 31 : 20 ] ) );
    _Stage__0_immI = _Stage__0__tmp_6;
    _Stage__0__tmp_7 = signExtend( unpack( { pack( _Stage__0_insn ) [ 31 : 25 ], pack( _Stage__0_insn ) [ 11 : 7 ] } ) );
    _Stage__0_immS = _Stage__0__tmp_7;
    _Stage__0_immBTmp = unpack( { pack( _Stage__0_insn ) [ 31 : 31 ], { pack( _Stage__0_insn ) [ 7 : 7 ], { pack( _Stage__0_insn ) [ 30 : 25 ], { pack( _Stage__0_insn ) [ 11 : 8 ], pack( 1'd0 ) } } } } );
    _Stage__0__tmp_8 = signExtend( _Stage__0_immBTmp );
    _Stage__0_immB = _Stage__0__tmp_8;
    _Stage__0_immJTmp = unpack( { pack( _Stage__0_insn ) [ 31 : 31 ], { pack( _Stage__0_insn ) [ 19 : 12 ], { pack( _Stage__0_insn ) [ 20 : 20 ], { pack( _Stage__0_insn ) [ 30 : 21 ], pack( 1'd0 ) } } } } );
    _Stage__0__tmp_9 = signExtend( _Stage__0_immJTmp );
    _Stage__0_immJ = _Stage__0__tmp_9;
    _Stage__0_immJRTmp = unpack( pack( _Stage__0_insn ) [ 31 : 20 ] );
    _Stage__0__tmp_10 = signExtend( _Stage__0_immJRTmp );
    _Stage__0_immJR = _Stage__0__tmp_10;
    _Stage__0_immU = unpack( { pack( _Stage__0_insn ) [ 31 : 12 ], pack( 12'd0 ) } );
    _Stage__0_doAdd = 3'd0;
    _Stage__0_isOpImm = ( _Stage__0_opcode == 7'b10011 );
    _Stage__0_flip = ( ( ! _Stage__0_isOpImm ) && ( _Stage__0_flipBit == 1'd1 ) );
    _Stage__0_isLui = ( _Stage__0_opcode == 7'b110111 );
    _Stage__0_isAui = ( _Stage__0_opcode == 7'b10111 );
    _Stage__0_isOp = ( _Stage__0_opcode == 7'b110011 );
    _Stage__0_isJal = ( _Stage__0_opcode == 7'b1101111 );
    _Stage__0_isJalr = ( _Stage__0_opcode == 7'b1100111 );
    _Stage__0_isBranch = ( _Stage__0_opcode == 7'b1100011 );
    _Stage__0_isStore = ( _Stage__0_opcode == 7'b100011 );
    _Stage__0_isLoad = ( _Stage__0_opcode == 7'b11 );
    _Stage__0_isMDiv = ( ( _Stage__0_funct7 == 7'd1 ) && _Stage__0_isOp );
    _Stage__0_isDiv = ( _Stage__0_isMDiv && ( _Stage__0_funct3 >= 3'd4 ) );
    _Stage__0_isMul = ( _Stage__0_isMDiv && ( _Stage__0_funct3 < 3'd4 ) );
    _Stage__0_needrs1 = ( ! _Stage__0_isJal );
    _Stage__0_needrs2 = ( ( ( _Stage__0_isOp || _Stage__0_isBranch ) || _Stage__0_isStore ) || _Stage__0_isJalr );
    _Stage__0_writerd = ( ( _Stage__0_rd != 5'd0 ) && ( ( ( ( ( ( _Stage__0_isOp || _Stage__0_isOpImm ) || _Stage__0_isLoad ) || _Stage__0_isJal ) || _Stage__0_isJalr ) || _Stage__0_isLui ) || _Stage__0_isAui ) );
    _Stage__0_notBranch = ( ( ( ! _Stage__0_isBranch ) && ( ! _Stage__0_isJal ) ) && ( ! _Stage__0_isJalr ) );
    _Stage__0___condStage__4 = ( ( ( ! _Stage__0_done ) && _Stage__0_notBranch ) ? 1'd0 : 1'd1 );
    _Stage__0___condStage__8 = ( _Stage__0_needrs1 ? 1'd0 : 1'd1 );
    if ( ( _Stage__0___condStage__8 == 1'd0 ))
    begin
        _Stage__0_rf1 = rf.atom_r(_Stage__0_rs1);
    end
    if ( ( _Stage__0___condStage__8 == 1'd1 ))
    begin
        _Stage__0_rf1 = 32'd0;
    end
    _Stage__0___condStage__12 = ( _Stage__0_needrs2 ? 1'd0 : 1'd1 );
    if ( ( _Stage__0___condStage__12 == 1'd0 ))
    begin
        _Stage__0_rf2 = rf.atom_r(_Stage__0_rs2);
    end
    if ( ( _Stage__0___condStage__12 == 1'd1 ))
    begin
        _Stage__0_rf2 = 32'd0;
    end
    _Stage__0___condStage__16 = ( _Stage__0_writerd ? 1'd0 : 1'd1 );
    _Stage__0_take = br(_Stage__0_funct3, _Stage__0_rf1, _Stage__0_rf2);
    _Stage__0___condStage__28 = ( _Stage__0_isBranch ? 1'd0 : 1'd1 );
    if ( ( _Stage__0___condStage__28 == 1'd0 ))
    begin
        _Stage__0_offpc = ( _Stage__0_pc + ( _Stage__0_immB >> 2'd2 ) );
        _Stage__0_npc = ( _Stage__0_take ? _Stage__0_offpc : ( _Stage__0_pc + 16'd1 ) );
    end
    if ( ( _Stage__0___condStage__28 == 1'd1 ))
    begin
        _Stage__0___condStage__26 = ( _Stage__0_isJal ? 1'd0 : 1'd1 );
    end
    if ( ( ( _Stage__0___condStage__28 == 1'd1 ) && ( _Stage__0___condStage__26 == 1'd0 ) ))
    begin
        _Stage__0__tmp_11 = signExtend( _Stage__0_pc );
        _Stage__0_npc32 = ( _Stage__0__tmp_11 + ( _Stage__0_immJ >> 2'd2 ) );
        _Stage__0_npc = unpack( pack( _Stage__0_npc32 ) [ 15 : 0 ] );
    end
    if ( ( ( _Stage__0___condStage__28 == 1'd1 ) && ( _Stage__0___condStage__26 == 1'd1 ) ))
    begin
        _Stage__0___condStage__24 = ( _Stage__0_isJalr ? 1'd0 : 1'd1 );
    end
    if ( ( ( _Stage__0___condStage__28 == 1'd1 ) && ( ( _Stage__0___condStage__26 == 1'd1 ) && ( _Stage__0___condStage__24 == 1'd0 ) ) ))
    begin
        _Stage__0_npc = ( ( unpack( pack( _Stage__0_rf1 ) [ 15 : 0 ] ) + _Stage__0_immJR ) >> 2'd2 );
    end
    if ( ( ( _Stage__0___condStage__28 == 1'd1 ) && ( ( _Stage__0___condStage__26 == 1'd1 ) && ( _Stage__0___condStage__24 == 1'd1 ) ) ))
    begin
        _Stage__0_npc = ( _Stage__0_pc + 16'd1 );
    end
    _Stage__0___condStage__36 = ( ( ! _Stage__0_done ) ? 1'd0 : 1'd1 );
    if ( ( _Stage__0___condStage__36 == 1'd0 ))
    begin
        _Stage__0___condStage__33 = ( ( ! _Stage__0_notBranch ) ? 1'd0 : 1'd1 );
    end
    if ( ( ( _Stage__0___condStage__36 == 1'd0 ) && ( _Stage__0___condStage__33 == 1'd0 ) ))
    begin
        _Stage__0_carg_340 = _Stage__0_npc;
    end
    _Stage__0_alu_arg1 = ( _Stage__0_isAui ? ( unpack( { pack( 16'd0 ), pack( _Stage__0_pc ) } ) << 2'd2 ) : _Stage__0_rf1 );
    _Stage__0_alu_arg2 = ( _Stage__0_isAui ? _Stage__0_immU : ( _Stage__0_isStore ? _Stage__0_immS : ( ( _Stage__0_isOpImm || _Stage__0_isLoad ) ? _Stage__0_immI : _Stage__0_rf2 ) ) );
    _Stage__0_alu_flip = ( ( ( _Stage__0_isStore || _Stage__0_isLoad ) || _Stage__0_isAui ) ? False : _Stage__0_flip );
    _Stage__0_alu_funct3 = ( ( ( _Stage__0_isStore || _Stage__0_isLoad ) || _Stage__0_isAui ) ? _Stage__0_doAdd : _Stage__0_funct3 );
    _Stage__0_alu_res = alu(_Stage__0_alu_arg1, _Stage__0_alu_arg2, _Stage__0_alu_funct3, _Stage__0_alu_flip);
    _Stage__0_tmppc = ( _Stage__0_pc + 16'd1 );
    _Stage__0_linkpc = unpack( { pack( 16'd0 ), pack( ( _Stage__0_tmppc << 2'd2 ) ) } );
    _Stage__0_mulres = mul(_Stage__0_rf1, _Stage__0_rf2, _Stage__0_funct3);
    Int#(32) _Stage__37_rf2 = fifo_Stage__0_TO_Stage__37.first.rf2;
    Int#(32) _Stage__37_rf1 = fifo_Stage__0_TO_Stage__37.first.rf1;
    Int#(32) _Stage__37_immU = fifo_Stage__0_TO_Stage__37.first.immU;
    Bool _Stage__37_isMul = fifo_Stage__0_TO_Stage__37.first.isMul;
    Maybe#( _lidTyp_rf ) _Stage__37__lock_id_rf_rd_rs = fifo_Stage__0_TO_Stage__37.first._lock_id_rf_rd_rs;
    Int#(16) _Stage__37_pc = fifo_Stage__0_TO_Stage__37.first.pc;
    Bool _Stage__37_isLui = fifo_Stage__0_TO_Stage__37.first.isLui;
    UInt#(3) _Stage__37_funct3 = fifo_Stage__0_TO_Stage__37.first.funct3;
    Int#(32) _Stage__37_linkpc = fifo_Stage__0_TO_Stage__37.first.linkpc;
    Bool _Stage__37_isStore = fifo_Stage__0_TO_Stage__37.first.isStore;
    Bool _Stage__37_isDiv = fifo_Stage__0_TO_Stage__37.first.isDiv;
    Bool _Stage__37_done = fifo_Stage__0_TO_Stage__37.first.done;
    Bool _Stage__37_isJalr = fifo_Stage__0_TO_Stage__37.first.isJalr;
    UInt#(5) _Stage__37_rd = fifo_Stage__0_TO_Stage__37.first.rd;
    Bool _Stage__37_isJal = fifo_Stage__0_TO_Stage__37.first.isJal;
    Int#(32) _Stage__37_mulres = fifo_Stage__0_TO_Stage__37.first.mulres;
    Bool _Stage__37_writerd = fifo_Stage__0_TO_Stage__37.first.writerd;
    Int#(32) _Stage__37_insn = fifo_Stage__0_TO_Stage__37.first.insn;
    Int#(32) _Stage__37_alu_res = fifo_Stage__0_TO_Stage__37.first.alu_res;
    Bool _Stage__37_isLoad = fifo_Stage__0_TO_Stage__37.first.isLoad;
    UInt#(3) _Stage__37__threadID = fifo_Stage__0_TO_Stage__37.first._threadID;
    Maybe#( SpecId#(4) ) _Stage__37__specId = fifo_Stage__0_TO_Stage__37.first._specId;
    UInt#(1) _Stage__37___condStage__41 = ?;
    Maybe#( _lidTyp_rf ) _Stage__37__lock_id_rf_rd_aq = ?;
    Int#(32) _Stage__37_rddata = ?;
    Maybe#( _lidTyp_rf ) _Stage__37__lock_id_rf_rd_op = ?;
    UInt#(1) _Stage__37___condStage__45 = ?;
    Int#(32) _Stage__37_sdividend = ?;
    Int#(32) _Stage__37_sdivisor = ?;
    Bool _Stage__37_isSignedDiv = ?;
    UInt#(32) _Stage__37__tmp_12 = ?;
    UInt#(32) _Stage__37__tmp_13 = ?;
    UInt#(32) _Stage__37_dividend = ?;
    UInt#(32) _Stage__37__tmp_14 = ?;
    UInt#(32) _Stage__37__tmp_15 = ?;
    UInt#(32) _Stage__37_divisor = ?;
    Bool _Stage__37_retQuot = ?;
    Bool _Stage__37_invertRes = ?;
    UInt#(32) _Stage__37_carg_341 = ?;
    UInt#(32) _Stage__37_carg_342 = ?;
    UInt#(32) _Stage__37_carg_343 = ?;
    UInt#(32) _Stage__37_carg_344 = ?;
    UInt#(5) _Stage__37_carg_345 = ?;
    Bool _Stage__37_carg_346 = ?;
    UInt#(32) _Stage__37_udivout = ?;
    UInt#(32) _Stage__37__tmp_16 = ?;
    UInt#(32) _Stage__37_tmpaddr = ?;
    UInt#(32) _Stage__37__tmp_17 = ?;
    UInt#(16) _Stage__37_memaddr = ?;
    UInt#(2) _Stage__37__tmp_18 = ?;
    UInt#(2) _Stage__37_boff = ?;
    UInt#(2) _Stage__37___condStage__50 = ?;
    UInt#(16) _Stage__37_raddr = ?;
    UInt#(16) _Stage__37_waddr = ?;
    UInt#(5) _Stage__37_nboff = ?;
    Int#(32) _Stage__37_msg_347 = ?;
    Int#(32) _Stage__37_wdata = ?;
    _Stage__37___condStage__41 = ( ( ( _Stage__37_writerd && ( ! _Stage__37_isLoad ) ) && ( ! _Stage__37_isDiv ) ) ? 1'd0 : 1'd1 );
    if ( ( _Stage__37___condStage__41 == 1'd0 ))
    begin
        _Stage__37__lock_id_rf_rd_aq = _Stage__37__lock_id_rf_rd_rs;
        _Stage__37_rddata = ( _Stage__37_isLui ? _Stage__37_immU : ( _Stage__37_isMul ? _Stage__37_mulres : ( ( _Stage__37_isJal || _Stage__37_isJalr ) ? _Stage__37_linkpc : _Stage__37_alu_res ) ) );
        _Stage__37__lock_id_rf_rd_op = _Stage__37__lock_id_rf_rd_aq;
    end
    if ( ( _Stage__37___condStage__41 == 1'd1 ))
    begin
        _Stage__37_rddata = 32'd0;
    end
    _Stage__37___condStage__45 = ( _Stage__37_isDiv ? 1'd0 : 1'd1 );
    if ( ( _Stage__37___condStage__45 == 1'd0 ))
    begin
        _Stage__37_sdividend = signum(_Stage__37_rf1);
        _Stage__37_sdivisor = ( ( _Stage__37_funct3 == 3'd6 ) ? 32'd1 : signum(_Stage__37_rf2) );
        _Stage__37_isSignedDiv = ( ( _Stage__37_funct3 == 3'd4 ) || ( _Stage__37_funct3 == 3'd6 ) );
        _Stage__37__tmp_12 = unpack( pack( abs(_Stage__37_rf1) ) );
        _Stage__37__tmp_13 = unpack( pack( _Stage__37_rf1 ) );
        _Stage__37_dividend = ( _Stage__37_isSignedDiv ? _Stage__37__tmp_12 : _Stage__37__tmp_13 );
        _Stage__37__tmp_14 = unpack( pack( abs(_Stage__37_rf2) ) );
        _Stage__37__tmp_15 = unpack( pack( _Stage__37_rf2 ) );
        _Stage__37_divisor = ( _Stage__37_isSignedDiv ? _Stage__37__tmp_14 : _Stage__37__tmp_15 );
        _Stage__37_retQuot = ( _Stage__37_funct3 <= 3'd5 );
        _Stage__37_invertRes = ( _Stage__37_isSignedDiv && ( _Stage__37_sdividend != _Stage__37_sdivisor ) );
        _Stage__37_carg_341 = _Stage__37_dividend;
        _Stage__37_carg_342 = _Stage__37_divisor;
        _Stage__37_carg_343 = 32'd0;
        _Stage__37_carg_344 = 32'd0;
        _Stage__37_carg_345 = 5'd0;
        _Stage__37_carg_346 = _Stage__37_retQuot;
    end
    if ( ( _Stage__37___condStage__45 == 1'd1 ))
    begin
        _Stage__37_invertRes = False;
        _Stage__37_udivout = 32'd0;
    end
    _Stage__37__tmp_16 = unpack( pack( _Stage__37_alu_res ) );
    _Stage__37_tmpaddr = _Stage__37__tmp_16;
    _Stage__37__tmp_17 = ( _Stage__37_tmpaddr >> 2'd2 );
    _Stage__37_memaddr = unpack( pack( _Stage__37__tmp_17 ) [ 15 : 0 ] );
    _Stage__37__tmp_18 = unpack( pack( _Stage__37_alu_res ) [ 1 : 0 ] );
    _Stage__37_boff = _Stage__37__tmp_18;
    _Stage__37___condStage__50 = ( _Stage__37_isLoad ? 2'd0 : ( _Stage__37_isStore ? 2'd1 : 2'd2 ) );
    if ( ( _Stage__37___condStage__50 == 2'd0 ))
    begin
        _Stage__37_raddr = _Stage__37_memaddr;
    end
    if ( ( _Stage__37___condStage__50 == 2'd1 ))
    begin
        _Stage__37_waddr = _Stage__37_memaddr;
        _Stage__37_nboff = unpack( { pack( _Stage__37_boff ), pack( 3'd0 ) } );
        _Stage__37_msg_347 = ( _Stage__37_rf2 << _Stage__37_nboff );
        _Stage__37_wdata = 32'd0;
    end
    if ( ( _Stage__37___condStage__50 == 2'd2 ))
    begin
        _Stage__37_wdata = 32'd0;
    end
    UInt#(2) _Stage__51___condStage__50 = fifo_Stage__37_TO_Stage__51.first.__condStage__50;
    Bool _Stage__51_writerd = fifo_Stage__37_TO_Stage__51.first.writerd;
    MemId#(8) _Stage__51__request_3 = fifo_Stage__37_TO_Stage__51.first._request_3;
    UInt#(2) _Stage__51_boff = fifo_Stage__37_TO_Stage__51.first.boff;
    UInt#(1) _Stage__51___condStage__45 = fifo_Stage__37_TO_Stage__51.first.__condStage__45;
    Int#(16) _Stage__51_pc = fifo_Stage__37_TO_Stage__51.first.pc;
    MemId#(8) _Stage__51__request_4 = fifo_Stage__37_TO_Stage__51.first._request_4;
    UInt#(3) _Stage__51_funct3 = fifo_Stage__37_TO_Stage__51.first.funct3;
    Bool _Stage__51_invertRes = fifo_Stage__37_TO_Stage__51.first.invertRes;
    Bool _Stage__51_done = fifo_Stage__37_TO_Stage__51.first.done;
    UInt#(5) _Stage__51_rd = fifo_Stage__37_TO_Stage__51.first.rd;
    Int#(32) _Stage__51_wdata = fifo_Stage__37_TO_Stage__51.first.wdata;
    Int#(32) _Stage__51_rddata = fifo_Stage__37_TO_Stage__51.first.rddata;
    Maybe#( _lidTyp_rf ) _Stage__51__lock_id_rf_rd_op = fifo_Stage__37_TO_Stage__51.first._lock_id_rf_rd_op;
    Maybe#( _lidTyp_rf ) _Stage__51__lock_id_rf_rd_rs = fifo_Stage__37_TO_Stage__51.first._lock_id_rf_rd_rs;
    UInt#(32) _Stage__51_udivout = fifo_Stage__37_TO_Stage__51.first.udivout;
    Bool _Stage__51_isDiv = fifo_Stage__37_TO_Stage__51.first.isDiv;
    Int#(32) _Stage__51_insn = fifo_Stage__37_TO_Stage__51.first.insn;
    UInt#(1) _Stage__51__request_2 = fifo_Stage__37_TO_Stage__51.first._request_2;
    Bool _Stage__51_isLoad = fifo_Stage__37_TO_Stage__51.first.isLoad;
    UInt#(3) _Stage__51__threadID = fifo_Stage__37_TO_Stage__51.first._threadID;
    Maybe#( SpecId#(4) ) _Stage__51__specId = fifo_Stage__37_TO_Stage__51.first._specId;
    UInt#(1) _Stage__51___condStage__59 = ?;
    Maybe#( _lidTyp_rf ) _Stage__51__lock_id_rf_rd_aq = ?;
    UInt#(1) _Stage__51___condStage__56 = ?;
    Int#(32) _Stage__51_insnout = ?;
    Int#(32) _Stage__51__tmp_19 = ?;
    Int#(32) _Stage__51__tmp_20 = ?;
    Int#(32) _Stage__51_divout = ?;
    UInt#(1) _Stage__51___condStage__63 = ?;
    UInt#(1) _Stage__51___condStage__67 = ?;
    if ( ( _Stage__51___condStage__45 == 1'd0 ))
    begin
        _Stage__51_udivout = div.peek;
    end
    if ( ( _Stage__51___condStage__50 == 2'd0 ))
    begin
        _Stage__51_wdata = dmem.peekResp1(_Stage__51__request_3);
    end
    _Stage__51___condStage__59 = ( ( _Stage__51_writerd && ( _Stage__51_isLoad || _Stage__51_isDiv ) ) ? 1'd0 : 1'd1 );
    if ( ( _Stage__51___condStage__59 == 1'd0 ))
    begin
        _Stage__51__lock_id_rf_rd_aq = _Stage__51__lock_id_rf_rd_rs;
        _Stage__51___condStage__56 = ( _Stage__51_isLoad ? 1'd0 : 1'd1 );
    end
    if ( ( ( _Stage__51___condStage__59 == 1'd0 ) && ( _Stage__51___condStage__56 == 1'd0 ) ))
    begin
        _Stage__51_insnout = maskLoad(_Stage__51_wdata, _Stage__51_funct3, _Stage__51_boff);
    end
    if ( ( ( _Stage__51___condStage__59 == 1'd0 ) && ( _Stage__51___condStage__56 == 1'd1 ) ))
    begin
        _Stage__51__tmp_19 = unpack( pack( _Stage__51_udivout ) );
        _Stage__51__tmp_20 = unpack( pack( _Stage__51_udivout ) );
        _Stage__51_divout = ( _Stage__51_invertRes ? ( - _Stage__51__tmp_19 ) : _Stage__51__tmp_20 );
        _Stage__51_insnout = _Stage__51_divout;
    end
    if ( ( _Stage__51___condStage__59 == 1'd0 ))
    begin
        _Stage__51__lock_id_rf_rd_op = _Stage__51__lock_id_rf_rd_aq;
    end
    if ( ( _Stage__51___condStage__59 == 1'd1 ))
    begin
        _Stage__51_insnout = _Stage__51_rddata;
    end
    _Stage__51___condStage__63 = ( _Stage__51_writerd ? 1'd0 : 1'd1 );
    _Stage__51___condStage__67 = ( _Stage__51_done ? 1'd0 : 1'd1 );
    rule s_Start_execute (( ( ! isValid( _Start__specId ) ) || fromMaybe( True , _specTable.check(fromMaybe( ? , _Start__specId ), 1) ) ));
        SpecId#(4) _Start_s = ?;
        MemId#(8) _Start__request_0 = ?;
        _Start_s <- _specTable.alloc;
        fifo__input__TO_Start.enq(E__input__TO_Start { pc : _Start__s_0,_threadID : _Start__threadID,_specId : tagged Valid _Start_s });
        _Start__request_0 <- imem.mem.req1(_Start_pcaddr, ?, 0);
        fifo__input__TO_Start.deq;
        fifo_Start_TO_Stage__0.enq(E_Start_TO_Stage__0 { pc : _Start_pc,_specId : _Start__specId,_request_0 : _Start__request_0,_threadID : _Start__threadID,s : _Start_s,_s_0 : _Start__s_0 });
    endrule
    rule s_Start_kill (( isValid( _Start__specId ) && ( ! fromMaybe( True , _specTable.check(fromMaybe( ? , _Start__specId ), 1) ) ) ));
        fifo__input__TO_Start.deq;
        _specTable.free(fromMaybe( ? , _Start__specId ));
    endrule
    rule s_Stage__0_execute (( ( ! isValid( _Stage__0__specId ) ) || fromMaybe( False , _specTable.check(fromMaybe( ? , _Stage__0__specId ), 0) ) ) && ( ( ! ( _Stage__0___condStage__8 == 1'd0 ) ) || rf.canAtom_r1(_Stage__0_rs1) ) && ( ( ! ( _Stage__0___condStage__12 == 1'd0 ) ) || rf.canAtom_r2(_Stage__0_rs2) ) && ( ( ! ( _Stage__0___condStage__16 == 1'd0 ) ) || rf.canRes_w1(_Stage__0_rd) ) && imem.mem.checkRespId1(_Stage__0__request_0));
        Maybe#( _lidTyp_rf ) _Stage__0__lock_id_rf_rd_rs = tagged Invalid;
        imem.mem.resp1(_Stage__0__request_0);
        if ( isValid( _Stage__0__specId ))
        begin
            _specTable.free(fromMaybe( ? , _Stage__0__specId ));
        end
        if ( ( _Stage__0___condStage__4 == 1'd0 ))
        begin
            if ( ( True && ( ( _Stage__0_pc + 16'd1 ) == _Stage__0__s_0 ) ))
            begin
                _specTable.validate(_Stage__0_s, 0);
            end
            else
            begin
                _specTable.invalidate(_Stage__0_s, 0);
                fifo__input__TO_Start.enq(E__input__TO_Start { pc : ( _Stage__0_pc + 16'd1 ),_threadID : _Stage__0__threadID,_specId : tagged Invalid });
            end
        end
        if ( ( _Stage__0___condStage__4 == 1'd1 ))
        begin
            _specTable.invalidate(_Stage__0_s, 0);
        end
        if ( ( _Stage__0___condStage__16 == 1'd0 ))
        begin
            let __tmp_0 <- rf.res_w1(_Stage__0_rd);
            _Stage__0__lock_id_rf_rd_rs = tagged Valid __tmp_0;
        end
        if ( ( ( _Stage__0___condStage__36 == 1'd0 ) && ( _Stage__0___condStage__33 == 1'd0 ) ))
        begin
            fifo__input__TO_Start.enq(E__input__TO_Start { pc : _Stage__0_carg_340,_threadID : _Stage__0__threadID,_specId : tagged Invalid });
        end
        fifo_Start_TO_Stage__0.deq;
        fifo_Stage__0_TO_Stage__37.enq(E_Stage__0_TO_Stage__37 { pc : _Stage__0_pc,alu_res : _Stage__0_alu_res,isLoad : _Stage__0_isLoad,isStore : _Stage__0_isStore,_specId : _Stage__0__specId,linkpc : _Stage__0_linkpc,isDiv : _Stage__0_isDiv,_lock_id_rf_rd_rs : _Stage__0__lock_id_rf_rd_rs,done : _Stage__0_done,writerd : _Stage__0_writerd,isJalr : _Stage__0_isJalr,immU : _Stage__0_immU,rd : _Stage__0_rd,rf1 : _Stage__0_rf1,isLui : _Stage__0_isLui,insn : _Stage__0_insn,isMul : _Stage__0_isMul,rf2 : _Stage__0_rf2,_threadID : _Stage__0__threadID,mulres : _Stage__0_mulres,isJal : _Stage__0_isJal,funct3 : _Stage__0_funct3 });
    endrule
    rule s_Stage__0_kill (( isValid( _Stage__0__specId ) && ( ! fromMaybe( True , _specTable.check(fromMaybe( ? , _Stage__0__specId ), 0) ) ) ) && imem.mem.checkRespId1(_Stage__0__request_0));
        fifo_Start_TO_Stage__0.deq;
        imem.mem.resp1(_Stage__0__request_0);
        _specTable.free(fromMaybe( ? , _Stage__0__specId ));
    endrule
    rule s_Stage__37_execute (( ( ! ( _Stage__37___condStage__41 == 1'd0 ) ) || rf.owns_w1(fromMaybe( ? , _Stage__37__lock_id_rf_rd_rs )) ));
        UInt#(1) _Stage__37__request_2 = ?;
        MemId#(8) _Stage__37__request_3 = ?;
        MemId#(8) _Stage__37__request_4 = ?;
        if ( ( _Stage__37___condStage__41 == 1'd0 ))
        begin
            rf.write(fromMaybe( ? , _Stage__37__lock_id_rf_rd_aq ), _Stage__37_rddata);
        end
        if ( ( _Stage__37___condStage__45 == 1'd0 ))
        begin
            _Stage__37__request_2 <- div.req(_Stage__37_carg_341, _Stage__37_carg_342, _Stage__37_carg_343, _Stage__37_carg_344, _Stage__37_carg_345, _Stage__37_carg_346);
        end
        if ( ( _Stage__37___condStage__50 == 2'd0 ))
        begin
            _Stage__37__request_3 <- dmem.req1(_Stage__37_raddr, ?, 0);
        end
        if ( ( _Stage__37___condStage__50 == 2'd1 ))
        begin
            _Stage__37__request_4 <- dmem.req1(_Stage__37_waddr, _Stage__37_msg_347, pack( storeMask(_Stage__37_boff, _Stage__37_funct3) ));
        end
        fifo_Stage__0_TO_Stage__37.deq;
        fifo_Stage__37_TO_Stage__51.enq(E_Stage__37_TO_Stage__51 { insn : _Stage__37_insn,rddata : _Stage__37_rddata,writerd : _Stage__37_writerd,_lock_id_rf_rd_rs : _Stage__37__lock_id_rf_rd_rs,isLoad : _Stage__37_isLoad,done : _Stage__37_done,_specId : _Stage__37__specId,_request_3 : _Stage__37__request_3,__condStage__50 : _Stage__37___condStage__50,rd : _Stage__37_rd,__condStage__45 : _Stage__37___condStage__45,isDiv : _Stage__37_isDiv,_lock_id_rf_rd_op : _Stage__37__lock_id_rf_rd_op,_threadID : _Stage__37__threadID,_request_2 : _Stage__37__request_2,_request_4 : _Stage__37__request_4,boff : _Stage__37_boff,funct3 : _Stage__37_funct3,pc : _Stage__37_pc,udivout : _Stage__37_udivout,invertRes : _Stage__37_invertRes,wdata : _Stage__37_wdata });
    endrule
    rule s_Stage__51_execute (( ( ! ( _Stage__51___condStage__59 == 1'd0 ) ) || rf.owns_w1(fromMaybe( ? , _Stage__51__lock_id_rf_rd_rs )) ) && ( ( ! ( _Stage__51___condStage__67 == 1'd0 ) ) || outputQueue.canWrite(_Stage__51__threadID) ) && ( ( ! ( _Stage__51___condStage__45 == 1'd0 ) ) || div.checkHandle(_Stage__51__request_2) ) && ( ( ! ( _Stage__51___condStage__50 == 2'd1 ) ) || dmem.checkRespId1(_Stage__51__request_4) ) && ( ( ! ( _Stage__51___condStage__50 == 2'd0 ) ) || dmem.checkRespId1(_Stage__51__request_3) ));
        if ( ( _Stage__51___condStage__45 == 1'd0 ))
        begin
            div.resp;
        end
        if ( ( _Stage__51___condStage__50 == 2'd1 ))
        begin
            dmem.resp1(_Stage__51__request_4);
        end
        if ( ( _Stage__51___condStage__50 == 2'd0 ))
        begin
            dmem.resp1(_Stage__51__request_3);
        end
        $display( "PC: %h",( _Stage__51_pc << 2'd2 ) );
        $display( "INSN: %h",_Stage__51_insn );
        if ( ( _Stage__51___condStage__59 == 1'd0 ))
        begin
            rf.write(fromMaybe( ? , _Stage__51__lock_id_rf_rd_aq ), _Stage__51_insnout);
        end
        if ( ( _Stage__51___condStage__63 == 1'd0 ))
        begin
            $display( "Writing %d to r%d",_Stage__51_insnout,_Stage__51_rd );
            rf.rel_w1(fromMaybe( ? , _Stage__51__lock_id_rf_rd_op ));
        end
        if ( ( _Stage__51___condStage__67 == 1'd0 ))
        begin
            busyReg <= False;
            outputQueue.enq(True);
        end
        fifo_Stage__37_TO_Stage__51.deq;
    endrule
    method ActionValue#(UInt#(3)) req ( Int#(16) pc ) if( ( ! busyReg ) );
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
