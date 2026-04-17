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
typedef struct { Bool isAui; Bool writerd; Int#(32) immU; Bool isMul; Bool needrs1; UInt#(5) rs2; Maybe#( _lidTyp_rf ) _lock_id_rf_rd_rs; Int#(16) immB; Bool flip; Int#(32) immI; Bool isLui; UInt#(3) funct3; Bool isStore; Bool isOpImm; Bool done; Bool isJalr; Bool notBranch; Bool isJal; Int#(32) immS; Maybe#( _lidTyp_rf ) _lock_id_rf_rs2_rs; Int#(32) immJ; SpecId#(4) s2; UInt#(5) rs1; Int#(16) _s2_0; Maybe#( _lidTyp_rf ) _lock_id_rf_rs1_rs; Int#(16) pc; Bool needrs2; Bool isBranch; Bool isDiv; Int#(16) immJR; UInt#(5) rd; UInt#(3) doAdd; Int#(32) insn; Bool isLoad; UInt#(3) _threadID; Maybe#( SpecId#(4) ) _specId ; } E_Stage__0_TO_Stage__25#( type _lidTyp_rf ) deriving( Bits,Eq );
typedef struct { MemId#(8) _request_0; Int#(16) _s_0; SpecId#(4) s; Int#(16) pc; UInt#(3) _threadID; Maybe#( SpecId#(4) ) _specId ; } E_Start_TO_Stage__0 deriving( Bits,Eq );
typedef struct { Bool writerd; Maybe#( _lidTyp_rf ) _lock_id_rf_rd_op; UInt#(5) rd; Bool done; UInt#(3) _threadID; Maybe#( SpecId#(4) ) _specId ; } E_Stage__72_TO_Stage__85#( type _lidTyp_rf ) deriving( Bits,Eq );
typedef struct { Int#(32) rf2; Int#(32) rf1; Bool writerd; Int#(32) rddata; Int#(16) pc; UInt#(3) funct3; Bool isStore; Bool isDiv; Bool done; UInt#(5) rd; Maybe#( _lidTyp_rf ) _lock_id_rf_rd_op; Maybe#( _lidTyp_rf ) _lock_id_rf_rd_rs; Int#(32) insn; Int#(32) alu_res; Bool isLoad; UInt#(3) _threadID; Maybe#( SpecId#(4) ) _specId ; } E_Stage__25_TO_Stage__62#( type _lidTyp_rf ) deriving( Bits,Eq );
typedef struct { UInt#(2) __condStage__71; Bool writerd; MemId#(8) _request_3; UInt#(2) boff; UInt#(1) __condStage__66; Int#(16) pc; MemId#(8) _request_4; UInt#(3) funct3; Bool invertRes; Bool done; UInt#(5) rd; Int#(32) wdata; Int#(32) rddata; Maybe#( _lidTyp_rf ) _lock_id_rf_rd_op; Maybe#( _lidTyp_rf ) _lock_id_rf_rd_rs; UInt#(32) udivout; Bool isDiv; Int#(32) insn; UInt#(1) _request_2; Bool isLoad; UInt#(3) _threadID; Maybe#( SpecId#(4) ) _specId ; } E_Stage__62_TO_Stage__72#( type _lidTyp_rf ) deriving( Bits,Eq );

interface Cpu;
    method ActionValue#(UInt#(3)) req ( Int#(16) pc ) ;
    method Action resp (  ) ;
    method Bool checkHandle ( UInt#(3) handle ) ;
    method Bool peek (  ) ;
endinterface


module mkCpu ( RenameRF#( UInt#(5), Int#(32), _lidTyp_rf ) rf, AsyncMem#( UInt#(16), Int#(32), MemId#(8), 4 ) imem, AsyncMem#( UInt#(16), Int#(32), MemId#(8), 4 ) dmem, Multi_stg_div div, BHT#( 16 ) bht, Cpu _unused_ ) provisos( Bits#(_lidTyp_rf,_sz_lidTyp_rf) );
    FIFOF#( E__input__TO_Start ) fifo__input__TO_Start <- mkNBFIFOF (  );
    FIFOF#( E_Stage__0_TO_Stage__25#(_lidTyp_rf) ) fifo_Stage__0_TO_Stage__25 <- mkFIFOF (  );
    FIFOF#( E_Start_TO_Stage__0 ) fifo_Start_TO_Stage__0 <- mkFIFOF (  );
    FIFOF#( E_Stage__72_TO_Stage__85#(_lidTyp_rf) ) fifo_Stage__72_TO_Stage__85 <- mkFIFOF (  );
    FIFOF#( E_Stage__25_TO_Stage__62#(_lidTyp_rf) ) fifo_Stage__25_TO_Stage__62 <- mkFIFOF (  );
    FIFOF#( E_Stage__62_TO_Stage__72#(_lidTyp_rf) ) fifo_Stage__62_TO_Stage__72 <- mkFIFOF (  );
    Reg#( Bool ) rf_lock_region <- mkReg ( True );
    Reg#( Bool ) dmem_lock_region <- mkReg ( True );
    Reg#( Bool ) imem_lock_region <- mkReg ( True );
    Reg#( Bool ) div_lock_region <- mkReg ( True );
    Reg#( Bool ) bht_lock_region <- mkReg ( True );
    CheckpointQueueLock#( LockId#(4), LockId#(4) ) _lock_div <- mkCheckpointQueueLock (  );
    Reg#( Bool ) busyReg <- mkReg ( False );
    SpecTable#( SpecId#(4), 3 ) _specTable <- mkSpecTable (  );
    OutputQ#( UInt#(3), Bool ) outputQueue <- mkOutputFIFOF ( 0 );
    Reg#( UInt#(3) ) _threadID <- mkReg ( 0 );
    Bool _Stage__85_writerd = fifo_Stage__72_TO_Stage__85.first.writerd;
    Maybe#( _lidTyp_rf ) _Stage__85__lock_id_rf_rd_op = fifo_Stage__72_TO_Stage__85.first._lock_id_rf_rd_op;
    UInt#(5) _Stage__85_rd = fifo_Stage__72_TO_Stage__85.first.rd;
    Bool _Stage__85_done = fifo_Stage__72_TO_Stage__85.first.done;
    UInt#(3) _Stage__85__threadID = fifo_Stage__72_TO_Stage__85.first._threadID;
    Maybe#( SpecId#(4) ) _Stage__85__specId = fifo_Stage__72_TO_Stage__85.first._specId;
    UInt#(1) _Stage__85___condStage__89 = ?;
    UInt#(1) _Stage__85___condStage__93 = ?;
    _Stage__85___condStage__89 = ( _Stage__85_writerd ? 1'd0 : 1'd1 );
    _Stage__85___condStage__93 = ( _Stage__85_done ? 1'd0 : 1'd1 );
    Bool _Stage__25_isAui = fifo_Stage__0_TO_Stage__25.first.isAui;
    Bool _Stage__25_writerd = fifo_Stage__0_TO_Stage__25.first.writerd;
    Int#(32) _Stage__25_immU = fifo_Stage__0_TO_Stage__25.first.immU;
    Bool _Stage__25_isMul = fifo_Stage__0_TO_Stage__25.first.isMul;
    Bool _Stage__25_needrs1 = fifo_Stage__0_TO_Stage__25.first.needrs1;
    UInt#(5) _Stage__25_rs2 = fifo_Stage__0_TO_Stage__25.first.rs2;
    Maybe#( _lidTyp_rf ) _Stage__25__lock_id_rf_rd_rs = fifo_Stage__0_TO_Stage__25.first._lock_id_rf_rd_rs;
    Int#(16) _Stage__25_immB = fifo_Stage__0_TO_Stage__25.first.immB;
    Bool _Stage__25_flip = fifo_Stage__0_TO_Stage__25.first.flip;
    Int#(32) _Stage__25_immI = fifo_Stage__0_TO_Stage__25.first.immI;
    Bool _Stage__25_isLui = fifo_Stage__0_TO_Stage__25.first.isLui;
    UInt#(3) _Stage__25_funct3 = fifo_Stage__0_TO_Stage__25.first.funct3;
    Bool _Stage__25_isStore = fifo_Stage__0_TO_Stage__25.first.isStore;
    Bool _Stage__25_isOpImm = fifo_Stage__0_TO_Stage__25.first.isOpImm;
    Bool _Stage__25_done = fifo_Stage__0_TO_Stage__25.first.done;
    Bool _Stage__25_isJalr = fifo_Stage__0_TO_Stage__25.first.isJalr;
    Bool _Stage__25_notBranch = fifo_Stage__0_TO_Stage__25.first.notBranch;
    Bool _Stage__25_isJal = fifo_Stage__0_TO_Stage__25.first.isJal;
    Int#(32) _Stage__25_immS = fifo_Stage__0_TO_Stage__25.first.immS;
    Maybe#( _lidTyp_rf ) _Stage__25__lock_id_rf_rs2_rs = fifo_Stage__0_TO_Stage__25.first._lock_id_rf_rs2_rs;
    Int#(32) _Stage__25_immJ = fifo_Stage__0_TO_Stage__25.first.immJ;
    SpecId#(4) _Stage__25_s2 = fifo_Stage__0_TO_Stage__25.first.s2;
    UInt#(5) _Stage__25_rs1 = fifo_Stage__0_TO_Stage__25.first.rs1;
    Int#(16) _Stage__25__s2_0 = fifo_Stage__0_TO_Stage__25.first._s2_0;
    Maybe#( _lidTyp_rf ) _Stage__25__lock_id_rf_rs1_rs = fifo_Stage__0_TO_Stage__25.first._lock_id_rf_rs1_rs;
    Int#(16) _Stage__25_pc = fifo_Stage__0_TO_Stage__25.first.pc;
    Bool _Stage__25_needrs2 = fifo_Stage__0_TO_Stage__25.first.needrs2;
    Bool _Stage__25_isBranch = fifo_Stage__0_TO_Stage__25.first.isBranch;
    Bool _Stage__25_isDiv = fifo_Stage__0_TO_Stage__25.first.isDiv;
    Int#(16) _Stage__25_immJR = fifo_Stage__0_TO_Stage__25.first.immJR;
    UInt#(5) _Stage__25_rd = fifo_Stage__0_TO_Stage__25.first.rd;
    UInt#(3) _Stage__25_doAdd = fifo_Stage__0_TO_Stage__25.first.doAdd;
    Int#(32) _Stage__25_insn = fifo_Stage__0_TO_Stage__25.first.insn;
    Bool _Stage__25_isLoad = fifo_Stage__0_TO_Stage__25.first.isLoad;
    UInt#(3) _Stage__25__threadID = fifo_Stage__0_TO_Stage__25.first._threadID;
    Maybe#( SpecId#(4) ) _Stage__25__specId = fifo_Stage__0_TO_Stage__25.first._specId;
    UInt#(1) _Stage__25___condStage__29 = ?;
    Maybe#( _lidTyp_rf ) _Stage__25__lock_id_rf_rs1_aq = ?;
    Int#(32) _Stage__25_rf1 = ?;
    UInt#(1) _Stage__25___condStage__33 = ?;
    Maybe#( _lidTyp_rf ) _Stage__25__lock_id_rf_rs2_aq = ?;
    Int#(32) _Stage__25_rf2 = ?;
    Bool _Stage__25_take = ?;
    UInt#(1) _Stage__25___condStage__45 = ?;
    Int#(16) _Stage__25_offpc = ?;
    Int#(16) _Stage__25_npc = ?;
    UInt#(1) _Stage__25___condStage__43 = ?;
    Int#(32) _Stage__25__tmp_11 = ?;
    Int#(32) _Stage__25_npc32 = ?;
    UInt#(1) _Stage__25___condStage__41 = ?;
    UInt#(1) _Stage__25___condStage__57 = ?;
    UInt#(1) _Stage__25___condStage__54 = ?;
    UInt#(1) _Stage__25___condStage__51 = ?;
    Int#(16) _Stage__25_carg_1309 = ?;
    Int#(32) _Stage__25_alu_arg1 = ?;
    Int#(32) _Stage__25_alu_arg2 = ?;
    Bool _Stage__25_alu_flip = ?;
    UInt#(3) _Stage__25_alu_funct3 = ?;
    Int#(32) _Stage__25_alu_res = ?;
    Int#(16) _Stage__25_tmppc = ?;
    Int#(32) _Stage__25_linkpc = ?;
    Int#(32) _Stage__25_mulres = ?;
    UInt#(1) _Stage__25___condStage__61 = ?;
    Maybe#( _lidTyp_rf ) _Stage__25__lock_id_rf_rd_aq = ?;
    Int#(32) _Stage__25_rddata = ?;
    Maybe#( _lidTyp_rf ) _Stage__25__lock_id_rf_rd_op = ?;
    _Stage__25___condStage__29 = ( _Stage__25_needrs1 ? 1'd0 : 1'd1 );
    if ( ( _Stage__25___condStage__29 == 1'd0 ))
    begin
        _Stage__25__lock_id_rf_rs1_aq = _Stage__25__lock_id_rf_rs1_rs;
        _Stage__25_rf1 = rf.read(fromMaybe( ? , _Stage__25__lock_id_rf_rs1_aq ));
    end
    if ( ( _Stage__25___condStage__29 == 1'd1 ))
    begin
        _Stage__25_rf1 = 32'd0;
    end
    _Stage__25___condStage__33 = ( _Stage__25_needrs2 ? 1'd0 : 1'd1 );
    if ( ( _Stage__25___condStage__33 == 1'd0 ))
    begin
        _Stage__25__lock_id_rf_rs2_aq = _Stage__25__lock_id_rf_rs2_rs;
        _Stage__25_rf2 = rf.read(fromMaybe( ? , _Stage__25__lock_id_rf_rs2_aq ));
    end
    if ( ( _Stage__25___condStage__33 == 1'd1 ))
    begin
        _Stage__25_rf2 = 32'd0;
    end
    _Stage__25_take = br(_Stage__25_funct3, _Stage__25_rf1, _Stage__25_rf2);
    _Stage__25___condStage__45 = ( _Stage__25_isBranch ? 1'd0 : 1'd1 );
    if ( ( _Stage__25___condStage__45 == 1'd0 ))
    begin
        _Stage__25_offpc = ( _Stage__25_pc + ( _Stage__25_immB >> 2'd2 ) );
        _Stage__25_npc = ( _Stage__25_take ? _Stage__25_offpc : ( _Stage__25_pc + 16'd1 ) );
    end
    if ( ( _Stage__25___condStage__45 == 1'd1 ))
    begin
        _Stage__25___condStage__43 = ( _Stage__25_isJal ? 1'd0 : 1'd1 );
    end
    if ( ( ( _Stage__25___condStage__45 == 1'd1 ) && ( _Stage__25___condStage__43 == 1'd0 ) ))
    begin
        _Stage__25__tmp_11 = signExtend( _Stage__25_pc );
        _Stage__25_npc32 = ( _Stage__25__tmp_11 + ( _Stage__25_immJ >> 2'd2 ) );
        _Stage__25_npc = unpack( pack( _Stage__25_npc32 ) [ 15 : 0 ] );
    end
    if ( ( ( _Stage__25___condStage__45 == 1'd1 ) && ( _Stage__25___condStage__43 == 1'd1 ) ))
    begin
        _Stage__25___condStage__41 = ( _Stage__25_isJalr ? 1'd0 : 1'd1 );
    end
    if ( ( ( _Stage__25___condStage__45 == 1'd1 ) && ( ( _Stage__25___condStage__43 == 1'd1 ) && ( _Stage__25___condStage__41 == 1'd0 ) ) ))
    begin
        _Stage__25_npc = ( ( unpack( pack( _Stage__25_rf1 ) [ 15 : 0 ] ) + _Stage__25_immJR ) >> 2'd2 );
    end
    if ( ( ( _Stage__25___condStage__45 == 1'd1 ) && ( ( _Stage__25___condStage__43 == 1'd1 ) && ( _Stage__25___condStage__41 == 1'd1 ) ) ))
    begin
        _Stage__25_npc = ( _Stage__25_pc + 16'd1 );
    end
    _Stage__25___condStage__57 = ( ( ! _Stage__25_done ) ? 1'd0 : 1'd1 );
    if ( ( _Stage__25___condStage__57 == 1'd0 ))
    begin
        _Stage__25___condStage__54 = ( ( ! _Stage__25_notBranch ) ? 1'd0 : 1'd1 );
    end
    if ( ( ( _Stage__25___condStage__57 == 1'd0 ) && ( _Stage__25___condStage__54 == 1'd0 ) ))
    begin
        _Stage__25___condStage__51 = ( _Stage__25_isBranch ? 1'd0 : 1'd1 );
    end
    if ( ( ( _Stage__25___condStage__57 == 1'd0 ) && ( ( _Stage__25___condStage__54 == 1'd0 ) && ( _Stage__25___condStage__51 == 1'd1 ) ) ))
    begin
        _Stage__25_carg_1309 = _Stage__25_npc;
    end
    _Stage__25_alu_arg1 = ( _Stage__25_isAui ? ( unpack( { pack( 16'd0 ), pack( _Stage__25_pc ) } ) << 2'd2 ) : _Stage__25_rf1 );
    _Stage__25_alu_arg2 = ( _Stage__25_isAui ? _Stage__25_immU : ( _Stage__25_isStore ? _Stage__25_immS : ( ( _Stage__25_isOpImm || _Stage__25_isLoad ) ? _Stage__25_immI : _Stage__25_rf2 ) ) );
    _Stage__25_alu_flip = ( ( ( _Stage__25_isStore || _Stage__25_isLoad ) || _Stage__25_isAui ) ? False : _Stage__25_flip );
    _Stage__25_alu_funct3 = ( ( ( _Stage__25_isStore || _Stage__25_isLoad ) || _Stage__25_isAui ) ? _Stage__25_doAdd : _Stage__25_funct3 );
    _Stage__25_alu_res = alu(_Stage__25_alu_arg1, _Stage__25_alu_arg2, _Stage__25_alu_funct3, _Stage__25_alu_flip);
    _Stage__25_tmppc = ( _Stage__25_pc + 16'd1 );
    _Stage__25_linkpc = unpack( { pack( 16'd0 ), pack( ( _Stage__25_tmppc << 2'd2 ) ) } );
    _Stage__25_mulres = mul(_Stage__25_rf1, _Stage__25_rf2, _Stage__25_funct3);
    _Stage__25___condStage__61 = ( ( ( _Stage__25_writerd && ( ! _Stage__25_isLoad ) ) && ( ! _Stage__25_isDiv ) ) ? 1'd0 : 1'd1 );
    if ( ( _Stage__25___condStage__61 == 1'd0 ))
    begin
        _Stage__25__lock_id_rf_rd_aq = _Stage__25__lock_id_rf_rd_rs;
        _Stage__25_rddata = ( _Stage__25_isLui ? _Stage__25_immU : ( _Stage__25_isMul ? _Stage__25_mulres : ( ( _Stage__25_isJal || _Stage__25_isJalr ) ? _Stage__25_linkpc : _Stage__25_alu_res ) ) );
        _Stage__25__lock_id_rf_rd_op = _Stage__25__lock_id_rf_rd_aq;
    end
    if ( ( _Stage__25___condStage__61 == 1'd1 ))
    begin
        _Stage__25_rddata = 32'd0;
    end
    Int#(32) _Stage__62_rf2 = fifo_Stage__25_TO_Stage__62.first.rf2;
    Int#(32) _Stage__62_rf1 = fifo_Stage__25_TO_Stage__62.first.rf1;
    Bool _Stage__62_writerd = fifo_Stage__25_TO_Stage__62.first.writerd;
    Int#(32) _Stage__62_rddata = fifo_Stage__25_TO_Stage__62.first.rddata;
    Int#(16) _Stage__62_pc = fifo_Stage__25_TO_Stage__62.first.pc;
    UInt#(3) _Stage__62_funct3 = fifo_Stage__25_TO_Stage__62.first.funct3;
    Bool _Stage__62_isStore = fifo_Stage__25_TO_Stage__62.first.isStore;
    Bool _Stage__62_isDiv = fifo_Stage__25_TO_Stage__62.first.isDiv;
    Bool _Stage__62_done = fifo_Stage__25_TO_Stage__62.first.done;
    UInt#(5) _Stage__62_rd = fifo_Stage__25_TO_Stage__62.first.rd;
    Maybe#( _lidTyp_rf ) _Stage__62__lock_id_rf_rd_op = fifo_Stage__25_TO_Stage__62.first._lock_id_rf_rd_op;
    Maybe#( _lidTyp_rf ) _Stage__62__lock_id_rf_rd_rs = fifo_Stage__25_TO_Stage__62.first._lock_id_rf_rd_rs;
    Int#(32) _Stage__62_insn = fifo_Stage__25_TO_Stage__62.first.insn;
    Int#(32) _Stage__62_alu_res = fifo_Stage__25_TO_Stage__62.first.alu_res;
    Bool _Stage__62_isLoad = fifo_Stage__25_TO_Stage__62.first.isLoad;
    UInt#(3) _Stage__62__threadID = fifo_Stage__25_TO_Stage__62.first._threadID;
    Maybe#( SpecId#(4) ) _Stage__62__specId = fifo_Stage__25_TO_Stage__62.first._specId;
    UInt#(1) _Stage__62___condStage__66 = ?;
    Int#(32) _Stage__62_sdividend = ?;
    Int#(32) _Stage__62_sdivisor = ?;
    Bool _Stage__62_isSignedDiv = ?;
    UInt#(32) _Stage__62__tmp_12 = ?;
    UInt#(32) _Stage__62__tmp_13 = ?;
    UInt#(32) _Stage__62_dividend = ?;
    UInt#(32) _Stage__62__tmp_14 = ?;
    UInt#(32) _Stage__62__tmp_15 = ?;
    UInt#(32) _Stage__62_divisor = ?;
    Bool _Stage__62_retQuot = ?;
    Bool _Stage__62_invertRes = ?;
    UInt#(32) _Stage__62_carg_1310 = ?;
    UInt#(32) _Stage__62_carg_1311 = ?;
    UInt#(32) _Stage__62_carg_1312 = ?;
    UInt#(32) _Stage__62_carg_1313 = ?;
    UInt#(5) _Stage__62_carg_1314 = ?;
    Bool _Stage__62_carg_1315 = ?;
    UInt#(32) _Stage__62_udivout = ?;
    UInt#(32) _Stage__62__tmp_16 = ?;
    UInt#(32) _Stage__62_tmpaddr = ?;
    UInt#(32) _Stage__62__tmp_17 = ?;
    UInt#(16) _Stage__62_memaddr = ?;
    UInt#(2) _Stage__62__tmp_18 = ?;
    UInt#(2) _Stage__62_boff = ?;
    UInt#(2) _Stage__62___condStage__71 = ?;
    UInt#(16) _Stage__62_raddr = ?;
    UInt#(16) _Stage__62_waddr = ?;
    UInt#(5) _Stage__62_nboff = ?;
    Int#(32) _Stage__62_msg_1316 = ?;
    Int#(32) _Stage__62_wdata = ?;
    _Stage__62___condStage__66 = ( _Stage__62_isDiv ? 1'd0 : 1'd1 );
    if ( ( _Stage__62___condStage__66 == 1'd0 ))
    begin
        _Stage__62_sdividend = signum(_Stage__62_rf1);
        _Stage__62_sdivisor = ( ( _Stage__62_funct3 == 3'd6 ) ? 32'd1 : signum(_Stage__62_rf2) );
        _Stage__62_isSignedDiv = ( ( _Stage__62_funct3 == 3'd4 ) || ( _Stage__62_funct3 == 3'd6 ) );
        _Stage__62__tmp_12 = unpack( pack( abs(_Stage__62_rf1) ) );
        _Stage__62__tmp_13 = unpack( pack( _Stage__62_rf1 ) );
        _Stage__62_dividend = ( _Stage__62_isSignedDiv ? _Stage__62__tmp_12 : _Stage__62__tmp_13 );
        _Stage__62__tmp_14 = unpack( pack( abs(_Stage__62_rf2) ) );
        _Stage__62__tmp_15 = unpack( pack( _Stage__62_rf2 ) );
        _Stage__62_divisor = ( _Stage__62_isSignedDiv ? _Stage__62__tmp_14 : _Stage__62__tmp_15 );
        _Stage__62_retQuot = ( _Stage__62_funct3 <= 3'd5 );
        _Stage__62_invertRes = ( _Stage__62_isSignedDiv && ( _Stage__62_sdividend != _Stage__62_sdivisor ) );
        _Stage__62_carg_1310 = _Stage__62_dividend;
        _Stage__62_carg_1311 = _Stage__62_divisor;
        _Stage__62_carg_1312 = 32'd0;
        _Stage__62_carg_1313 = 32'd0;
        _Stage__62_carg_1314 = 5'd0;
        _Stage__62_carg_1315 = _Stage__62_retQuot;
    end
    if ( ( _Stage__62___condStage__66 == 1'd1 ))
    begin
        _Stage__62_invertRes = False;
        _Stage__62_udivout = 32'd0;
    end
    _Stage__62__tmp_16 = unpack( pack( _Stage__62_alu_res ) );
    _Stage__62_tmpaddr = _Stage__62__tmp_16;
    _Stage__62__tmp_17 = ( _Stage__62_tmpaddr >> 2'd2 );
    _Stage__62_memaddr = unpack( pack( _Stage__62__tmp_17 ) [ 15 : 0 ] );
    _Stage__62__tmp_18 = unpack( pack( _Stage__62_alu_res ) [ 1 : 0 ] );
    _Stage__62_boff = _Stage__62__tmp_18;
    _Stage__62___condStage__71 = ( _Stage__62_isLoad ? 2'd0 : ( _Stage__62_isStore ? 2'd1 : 2'd2 ) );
    if ( ( _Stage__62___condStage__71 == 2'd0 ))
    begin
        _Stage__62_raddr = _Stage__62_memaddr;
    end
    if ( ( _Stage__62___condStage__71 == 2'd1 ))
    begin
        _Stage__62_waddr = _Stage__62_memaddr;
        _Stage__62_nboff = unpack( { pack( _Stage__62_boff ), pack( 3'd0 ) } );
        _Stage__62_msg_1316 = ( _Stage__62_rf2 << _Stage__62_nboff );
        _Stage__62_wdata = 32'd0;
    end
    if ( ( _Stage__62___condStage__71 == 2'd2 ))
    begin
        _Stage__62_wdata = 32'd0;
    end
    UInt#(2) _Stage__72___condStage__71 = fifo_Stage__62_TO_Stage__72.first.__condStage__71;
    Bool _Stage__72_writerd = fifo_Stage__62_TO_Stage__72.first.writerd;
    MemId#(8) _Stage__72__request_3 = fifo_Stage__62_TO_Stage__72.first._request_3;
    UInt#(2) _Stage__72_boff = fifo_Stage__62_TO_Stage__72.first.boff;
    UInt#(1) _Stage__72___condStage__66 = fifo_Stage__62_TO_Stage__72.first.__condStage__66;
    Int#(16) _Stage__72_pc = fifo_Stage__62_TO_Stage__72.first.pc;
    MemId#(8) _Stage__72__request_4 = fifo_Stage__62_TO_Stage__72.first._request_4;
    UInt#(3) _Stage__72_funct3 = fifo_Stage__62_TO_Stage__72.first.funct3;
    Bool _Stage__72_invertRes = fifo_Stage__62_TO_Stage__72.first.invertRes;
    Bool _Stage__72_done = fifo_Stage__62_TO_Stage__72.first.done;
    UInt#(5) _Stage__72_rd = fifo_Stage__62_TO_Stage__72.first.rd;
    Int#(32) _Stage__72_wdata = fifo_Stage__62_TO_Stage__72.first.wdata;
    Int#(32) _Stage__72_rddata = fifo_Stage__62_TO_Stage__72.first.rddata;
    Maybe#( _lidTyp_rf ) _Stage__72__lock_id_rf_rd_op = fifo_Stage__62_TO_Stage__72.first._lock_id_rf_rd_op;
    Maybe#( _lidTyp_rf ) _Stage__72__lock_id_rf_rd_rs = fifo_Stage__62_TO_Stage__72.first._lock_id_rf_rd_rs;
    UInt#(32) _Stage__72_udivout = fifo_Stage__62_TO_Stage__72.first.udivout;
    Bool _Stage__72_isDiv = fifo_Stage__62_TO_Stage__72.first.isDiv;
    Int#(32) _Stage__72_insn = fifo_Stage__62_TO_Stage__72.first.insn;
    UInt#(1) _Stage__72__request_2 = fifo_Stage__62_TO_Stage__72.first._request_2;
    Bool _Stage__72_isLoad = fifo_Stage__62_TO_Stage__72.first.isLoad;
    UInt#(3) _Stage__72__threadID = fifo_Stage__62_TO_Stage__72.first._threadID;
    Maybe#( SpecId#(4) ) _Stage__72__specId = fifo_Stage__62_TO_Stage__72.first._specId;
    UInt#(1) _Stage__72___condStage__84 = ?;
    UInt#(1) _Stage__72___condStage__81 = ?;
    Maybe#( _lidTyp_rf ) _Stage__72__lock_id_rf_rd_aq = ?;
    Int#(32) _Stage__72_insnout = ?;
    UInt#(1) _Stage__72___condStage__79 = ?;
    Int#(32) _Stage__72__tmp_19 = ?;
    Int#(32) _Stage__72__tmp_20 = ?;
    Int#(32) _Stage__72_divout = ?;
    if ( ( _Stage__72___condStage__66 == 1'd0 ))
    begin
        _Stage__72_udivout = div.peek;
    end
    if ( ( _Stage__72___condStage__71 == 2'd0 ))
    begin
        _Stage__72_wdata = dmem.peekResp1(_Stage__72__request_3);
    end
    _Stage__72___condStage__84 = ( _Stage__72_writerd ? 1'd0 : 1'd1 );
    if ( ( _Stage__72___condStage__84 == 1'd0 ))
    begin
        _Stage__72___condStage__81 = ( _Stage__72_isLoad ? 1'd0 : 1'd1 );
    end
    if ( ( ( _Stage__72___condStage__84 == 1'd0 ) && ( _Stage__72___condStage__81 == 1'd0 ) ))
    begin
        _Stage__72__lock_id_rf_rd_aq = _Stage__72__lock_id_rf_rd_rs;
        _Stage__72_insnout = maskLoad(_Stage__72_wdata, _Stage__72_funct3, _Stage__72_boff);
        _Stage__72__lock_id_rf_rd_op = _Stage__72__lock_id_rf_rd_aq;
    end
    if ( ( ( _Stage__72___condStage__84 == 1'd0 ) && ( _Stage__72___condStage__81 == 1'd1 ) ))
    begin
        _Stage__72___condStage__79 = ( _Stage__72_isDiv ? 1'd0 : 1'd1 );
    end
    if ( ( ( _Stage__72___condStage__84 == 1'd0 ) && ( ( _Stage__72___condStage__81 == 1'd1 ) && ( _Stage__72___condStage__79 == 1'd0 ) ) ))
    begin
        _Stage__72__lock_id_rf_rd_aq = _Stage__72__lock_id_rf_rd_rs;
        _Stage__72__tmp_19 = unpack( pack( _Stage__72_udivout ) );
        _Stage__72__tmp_20 = unpack( pack( _Stage__72_udivout ) );
        _Stage__72_divout = ( _Stage__72_invertRes ? ( - _Stage__72__tmp_19 ) : _Stage__72__tmp_20 );
        _Stage__72_insnout = _Stage__72_divout;
        _Stage__72__lock_id_rf_rd_op = _Stage__72__lock_id_rf_rd_aq;
    end
    if ( ( ( _Stage__72___condStage__84 == 1'd0 ) && ( ( _Stage__72___condStage__81 == 1'd1 ) && ( _Stage__72___condStage__79 == 1'd1 ) ) ))
    begin
        _Stage__72_insnout = _Stage__72_rddata;
    end
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
    UInt#(1) _Stage__0___condStage__12 = ?;
    UInt#(1) _Stage__0___condStage__9 = ?;
    SpecId#(4) _Stage__0_s2 = ?;
    UInt#(1) _Stage__0___condStage__7 = ?;
    Int#(16) _Stage__0__s2_0 = ?;
    UInt#(1) _Stage__0___condStage__16 = ?;
    Maybe#( _lidTyp_rf ) _Stage__0__lock_id_rf_rs1_rs = tagged Invalid;
    UInt#(1) _Stage__0___condStage__20 = ?;
    Maybe#( _lidTyp_rf ) _Stage__0__lock_id_rf_rs2_rs = tagged Invalid;
    UInt#(1) _Stage__0___condStage__24 = ?;
    _Stage__0_insn = imem.peekResp1(_Stage__0__request_0);
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
    _Stage__0___condStage__12 = ( ( ! _Stage__0_done ) ? 1'd0 : 1'd1 );
    if ( ( _Stage__0___condStage__12 == 1'd0 ))
    begin
        _Stage__0___condStage__9 = ( _Stage__0_notBranch ? 1'd0 : 1'd1 );
    end
    if ( ( ( _Stage__0___condStage__12 == 1'd0 ) && ( _Stage__0___condStage__9 == 1'd0 ) ))
    begin
        _Stage__0_s2 = _Stage__0_s;
    end
    if ( ( ( _Stage__0___condStage__12 == 1'd0 ) && ( _Stage__0___condStage__9 == 1'd1 ) ))
    begin
        _Stage__0___condStage__7 = ( _Stage__0_isBranch ? 1'd0 : 1'd1 );
    end
    if ( ( ( _Stage__0___condStage__12 == 1'd0 ) && ( ( _Stage__0___condStage__9 == 1'd1 ) && ( _Stage__0___condStage__7 == 1'd0 ) ) ))
    begin
        _Stage__0__s2_0 = bht.req(_Stage__0_pc, _Stage__0_immB, 16'd1);
    end
    if ( ( ( _Stage__0___condStage__12 == 1'd0 ) && ( ( _Stage__0___condStage__9 == 1'd1 ) && ( _Stage__0___condStage__7 == 1'd1 ) ) ))
    begin
        _Stage__0_s2 = _Stage__0_s;
    end
    if ( ( _Stage__0___condStage__12 == 1'd1 ))
    begin
        _Stage__0_s2 = _Stage__0_s;
    end
    _Stage__0___condStage__16 = ( _Stage__0_needrs1 ? 1'd0 : 1'd1 );
    if ( ( _Stage__0___condStage__16 == 1'd0 ))
    begin
        _Stage__0__lock_id_rf_rs1_rs = tagged Valid rf.res_r1(_Stage__0_rs1);
    end
    _Stage__0___condStage__20 = ( _Stage__0_needrs2 ? 1'd0 : 1'd1 );
    if ( ( _Stage__0___condStage__20 == 1'd0 ))
    begin
        _Stage__0__lock_id_rf_rs2_rs = tagged Valid rf.res_r2(_Stage__0_rs2);
    end
    _Stage__0___condStage__24 = ( _Stage__0_writerd ? 1'd0 : 1'd1 );
    Int#(16) _Start_pc = fifo__input__TO_Start.first.pc;
    UInt#(3) _Start__threadID = fifo__input__TO_Start.first._threadID;
    Maybe#( SpecId#(4) ) _Start__specId = fifo__input__TO_Start.first._specId;
    UInt#(16) _Start__tmp_0 = ?;
    UInt#(16) _Start_pcaddr = ?;
    Int#(16) _Start__s_0 = ?;
    _Start__tmp_0 = unpack( pack( _Start_pc ) );
    _Start_pcaddr = _Start__tmp_0;
    _Start__s_0 = ( _Start_pc + 16'd1 );
    rule s_Stage__85_execute (( ( ! ( _Stage__85___condStage__93 == 1'd0 ) ) || outputQueue.canWrite(_Stage__85__threadID) ));
        if ( ( _Stage__85___condStage__89 == 1'd0 ))
        begin
            rf.rel_w1(fromMaybe( ? , _Stage__85__lock_id_rf_rd_op ));
        end
        if ( ( _Stage__85___condStage__93 == 1'd0 ))
        begin
            busyReg <= False;
            outputQueue.enq(True);
        end
        fifo_Stage__72_TO_Stage__85.deq;
    endrule
    rule s_Stage__25_execute (( ( ! ( _Stage__25___condStage__29 == 1'd0 ) ) || rf.owns_r1(fromMaybe( ? , _Stage__25__lock_id_rf_rs1_rs )) ) && ( ( ! ( _Stage__25___condStage__33 == 1'd0 ) ) || rf.owns_r2(fromMaybe( ? , _Stage__25__lock_id_rf_rs2_rs )) ));
        if ( ( ( _Stage__25___condStage__57 == 1'd0 ) && ( ( _Stage__25___condStage__54 == 1'd0 ) && ( _Stage__25___condStage__51 == 1'd0 ) ) ))
        begin
            bht.upd(_Stage__25_pc, _Stage__25_take);
            if ( ( True && ( _Stage__25_npc == _Stage__25__s2_0 ) ))
            begin
                _specTable.validate(_Stage__25_s2, 0);
            end
            else
            begin
                _specTable.invalidate(_Stage__25_s2, 0);
                fifo__input__TO_Start.enq(E__input__TO_Start { pc : _Stage__25_npc,_threadID : _Stage__25__threadID,_specId : tagged Invalid });
            end
        end
        if ( ( ( _Stage__25___condStage__57 == 1'd0 ) && ( ( _Stage__25___condStage__54 == 1'd0 ) && ( _Stage__25___condStage__51 == 1'd1 ) ) ))
        begin
            fifo__input__TO_Start.enq(E__input__TO_Start { pc : _Stage__25_carg_1309,_threadID : _Stage__25__threadID,_specId : tagged Invalid });
        end
        if ( ( _Stage__25___condStage__61 == 1'd0 ))
        begin
            rf.write(fromMaybe( ? , _Stage__25__lock_id_rf_rd_aq ), _Stage__25_rddata);
        end
        fifo_Stage__0_TO_Stage__25.deq;
        fifo_Stage__25_TO_Stage__62.enq(E_Stage__25_TO_Stage__62 { _lock_id_rf_rd_rs : _Stage__25__lock_id_rf_rd_rs,_threadID : _Stage__25__threadID,alu_res : _Stage__25_alu_res,funct3 : _Stage__25_funct3,isStore : _Stage__25_isStore,rddata : _Stage__25_rddata,isDiv : _Stage__25_isDiv,isLoad : _Stage__25_isLoad,done : _Stage__25_done,pc : _Stage__25_pc,insn : _Stage__25_insn,rf2 : _Stage__25_rf2,_lock_id_rf_rd_op : _Stage__25__lock_id_rf_rd_op,rd : _Stage__25_rd,_specId : _Stage__25__specId,rf1 : _Stage__25_rf1,writerd : _Stage__25_writerd });
    endrule
    rule s_Stage__62_execute ;
        UInt#(1) _Stage__62__request_2 = ?;
        MemId#(8) _Stage__62__request_3 = ?;
        MemId#(8) _Stage__62__request_4 = ?;
        if ( ( _Stage__62___condStage__66 == 1'd0 ))
        begin
            _Stage__62__request_2 <- div.req(_Stage__62_carg_1310, _Stage__62_carg_1311, _Stage__62_carg_1312, _Stage__62_carg_1313, _Stage__62_carg_1314, _Stage__62_carg_1315);
        end
        if ( ( _Stage__62___condStage__71 == 2'd0 ))
        begin
            _Stage__62__request_3 <- dmem.req1(_Stage__62_raddr, ?, 0);
        end
        if ( ( _Stage__62___condStage__71 == 2'd1 ))
        begin
            _Stage__62__request_4 <- dmem.req1(_Stage__62_waddr, _Stage__62_msg_1316, pack( storeMask(_Stage__62_boff, _Stage__62_funct3) ));
        end
        fifo_Stage__25_TO_Stage__62.deq;
        fifo_Stage__62_TO_Stage__72.enq(E_Stage__62_TO_Stage__72 { insn : _Stage__62_insn,_specId : _Stage__62__specId,funct3 : _Stage__62_funct3,isDiv : _Stage__62_isDiv,__condStage__66 : _Stage__62___condStage__66,_lock_id_rf_rd_op : _Stage__62__lock_id_rf_rd_op,_request_3 : _Stage__62__request_3,pc : _Stage__62_pc,wdata : _Stage__62_wdata,__condStage__71 : _Stage__62___condStage__71,_lock_id_rf_rd_rs : _Stage__62__lock_id_rf_rd_rs,_threadID : _Stage__62__threadID,_request_2 : _Stage__62__request_2,rddata : _Stage__62_rddata,writerd : _Stage__62_writerd,invertRes : _Stage__62_invertRes,_request_4 : _Stage__62__request_4,boff : _Stage__62_boff,done : _Stage__62_done,udivout : _Stage__62_udivout,rd : _Stage__62_rd,isLoad : _Stage__62_isLoad });
    endrule
    rule s_Stage__72_execute (( ( ! ( _Stage__72___condStage__66 == 1'd0 ) ) || div.checkHandle(_Stage__72__request_2) ) && ( ( ! ( _Stage__72___condStage__71 == 2'd1 ) ) || dmem.checkRespId1(_Stage__72__request_4) ) && ( ( ! ( _Stage__72___condStage__71 == 2'd0 ) ) || dmem.checkRespId1(_Stage__72__request_3) ));
        if ( ( _Stage__72___condStage__66 == 1'd0 ))
        begin
            div.resp;
        end
        if ( ( _Stage__72___condStage__71 == 2'd1 ))
        begin
            dmem.resp1(_Stage__72__request_4);
        end
        if ( ( _Stage__72___condStage__71 == 2'd0 ))
        begin
            dmem.resp1(_Stage__72__request_3);
        end
        $display( "PC: %h",( _Stage__72_pc << 2'd2 ) );
        $display( "INSN: %h",_Stage__72_insn );
        if ( ( ( _Stage__72___condStage__84 == 1'd0 ) && ( _Stage__72___condStage__81 == 1'd0 ) ))
        begin
            rf.write(fromMaybe( ? , _Stage__72__lock_id_rf_rd_aq ), _Stage__72_insnout);
        end
        if ( ( ( _Stage__72___condStage__84 == 1'd0 ) && ( ( _Stage__72___condStage__81 == 1'd1 ) && ( _Stage__72___condStage__79 == 1'd0 ) ) ))
        begin
            rf.write(fromMaybe( ? , _Stage__72__lock_id_rf_rd_aq ), _Stage__72_insnout);
        end
        if ( ( _Stage__72___condStage__84 == 1'd0 ))
        begin
            $display( "Writing %d to r%d",_Stage__72_insnout,_Stage__72_rd );
        end
        fifo_Stage__62_TO_Stage__72.deq;
        fifo_Stage__72_TO_Stage__85.enq(E_Stage__72_TO_Stage__85 { _specId : _Stage__72__specId,_lock_id_rf_rd_op : _Stage__72__lock_id_rf_rd_op,rd : _Stage__72_rd,_threadID : _Stage__72__threadID,writerd : _Stage__72_writerd,done : _Stage__72_done });
    endrule
    rule s_Stage__0_execute (( ( ! isValid( _Stage__0__specId ) ) || fromMaybe( False , _specTable.check(fromMaybe( ? , _Stage__0__specId ), 1) ) ) && imem.checkRespId1(_Stage__0__request_0));
        SpecId#(4) _Stage__0_s2 = ?;
        Maybe#( _lidTyp_rf ) _Stage__0__lock_id_rf_rd_rs = tagged Invalid;
        imem.resp1(_Stage__0__request_0);
        if ( isValid( _Stage__0__specId ))
        begin
            _specTable.free(fromMaybe( ? , _Stage__0__specId ));
        end
        if ( ( ( _Stage__0___condStage__12 == 1'd0 ) && ( _Stage__0___condStage__9 == 1'd0 ) ))
        begin
            if ( ( True && ( ( _Stage__0_pc + 16'd1 ) == _Stage__0__s_0 ) ))
            begin
                _specTable.validate(_Stage__0_s, 1);
            end
            else
            begin
                _specTable.invalidate(_Stage__0_s, 1);
                fifo__input__TO_Start.enq(E__input__TO_Start { pc : ( _Stage__0_pc + 16'd1 ),_threadID : _Stage__0__threadID,_specId : tagged Invalid });
            end
        end
        if ( ( ( _Stage__0___condStage__12 == 1'd0 ) && ( ( _Stage__0___condStage__9 == 1'd1 ) && ( _Stage__0___condStage__7 == 1'd0 ) ) ))
        begin
            if ( ( False || ( _Stage__0__s2_0 != _Stage__0__s_0 ) ))
            begin
                _specTable.invalidate(_Stage__0_s, 1);
                _Stage__0_s2 <- _specTable.alloc;
                fifo__input__TO_Start.enq(E__input__TO_Start { pc : _Stage__0__s2_0,_threadID : _Stage__0__threadID,_specId : tagged Valid _Stage__0_s2 });
            end
            else
            begin
                _Stage__0_s2 = _Stage__0_s;
            end
        end
        if ( ( ( _Stage__0___condStage__12 == 1'd0 ) && ( ( _Stage__0___condStage__9 == 1'd1 ) && ( _Stage__0___condStage__7 == 1'd1 ) ) ))
        begin
            _specTable.invalidate(_Stage__0_s, 1);
        end
        if ( ( _Stage__0___condStage__12 == 1'd1 ))
        begin
            _specTable.invalidate(_Stage__0_s, 1);
        end
        if ( ( _Stage__0___condStage__24 == 1'd0 ))
        begin
            let __tmp_0 <- rf.res_w1(_Stage__0_rd);
            _Stage__0__lock_id_rf_rd_rs = tagged Valid __tmp_0;
        end
        fifo_Start_TO_Stage__0.deq;
        fifo_Stage__0_TO_Stage__25.enq(E_Stage__0_TO_Stage__25 { isStore : _Stage__0_isStore,_specId : _Stage__0__specId,immB : _Stage__0_immB,immS : _Stage__0_immS,_s2_0 : _Stage__0__s2_0,isAui : _Stage__0_isAui,isDiv : _Stage__0_isDiv,_lock_id_rf_rd_rs : _Stage__0__lock_id_rf_rd_rs,needrs2 : _Stage__0_needrs2,doAdd : _Stage__0_doAdd,immU : _Stage__0_immU,needrs1 : _Stage__0_needrs1,pc : _Stage__0_pc,isBranch : _Stage__0_isBranch,isOpImm : _Stage__0_isOpImm,isLoad : _Stage__0_isLoad,_lock_id_rf_rs2_rs : _Stage__0__lock_id_rf_rs2_rs,insn : _Stage__0_insn,immJ : _Stage__0_immJ,isLui : _Stage__0_isLui,rs1 : _Stage__0_rs1,rs2 : _Stage__0_rs2,isMul : _Stage__0_isMul,_threadID : _Stage__0__threadID,_lock_id_rf_rs1_rs : _Stage__0__lock_id_rf_rs1_rs,done : _Stage__0_done,writerd : _Stage__0_writerd,notBranch : _Stage__0_notBranch,immI : _Stage__0_immI,immJR : _Stage__0_immJR,s2 : _Stage__0_s2,isJalr : _Stage__0_isJalr,isJal : _Stage__0_isJal,funct3 : _Stage__0_funct3,flip : _Stage__0_flip,rd : _Stage__0_rd });
    endrule
    rule s_Stage__0_kill (( isValid( _Stage__0__specId ) && ( ! fromMaybe( True , _specTable.check(fromMaybe( ? , _Stage__0__specId ), 1) ) ) ) && imem.checkRespId1(_Stage__0__request_0));
        fifo_Start_TO_Stage__0.deq;
        imem.resp1(_Stage__0__request_0);
        _specTable.free(fromMaybe( ? , _Stage__0__specId ));
    endrule
    rule s_Start_execute (( ( ! isValid( _Start__specId ) ) || fromMaybe( True , _specTable.check(fromMaybe( ? , _Start__specId ), 2) ) ));
        SpecId#(4) _Start_s = ?;
        MemId#(8) _Start__request_0 = ?;
        _Start_s <- _specTable.alloc;
        fifo__input__TO_Start.enq(E__input__TO_Start { pc : _Start__s_0,_threadID : _Start__threadID,_specId : tagged Valid _Start_s });
        _Start__request_0 <- imem.req1(_Start_pcaddr, ?, 0);
        fifo__input__TO_Start.deq;
        fifo_Start_TO_Stage__0.enq(E_Start_TO_Stage__0 { pc : _Start_pc,_specId : _Start__specId,_request_0 : _Start__request_0,_threadID : _Start__threadID,s : _Start_s,_s_0 : _Start__s_0 });
    endrule
    rule s_Start_kill (( isValid( _Start__specId ) && ( ! fromMaybe( True , _specTable.check(fromMaybe( ? , _Start__specId ), 2) ) ) ));
        fifo__input__TO_Start.deq;
        _specTable.free(fromMaybe( ? , _Start__specId ));
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
