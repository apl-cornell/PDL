import FIFOF :: *;
import Locks :: *;
import Memories :: *;
import VerilogLibs :: *;
import Speculation :: *;
import Functions :: *;
import Multi_stg_div :: *;

export Cpu (..);
export mkCpu ;

typedef struct { Int#(16) pc; UInt#(3) _threadID ; } E__input__TO_Start deriving( Bits,Eq );
typedef struct { Int#(32) rf1; Bool isAui; Bool writerd; Int#(32) immU; Int#(16) immB; Int#(32) immJ; Int#(16) pc; Int#(32) immI; Bool isLui; UInt#(3) funct3; Bool isStore; Bool isOpImm; Bool done; Bool isJal; Int#(32) rf2; Int#(32) immS; Bool flip; Bool isMDiv; Bool isBranch; Bool isDiv; Bool isJalr; Maybe#( _lidTyp_rf ) _lock_id_rf_rd; Int#(16) immJR; UInt#(5) rd; Int#(32) insn; Bool isLoad; UInt#(3) _threadID ; } E_Stage__0_TO_Stage__13#( type _lidTyp_rf ) deriving( Bits,Eq );
typedef struct { Int#(32) rf2; Bool writerd; UInt#(2) boff; Int#(16) pc; UInt#(3) funct3; Bool isStore; Bool done; UInt#(5) rd; Bool isJal; Bool isJalr; UInt#(16) memaddr; Maybe#( _lidTyp_rf ) _lock_id_rf_rd; Int#(32) insn; Int#(32) alu_res; Bool isLoad; UInt#(3) _threadID ; } E_Stage__34_TO_Stage__47#( type _lidTyp_rf ) deriving( Bits,Eq );
typedef struct { Int#(32) rf1; Bool isAui; Bool writerd; Int#(32) immU; UInt#(1) __condStage__33; Int#(32) alu_arg2; Int#(16) pc; Int#(32) immI; Bool isLui; UInt#(3) funct3; Bool invertRes; Bool isStore; Bool done; UInt#(5) rd; Bool isJal; Int#(32) rf2; Int#(32) immS; Bool flip; Bool isMDiv; UInt#(32) udivout; Bool isDiv; Bool isJalr; Maybe#( _lidTyp_rf ) _lock_id_rf_rd; Int#(32) insn; UInt#(1) _request_2; Bool isLoad; UInt#(3) _threadID ; } E_Stage__13_TO_Stage__34#( type _lidTyp_rf ) deriving( Bits,Eq );
typedef struct { MemId#(8) _request_0; Int#(16) pc; UInt#(3) _threadID ; } E_Start_TO_Stage__0 deriving( Bits,Eq );
typedef struct { Bool writerd; MemId#(8) _request_3; Int#(32) wdata; UInt#(2) boff; Int#(16) pc; MemId#(8) _request_4; UInt#(3) funct3; Bool done; UInt#(5) rd; Bool isJal; Bool isJalr; Maybe#( _lidTyp_rf ) _lock_id_rf_rd; Int#(32) insn; Int#(32) alu_res; Bool isLoad; UInt#(2) __condStage__52; UInt#(3) _threadID ; } E_Stage__47_TO_Stage__53#( type _lidTyp_rf ) deriving( Bits,Eq );
typedef struct { UInt#(3) handle; Bool data ; } OutputQueueInfo deriving( Bits,Eq );

interface Cpu;
   method ActionValue#(UInt#(3)) req ( Int#(16) pc ) ;
   method Action resp (  ) ;
   method Bool checkHandle ( UInt#(3) handle ) ;
   method Bool peek (  ) ;
endinterface


module mkCpu ( AddrLockCombMem#( UInt#(5), Int#(32), _lidTyp_rf, _szParam_0_rf ) rf, QueueLockAsyncMem#( UInt#(16), Int#(32), MemId#(8), 4, _lidTyp_imem ) imem, AddrLockAsyncMem#( UInt#(16), Int#(32), MemId#(8), 4, _lidTyp_dmem, _szParam_0_dmem ) dmem, Multi_stg_div div, Cpu _unused_ ) provisos( Bits#(_lidTyp_dmem,_sz_lidTyp_dmem),Bits#(_lidTyp_rf,_sz_lidTyp_rf),Bits#(_lidTyp_imem,_sz_lidTyp_imem) );
    FIFOF#( E__input__TO_Start ) fifo__input__TO_Start <- mkNBFIFOF (  );
    FIFOF#( E_Stage__0_TO_Stage__13#(_lidTyp_rf) ) fifo_Stage__0_TO_Stage__13 <- mkFIFOF (  );
    FIFOF#( E_Stage__34_TO_Stage__47#(_lidTyp_rf) ) fifo_Stage__34_TO_Stage__47 <- mkFIFOF (  );
    FIFOF#( E_Stage__13_TO_Stage__34#(_lidTyp_rf) ) fifo_Stage__13_TO_Stage__34 <- mkFIFOF (  );
    FIFOF#( E_Start_TO_Stage__0 ) fifo_Start_TO_Stage__0 <- mkFIFOF (  );
    FIFOF#( E_Stage__47_TO_Stage__53#(_lidTyp_rf) ) fifo_Stage__47_TO_Stage__53 <- mkFIFOF (  );
    Reg#( Bool ) rf_lock_region <- mkReg ( True );
    Reg#( Bool ) imem_lock_region <- mkReg ( True );
    Reg#( Bool ) dmem_lock_region <- mkReg ( True );
    Reg#( Bool ) div_lock_region <- mkReg ( True );
    Reg#( Bool ) busyReg <- mkReg ( False );
    FIFOF#( OutputQueueInfo ) outputQueue <- mkFIFOF (  );
    Reg#( UInt#(3) ) _threadID <- mkReg ( 0 );
    Int#(32) _Stage__13_rf1 = fifo_Stage__0_TO_Stage__13.first().rf1;
    Bool _Stage__13_isAui = fifo_Stage__0_TO_Stage__13.first().isAui;
    Bool _Stage__13_writerd = fifo_Stage__0_TO_Stage__13.first().writerd;
    Int#(32) _Stage__13_immU = fifo_Stage__0_TO_Stage__13.first().immU;
    Int#(16) _Stage__13_immB = fifo_Stage__0_TO_Stage__13.first().immB;
    Int#(32) _Stage__13_immJ = fifo_Stage__0_TO_Stage__13.first().immJ;
    Int#(16) _Stage__13_pc = fifo_Stage__0_TO_Stage__13.first().pc;
    Int#(32) _Stage__13_immI = fifo_Stage__0_TO_Stage__13.first().immI;
    Bool _Stage__13_isLui = fifo_Stage__0_TO_Stage__13.first().isLui;
    UInt#(3) _Stage__13_funct3 = fifo_Stage__0_TO_Stage__13.first().funct3;
    Bool _Stage__13_isStore = fifo_Stage__0_TO_Stage__13.first().isStore;
    Bool _Stage__13_isOpImm = fifo_Stage__0_TO_Stage__13.first().isOpImm;
    Bool _Stage__13_done = fifo_Stage__0_TO_Stage__13.first().done;
    Bool _Stage__13_isJal = fifo_Stage__0_TO_Stage__13.first().isJal;
    Int#(32) _Stage__13_rf2 = fifo_Stage__0_TO_Stage__13.first().rf2;
    Int#(32) _Stage__13_immS = fifo_Stage__0_TO_Stage__13.first().immS;
    Bool _Stage__13_flip = fifo_Stage__0_TO_Stage__13.first().flip;
    Bool _Stage__13_isMDiv = fifo_Stage__0_TO_Stage__13.first().isMDiv;
    Bool _Stage__13_isBranch = fifo_Stage__0_TO_Stage__13.first().isBranch;
    Bool _Stage__13_isDiv = fifo_Stage__0_TO_Stage__13.first().isDiv;
    Bool _Stage__13_isJalr = fifo_Stage__0_TO_Stage__13.first().isJalr;
    Maybe#( _lidTyp_rf ) _Stage__13__lock_id_rf_rd = fifo_Stage__0_TO_Stage__13.first()._lock_id_rf_rd;
    Int#(16) _Stage__13_immJR = fifo_Stage__0_TO_Stage__13.first().immJR;
    UInt#(5) _Stage__13_rd = fifo_Stage__0_TO_Stage__13.first().rd;
    Int#(32) _Stage__13_insn = fifo_Stage__0_TO_Stage__13.first().insn;
    Bool _Stage__13_isLoad = fifo_Stage__0_TO_Stage__13.first().isLoad;
    UInt#(3) _Stage__13__threadID = fifo_Stage__0_TO_Stage__13.first()._threadID;
    UInt#(1) _Stage__13___condStage__25 = ?;
    Int#(16) _Stage__13_npc = ?;
    UInt#(1) _Stage__13___condStage__23 = ?;
    Int#(32) _Stage__13__tmp_11 = ?;
    Int#(32) _Stage__13_npc32 = ?;
    UInt#(1) _Stage__13___condStage__21 = ?;
    UInt#(1) _Stage__13___condStage__29 = ?;
    Int#(16) _Stage__13_carg_5 = ?;
    Int#(32) _Stage__13_alu_arg2 = ?;
    UInt#(1) _Stage__13___condStage__33 = ?;
    Int#(32) _Stage__13_sdividend = ?;
    Int#(32) _Stage__13_sdivisor = ?;
    Bool _Stage__13_isSignedDiv = ?;
    Bool _Stage__13_invertRes = ?;
    UInt#(32) _Stage__13__tmp_12 = ?;
    UInt#(32) _Stage__13__tmp_13 = ?;
    UInt#(32) _Stage__13_dividend = ?;
    UInt#(32) _Stage__13__tmp_14 = ?;
    UInt#(32) _Stage__13__tmp_15 = ?;
    UInt#(32) _Stage__13_divisor = ?;
    Bool _Stage__13_retQuot = ?;
    UInt#(32) _Stage__13_carg_6 = ?;
    UInt#(32) _Stage__13_carg_7 = ?;
    UInt#(32) _Stage__13_carg_8 = ?;
    UInt#(32) _Stage__13_carg_9 = ?;
    UInt#(5) _Stage__13_carg_10 = ?;
    Bool _Stage__13_carg_11 = ?;
    UInt#(32) _Stage__13_udivout = ?;
    _Stage__13___condStage__25 = ( _Stage__13_isBranch ? 1'd0 : 1'd1 );
    if ( ( _Stage__13___condStage__25 == 1'd0 ))
    begin
        _Stage__13_npc = br(_Stage__13_pc, _Stage__13_immB, _Stage__13_funct3, _Stage__13_rf1, _Stage__13_rf2);
    end
    if ( ( _Stage__13___condStage__25 == 1'd1 ))
    begin
        _Stage__13___condStage__23 = ( _Stage__13_isJal ? 1'd0 : 1'd1 );
    end
    if ( ( ( _Stage__13___condStage__25 == 1'd1 ) && ( _Stage__13___condStage__23 == 1'd0 ) ))
    begin
        _Stage__13__tmp_11 = signExtend( _Stage__13_pc );
        _Stage__13_npc32 = ( _Stage__13__tmp_11 + ( _Stage__13_immJ >> 2'd2 ) );
        _Stage__13_npc = unpack( pack( _Stage__13_npc32 ) [ 15 : 0 ] );
    end
    if ( ( ( _Stage__13___condStage__25 == 1'd1 ) && ( _Stage__13___condStage__23 == 1'd1 ) ))
    begin
        _Stage__13___condStage__21 = ( _Stage__13_isJalr ? 1'd0 : 1'd1 );
    end
    if ( ( ( _Stage__13___condStage__25 == 1'd1 ) && ( ( _Stage__13___condStage__23 == 1'd1 ) && ( _Stage__13___condStage__21 == 1'd0 ) ) ))
    begin
        _Stage__13_npc = ( ( unpack( pack( _Stage__13_rf1 ) [ 15 : 0 ] ) + _Stage__13_immJR ) >> 2'd2 );
    end
    if ( ( ( _Stage__13___condStage__25 == 1'd1 ) && ( ( _Stage__13___condStage__23 == 1'd1 ) && ( _Stage__13___condStage__21 == 1'd1 ) ) ))
    begin
        _Stage__13_npc = ( _Stage__13_pc + 16'd1 );
    end
    _Stage__13___condStage__29 = ( ( ! _Stage__13_done ) ? 1'd0 : 1'd1 );
    if ( ( _Stage__13___condStage__29 == 1'd0 ))
    begin
        _Stage__13_carg_5 = _Stage__13_npc;
    end
    _Stage__13_alu_arg2 = ( _Stage__13_isOpImm ? _Stage__13_immI : _Stage__13_rf2 );
    _Stage__13___condStage__33 = ( _Stage__13_isDiv ? 1'd0 : 1'd1 );
    if ( ( _Stage__13___condStage__33 == 1'd0 ))
    begin
        _Stage__13_sdividend = signum(_Stage__13_rf1);
        _Stage__13_sdivisor = ( ( _Stage__13_funct3 == 3'd6 ) ? 32'd1 : signum(_Stage__13_rf2) );
        _Stage__13_isSignedDiv = ( ( _Stage__13_funct3 == 3'd4 ) || ( _Stage__13_funct3 == 3'd6 ) );
        _Stage__13_invertRes = ( _Stage__13_isSignedDiv && ( _Stage__13_sdividend != _Stage__13_sdivisor ) );
        _Stage__13__tmp_12 = unpack( pack( abs(_Stage__13_rf1) ) );
        _Stage__13__tmp_13 = unpack( pack( _Stage__13_rf1 ) );
        _Stage__13_dividend = ( _Stage__13_isSignedDiv ? _Stage__13__tmp_12 : _Stage__13__tmp_13 );
        _Stage__13__tmp_14 = unpack( pack( abs(_Stage__13_rf2) ) );
        _Stage__13__tmp_15 = unpack( pack( _Stage__13_rf2 ) );
        _Stage__13_divisor = ( _Stage__13_isSignedDiv ? _Stage__13__tmp_14 : _Stage__13__tmp_15 );
        _Stage__13_retQuot = ( _Stage__13_funct3 <= 3'd5 );
        _Stage__13_carg_6 = _Stage__13_dividend;
        _Stage__13_carg_7 = _Stage__13_divisor;
        _Stage__13_carg_8 = 32'd0;
        _Stage__13_carg_9 = 32'd0;
        _Stage__13_carg_10 = 5'd0;
        _Stage__13_carg_11 = _Stage__13_retQuot;
    end
    if ( ( _Stage__13___condStage__33 == 1'd1 ))
    begin
        _Stage__13_udivout = 32'd0;
        _Stage__13_invertRes = False;
    end
    MemId#(8) _Stage__0__request_0 = fifo_Start_TO_Stage__0.first()._request_0;
    Int#(16) _Stage__0_pc = fifo_Start_TO_Stage__0.first().pc;
    UInt#(3) _Stage__0__threadID = fifo_Start_TO_Stage__0.first()._threadID;
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
    Bool _Stage__0_needrs1 = ?;
    Bool _Stage__0_needrs2 = ?;
    Bool _Stage__0_writerd = ?;
    UInt#(1) _Stage__0___condStage__4 = ?;
    Int#(32) _Stage__0_rf1 = ?;
    UInt#(1) _Stage__0___condStage__8 = ?;
    Int#(32) _Stage__0_rf2 = ?;
    UInt#(1) _Stage__0___condStage__12 = ?;
    _Stage__0_insn = imem.mem.peekResp(_Stage__0__request_0);
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
    _Stage__0_needrs1 = ( ! _Stage__0_isJal );
    _Stage__0_needrs2 = ( ( ( _Stage__0_isOp || _Stage__0_isBranch ) || _Stage__0_isStore ) || _Stage__0_isJalr );
    _Stage__0_writerd = ( ( _Stage__0_rd != 5'd0 ) && ( ( ( ( ( ( _Stage__0_isOp || _Stage__0_isOpImm ) || _Stage__0_isLoad ) || _Stage__0_isJal ) || _Stage__0_isJalr ) || _Stage__0_isLui ) || _Stage__0_isAui ) );
    _Stage__0___condStage__4 = ( _Stage__0_needrs1 ? 1'd0 : 1'd1 );
    if ( ( _Stage__0___condStage__4 == 1'd0 ))
    begin
        _Stage__0_rf1 = rf.read(_Stage__0_rs1);
    end
    if ( ( _Stage__0___condStage__4 == 1'd1 ))
    begin
        _Stage__0_rf1 = 32'd0;
    end
    _Stage__0___condStage__8 = ( _Stage__0_needrs2 ? 1'd0 : 1'd1 );
    if ( ( _Stage__0___condStage__8 == 1'd0 ))
    begin
        _Stage__0_rf2 = rf.read(_Stage__0_rs2);
    end
    if ( ( _Stage__0___condStage__8 == 1'd1 ))
    begin
        _Stage__0_rf2 = 32'd0;
    end
    _Stage__0___condStage__12 = ( _Stage__0_writerd ? 1'd0 : 1'd1 );
    Int#(32) _Stage__34_rf1 = fifo_Stage__13_TO_Stage__34.first().rf1;
    Bool _Stage__34_isAui = fifo_Stage__13_TO_Stage__34.first().isAui;
    Bool _Stage__34_writerd = fifo_Stage__13_TO_Stage__34.first().writerd;
    Int#(32) _Stage__34_immU = fifo_Stage__13_TO_Stage__34.first().immU;
    UInt#(1) _Stage__34___condStage__33 = fifo_Stage__13_TO_Stage__34.first().__condStage__33;
    Int#(32) _Stage__34_alu_arg2 = fifo_Stage__13_TO_Stage__34.first().alu_arg2;
    Int#(16) _Stage__34_pc = fifo_Stage__13_TO_Stage__34.first().pc;
    Int#(32) _Stage__34_immI = fifo_Stage__13_TO_Stage__34.first().immI;
    Bool _Stage__34_isLui = fifo_Stage__13_TO_Stage__34.first().isLui;
    UInt#(3) _Stage__34_funct3 = fifo_Stage__13_TO_Stage__34.first().funct3;
    Bool _Stage__34_invertRes = fifo_Stage__13_TO_Stage__34.first().invertRes;
    Bool _Stage__34_isStore = fifo_Stage__13_TO_Stage__34.first().isStore;
    Bool _Stage__34_done = fifo_Stage__13_TO_Stage__34.first().done;
    UInt#(5) _Stage__34_rd = fifo_Stage__13_TO_Stage__34.first().rd;
    Bool _Stage__34_isJal = fifo_Stage__13_TO_Stage__34.first().isJal;
    Int#(32) _Stage__34_rf2 = fifo_Stage__13_TO_Stage__34.first().rf2;
    Int#(32) _Stage__34_immS = fifo_Stage__13_TO_Stage__34.first().immS;
    Bool _Stage__34_flip = fifo_Stage__13_TO_Stage__34.first().flip;
    Bool _Stage__34_isMDiv = fifo_Stage__13_TO_Stage__34.first().isMDiv;
    UInt#(32) _Stage__34_udivout = fifo_Stage__13_TO_Stage__34.first().udivout;
    Bool _Stage__34_isDiv = fifo_Stage__13_TO_Stage__34.first().isDiv;
    Bool _Stage__34_isJalr = fifo_Stage__13_TO_Stage__34.first().isJalr;
    Maybe#( _lidTyp_rf ) _Stage__34__lock_id_rf_rd = fifo_Stage__13_TO_Stage__34.first()._lock_id_rf_rd;
    Int#(32) _Stage__34_insn = fifo_Stage__13_TO_Stage__34.first().insn;
    UInt#(1) _Stage__34__request_2 = fifo_Stage__13_TO_Stage__34.first()._request_2;
    Bool _Stage__34_isLoad = fifo_Stage__13_TO_Stage__34.first().isLoad;
    UInt#(3) _Stage__34__threadID = fifo_Stage__13_TO_Stage__34.first()._threadID;
    UInt#(3) _Stage__34___condStage__41 = ?;
    Int#(32) _Stage__34_alu_res = ?;
    Int#(32) _Stage__34_pc32 = ?;
    Int#(32) _Stage__34__tmp_16 = ?;
    Int#(32) _Stage__34__tmp_17 = ?;
    UInt#(2) _Stage__34___condStage__46 = ?;
    Int#(32) _Stage__34_tmp = ?;
    UInt#(32) _Stage__34__tmp_18 = ?;
    UInt#(32) _Stage__34_ctmp = ?;
    UInt#(16) _Stage__34_memaddr = ?;
    UInt#(2) _Stage__34_boff = ?;
    UInt#(32) _Stage__34__tmp_19 = ?;
    if ( ( _Stage__34___condStage__33 == 1'd0 ))
    begin
        _Stage__34_udivout = div.peek();
    end
    _Stage__34___condStage__41 = ( _Stage__34_isLui ? 3'd0 : ( _Stage__34_isAui ? 3'd1 : ( _Stage__34_isDiv ? 3'd2 : ( _Stage__34_isMDiv ? 3'd3 : 3'd4 ) ) ) );
    if ( ( _Stage__34___condStage__41 == 3'd0 ))
    begin
        _Stage__34_alu_res = _Stage__34_immU;
    end
    if ( ( _Stage__34___condStage__41 == 3'd1 ))
    begin
        _Stage__34_pc32 = ( unpack( { pack( 16'd0 ), pack( _Stage__34_pc ) } ) << 2'd2 );
        _Stage__34_alu_res = ( _Stage__34_pc32 + _Stage__34_immU );
    end
    if ( ( _Stage__34___condStage__41 == 3'd2 ))
    begin
        _Stage__34__tmp_16 = unpack( pack( _Stage__34_udivout ) );
        _Stage__34__tmp_17 = unpack( pack( _Stage__34_udivout ) );
        _Stage__34_alu_res = ( _Stage__34_invertRes ? ( - _Stage__34__tmp_16 ) : _Stage__34__tmp_17 );
    end
    if ( ( _Stage__34___condStage__41 == 3'd3 ))
    begin
        _Stage__34_alu_res = mul(_Stage__34_rf1, _Stage__34_rf2, _Stage__34_funct3);
    end
    if ( ( _Stage__34___condStage__41 == 3'd4 ))
    begin
        _Stage__34_alu_res = alu(_Stage__34_rf1, _Stage__34_alu_arg2, _Stage__34_funct3, _Stage__34_flip);
    end
    _Stage__34___condStage__46 = ( _Stage__34_isStore ? 2'd0 : ( _Stage__34_isLoad ? 2'd1 : 2'd2 ) );
    if ( ( _Stage__34___condStage__46 == 2'd0 ))
    begin
        _Stage__34_tmp = ( _Stage__34_immS + _Stage__34_rf1 );
        _Stage__34__tmp_18 = unpack( pack( _Stage__34_tmp ) );
        _Stage__34_ctmp = _Stage__34__tmp_18;
        _Stage__34_memaddr = unpack( pack( ( _Stage__34_ctmp >> 2'd2 ) ) [ 15 : 0 ] );
        _Stage__34_boff = unpack( pack( _Stage__34_ctmp ) [ 1 : 0 ] );
    end
    if ( ( _Stage__34___condStage__46 == 2'd1 ))
    begin
        _Stage__34_tmp = ( _Stage__34_immI + _Stage__34_rf1 );
        _Stage__34__tmp_19 = unpack( pack( _Stage__34_tmp ) );
        _Stage__34_ctmp = _Stage__34__tmp_19;
        _Stage__34_memaddr = unpack( pack( ( _Stage__34_ctmp >> 2'd2 ) ) [ 15 : 0 ] );
        _Stage__34_boff = unpack( pack( _Stage__34_ctmp ) [ 1 : 0 ] );
    end
    if ( ( _Stage__34___condStage__46 == 2'd2 ))
    begin
        _Stage__34_memaddr = 16'd0;
        _Stage__34_boff = 2'd0;
    end
    Bool _Stage__53_writerd = fifo_Stage__47_TO_Stage__53.first().writerd;
    MemId#(8) _Stage__53__request_3 = fifo_Stage__47_TO_Stage__53.first()._request_3;
    Int#(32) _Stage__53_wdata = fifo_Stage__47_TO_Stage__53.first().wdata;
    UInt#(2) _Stage__53_boff = fifo_Stage__47_TO_Stage__53.first().boff;
    Int#(16) _Stage__53_pc = fifo_Stage__47_TO_Stage__53.first().pc;
    MemId#(8) _Stage__53__request_4 = fifo_Stage__47_TO_Stage__53.first()._request_4;
    UInt#(3) _Stage__53_funct3 = fifo_Stage__47_TO_Stage__53.first().funct3;
    Bool _Stage__53_done = fifo_Stage__47_TO_Stage__53.first().done;
    UInt#(5) _Stage__53_rd = fifo_Stage__47_TO_Stage__53.first().rd;
    Bool _Stage__53_isJal = fifo_Stage__47_TO_Stage__53.first().isJal;
    Bool _Stage__53_isJalr = fifo_Stage__47_TO_Stage__53.first().isJalr;
    Maybe#( _lidTyp_rf ) _Stage__53__lock_id_rf_rd = fifo_Stage__47_TO_Stage__53.first()._lock_id_rf_rd;
    Int#(32) _Stage__53_insn = fifo_Stage__47_TO_Stage__53.first().insn;
    Int#(32) _Stage__53_alu_res = fifo_Stage__47_TO_Stage__53.first().alu_res;
    Bool _Stage__53_isLoad = fifo_Stage__47_TO_Stage__53.first().isLoad;
    UInt#(2) _Stage__53___condStage__52 = fifo_Stage__47_TO_Stage__53.first().__condStage__52;
    UInt#(3) _Stage__53__threadID = fifo_Stage__47_TO_Stage__53.first()._threadID;
    UInt#(1) _Stage__53___condStage__65 = ?;
    UInt#(1) _Stage__53___condStage__62 = ?;
    UInt#(5) _Stage__53__tmp_rf_rd_lock_var_W = ?;
    Int#(32) _Stage__53_insnout = ?;
    UInt#(1) _Stage__53___condStage__60 = ?;
    Int#(16) _Stage__53_nextpc = ?;
    UInt#(1) _Stage__53___condStage__69 = ?;
    if ( ( _Stage__53___condStage__52 == 2'd0 ))
    begin
        _Stage__53_wdata = dmem.mem.peekResp(_Stage__53__request_3);
    end
    _Stage__53___condStage__65 = ( _Stage__53_writerd ? 1'd0 : 1'd1 );
    if ( ( _Stage__53___condStage__65 == 1'd0 ))
    begin
        _Stage__53___condStage__62 = ( _Stage__53_isLoad ? 1'd0 : 1'd1 );
        _Stage__53__tmp_rf_rd_lock_var_W = _Stage__53_rd;
    end
    if ( ( ( _Stage__53___condStage__65 == 1'd0 ) && ( _Stage__53___condStage__62 == 1'd0 ) ))
    begin
        _Stage__53_insnout = maskLoad(_Stage__53_wdata, _Stage__53_funct3, _Stage__53_boff);
    end
    if ( ( ( _Stage__53___condStage__65 == 1'd0 ) && ( _Stage__53___condStage__62 == 1'd1 ) ))
    begin
        _Stage__53___condStage__60 = ( ( _Stage__53_isJal || _Stage__53_isJalr ) ? 1'd0 : 1'd1 );
    end
    if ( ( ( _Stage__53___condStage__65 == 1'd0 ) && ( ( _Stage__53___condStage__62 == 1'd1 ) && ( _Stage__53___condStage__60 == 1'd0 ) ) ))
    begin
        _Stage__53_nextpc = ( _Stage__53_pc + 16'd1 );
        _Stage__53_insnout = unpack( { pack( 16'd0 ), pack( ( _Stage__53_nextpc << 2'd2 ) ) } );
    end
    if ( ( ( _Stage__53___condStage__65 == 1'd0 ) && ( ( _Stage__53___condStage__62 == 1'd1 ) && ( _Stage__53___condStage__60 == 1'd1 ) ) ))
    begin
        _Stage__53_insnout = _Stage__53_alu_res;
    end
    _Stage__53___condStage__69 = ( _Stage__53_done ? 1'd0 : 1'd1 );
    Int#(32) _Stage__47_rf2 = fifo_Stage__34_TO_Stage__47.first().rf2;
    Bool _Stage__47_writerd = fifo_Stage__34_TO_Stage__47.first().writerd;
    UInt#(2) _Stage__47_boff = fifo_Stage__34_TO_Stage__47.first().boff;
    Int#(16) _Stage__47_pc = fifo_Stage__34_TO_Stage__47.first().pc;
    UInt#(3) _Stage__47_funct3 = fifo_Stage__34_TO_Stage__47.first().funct3;
    Bool _Stage__47_isStore = fifo_Stage__34_TO_Stage__47.first().isStore;
    Bool _Stage__47_done = fifo_Stage__34_TO_Stage__47.first().done;
    UInt#(5) _Stage__47_rd = fifo_Stage__34_TO_Stage__47.first().rd;
    Bool _Stage__47_isJal = fifo_Stage__34_TO_Stage__47.first().isJal;
    Bool _Stage__47_isJalr = fifo_Stage__34_TO_Stage__47.first().isJalr;
    UInt#(16) _Stage__47_memaddr = fifo_Stage__34_TO_Stage__47.first().memaddr;
    Maybe#( _lidTyp_rf ) _Stage__47__lock_id_rf_rd = fifo_Stage__34_TO_Stage__47.first()._lock_id_rf_rd;
    Int#(32) _Stage__47_insn = fifo_Stage__34_TO_Stage__47.first().insn;
    Int#(32) _Stage__47_alu_res = fifo_Stage__34_TO_Stage__47.first().alu_res;
    Bool _Stage__47_isLoad = fifo_Stage__34_TO_Stage__47.first().isLoad;
    UInt#(3) _Stage__47__threadID = fifo_Stage__34_TO_Stage__47.first()._threadID;
    UInt#(2) _Stage__47___condStage__52 = ?;
    UInt#(16) _Stage__47_raddr = ?;
    UInt#(16) _Stage__47__tmp_dmem_raddr_lock_var_R = ?;
    UInt#(16) _Stage__47_waddr = ?;
    UInt#(5) _Stage__47_nboff = ?;
    Int#(32) _Stage__47_msg = ?;
    Int#(32) _Stage__47_wdata = ?;
    UInt#(16) _Stage__47__tmp_dmem_waddr_lock_var_W = ?;
    _Stage__47___condStage__52 = ( _Stage__47_isLoad ? 2'd0 : ( _Stage__47_isStore ? 2'd1 : 2'd2 ) );
    if ( ( _Stage__47___condStage__52 == 2'd0 ))
    begin
        _Stage__47_raddr = _Stage__47_memaddr;
        _Stage__47__tmp_dmem_raddr_lock_var_R = _Stage__47_raddr;
    end
    if ( ( _Stage__47___condStage__52 == 2'd1 ))
    begin
        _Stage__47_waddr = _Stage__47_memaddr;
        _Stage__47_nboff = unpack( { pack( _Stage__47_boff ), pack( 3'd0 ) } );
        _Stage__47_msg = ( _Stage__47_rf2 << _Stage__47_nboff );
        _Stage__47_wdata = 32'd0;
        _Stage__47__tmp_dmem_waddr_lock_var_W = _Stage__47_waddr;
    end
    if ( ( _Stage__47___condStage__52 == 2'd2 ))
    begin
        _Stage__47_wdata = 32'd0;
    end
    Int#(16) _Start_pc = fifo__input__TO_Start.first().pc;
    UInt#(3) _Start__threadID = fifo__input__TO_Start.first()._threadID;
    UInt#(16) _Start__tmp_0 = ?;
    _Start__tmp_0 = unpack( pack( _Start_pc ) );
    rule s_Stage__13_execute ;
        UInt#(1) _Stage__13__request_2 = ?;
        if ( ( _Stage__13___condStage__29 == 1'd0 ))
        begin
            fifo__input__TO_Start.enq(E__input__TO_Start { pc : _Stage__13_carg_5,_threadID : _Stage__13__threadID });
        end
        if ( ( _Stage__13___condStage__33 == 1'd0 ))
        begin
            _Stage__13__request_2 <- div.req(_Stage__13_carg_6, _Stage__13_carg_7, _Stage__13_carg_8, _Stage__13_carg_9, _Stage__13_carg_10, _Stage__13_carg_11);
        end
        fifo_Stage__0_TO_Stage__13.deq();
        fifo_Stage__13_TO_Stage__34.enq(E_Stage__13_TO_Stage__34 { _lock_id_rf_rd : _Stage__13__lock_id_rf_rd,immI : _Stage__13_immI,isDiv : _Stage__13_isDiv,isJal : _Stage__13_isJal,immS : _Stage__13_immS,immU : _Stage__13_immU,rf1 : _Stage__13_rf1,flip : _Stage__13_flip,writerd : _Stage__13_writerd,rd : _Stage__13_rd,isMDiv : _Stage__13_isMDiv,insn : _Stage__13_insn,funct3 : _Stage__13_funct3,isLoad : _Stage__13_isLoad,_threadID : _Stage__13__threadID,__condStage__33 : _Stage__13___condStage__33,isAui : _Stage__13_isAui,isStore : _Stage__13_isStore,alu_arg2 : _Stage__13_alu_arg2,rf2 : _Stage__13_rf2,_request_2 : _Stage__13__request_2,invertRes : _Stage__13_invertRes,udivout : _Stage__13_udivout,done : _Stage__13_done,isJalr : _Stage__13_isJalr,isLui : _Stage__13_isLui,pc : _Stage__13_pc });
    endrule
    rule s_Stage__0_execute (( ( ! ( _Stage__0___condStage__4 == 1'd0 ) ) || rf.lock.isEmpty(_Stage__0_rs1) ) && ( ( ! ( _Stage__0___condStage__8 == 1'd0 ) ) || rf.lock.isEmpty(_Stage__0_rs2) ) && ( ( ! ( _Stage__0___condStage__12 == 1'd0 ) ) || rf.lock.canRes(_Stage__0_rd) ) && imem.mem.checkRespId(_Stage__0__request_0));
        Maybe#( _lidTyp_rf ) _Stage__0__lock_id_rf_rd = tagged Invalid;
        imem.mem.resp(_Stage__0__request_0);
        if ( ( _Stage__0___condStage__12 == 1'd0 ))
        begin
            let __tmp_0 <- rf.lock.res(_Stage__0_rd);
            _Stage__0__lock_id_rf_rd = tagged Valid __tmp_0;
        end
        fifo_Start_TO_Stage__0.deq();
        fifo_Stage__0_TO_Stage__13.enq(E_Stage__0_TO_Stage__13 { pc : _Stage__0_pc,isLoad : _Stage__0_isLoad,isMDiv : _Stage__0_isMDiv,isStore : _Stage__0_isStore,immB : _Stage__0_immB,immS : _Stage__0_immS,isAui : _Stage__0_isAui,isDiv : _Stage__0_isDiv,done : _Stage__0_done,writerd : _Stage__0_writerd,isJalr : _Stage__0_isJalr,immU : _Stage__0_immU,_lock_id_rf_rd : _Stage__0__lock_id_rf_rd,rf1 : _Stage__0_rf1,isBranch : _Stage__0_isBranch,isOpImm : _Stage__0_isOpImm,insn : _Stage__0_insn,immJ : _Stage__0_immJ,isLui : _Stage__0_isLui,rf2 : _Stage__0_rf2,_threadID : _Stage__0__threadID,immI : _Stage__0_immI,immJR : _Stage__0_immJR,isJal : _Stage__0_isJal,funct3 : _Stage__0_funct3,flip : _Stage__0_flip,rd : _Stage__0_rd });
    endrule
    rule s_Stage__34_execute (( ( ! ( _Stage__34___condStage__33 == 1'd0 ) ) || div.checkHandle(_Stage__34__request_2) ));
        if ( ( _Stage__34___condStage__33 == 1'd0 ))
        begin
            div.resp();
        end
        fifo_Stage__13_TO_Stage__34.deq();
        fifo_Stage__34_TO_Stage__47.enq(E_Stage__34_TO_Stage__47 { isLoad : _Stage__34_isLoad,isJal : _Stage__34_isJal,pc : _Stage__34_pc,rd : _Stage__34_rd,alu_res : _Stage__34_alu_res,boff : _Stage__34_boff,funct3 : _Stage__34_funct3,memaddr : _Stage__34_memaddr,_lock_id_rf_rd : _Stage__34__lock_id_rf_rd,writerd : _Stage__34_writerd,insn : _Stage__34_insn,rf2 : _Stage__34_rf2,done : _Stage__34_done,_threadID : _Stage__34__threadID,isStore : _Stage__34_isStore,isJalr : _Stage__34_isJalr });
    endrule
    rule s_Stage__53_execute (( ( ! ( _Stage__53___condStage__69 == 1'd0 ) ) || busyReg ) && ( ( ! ( _Stage__53___condStage__65 == 1'd0 ) ) || rf.lock.owns(fromMaybe( ? , _Stage__53__lock_id_rf_rd ), _Stage__53_rd) ) && ( ( ! ( _Stage__53___condStage__52 == 2'd1 ) ) || dmem.mem.checkRespId(_Stage__53__request_4) ) && ( ( ! ( _Stage__53___condStage__52 == 2'd0 ) ) || dmem.mem.checkRespId(_Stage__53__request_3) ));
        $display( "PC: %h",( _Stage__53_pc << 2'd2 ) );
        $display( "INSN: %h",_Stage__53_insn );
        if ( ( _Stage__53___condStage__52 == 2'd1 ))
        begin
            dmem.mem.resp(_Stage__53__request_4);
        end
        if ( ( _Stage__53___condStage__52 == 2'd0 ))
        begin
            dmem.mem.resp(_Stage__53__request_3);
        end
        if ( ( _Stage__53___condStage__65 == 1'd0 ))
        begin
            $display( "Writing %d to r%d",_Stage__53_insnout,_Stage__53_rd );
            rf.write(_Stage__53__tmp_rf_rd_lock_var_W, _Stage__53_insnout);
        end
        if ( ( _Stage__53___condStage__69 == 1'd0 ))
        begin
            busyReg <= False;
            outputQueue.enq(OutputQueueInfo { data : True,handle : _Stage__53__threadID });
        end
        if ( ( _Stage__53___condStage__65 == 1'd0 ))
        begin
            rf.lock.rel(fromMaybe( ? , _Stage__53__lock_id_rf_rd ), _Stage__53_rd);
        end
        fifo_Stage__47_TO_Stage__53.deq();
    endrule
    rule s_Stage__47_execute (( ( ! ( _Stage__47___condStage__52 == 2'd0 ) ) || dmem.lock.isEmpty(_Stage__47_raddr) ) && ( ( ! ( _Stage__47___condStage__52 == 2'd1 ) ) || dmem.lock.isEmpty(_Stage__47_waddr) ));
        MemId#(8) _Stage__47__request_3 = ?;
        MemId#(8) _Stage__47__request_4 = ?;
        if ( ( _Stage__47___condStage__52 == 2'd0 ))
        begin
            _Stage__47__request_3 <- dmem.mem.req(_Stage__47__tmp_dmem_raddr_lock_var_R, ?, 0);
        end
        if ( ( _Stage__47___condStage__52 == 2'd1 ))
        begin
            _Stage__47__request_4 <- dmem.mem.req(_Stage__47__tmp_dmem_waddr_lock_var_W, _Stage__47_msg, pack( storeMask(_Stage__47_boff, _Stage__47_funct3) ));
        end
        fifo_Stage__34_TO_Stage__47.deq();
        fifo_Stage__47_TO_Stage__53.enq(E_Stage__47_TO_Stage__53 { funct3 : _Stage__47_funct3,insn : _Stage__47_insn,_request_4 : _Stage__47__request_4,_lock_id_rf_rd : _Stage__47__lock_id_rf_rd,boff : _Stage__47_boff,done : _Stage__47_done,_threadID : _Stage__47__threadID,rd : _Stage__47_rd,isJal : _Stage__47_isJal,_request_3 : _Stage__47__request_3,pc : _Stage__47_pc,wdata : _Stage__47_wdata,writerd : _Stage__47_writerd,__condStage__52 : _Stage__47___condStage__52,isLoad : _Stage__47_isLoad,isJalr : _Stage__47_isJalr,alu_res : _Stage__47_alu_res });
    endrule
    rule s_Start_execute (imem.lock.isEmpty());
        MemId#(8) _Start__request_0 = ?;
        _Start__request_0 <- imem.mem.req(_Start__tmp_0, ?, 0);
        fifo__input__TO_Start.deq();
        fifo_Start_TO_Stage__0.enq(E_Start_TO_Stage__0 { _request_0 : _Start__request_0,pc : _Start_pc,_threadID : _Start__threadID });
    endrule
    method ActionValue#(UInt#(3)) req ( Int#(16) pc ) if( ( ! busyReg ) );
        fifo__input__TO_Start.enq(E__input__TO_Start { pc : pc,_threadID : _threadID });
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
