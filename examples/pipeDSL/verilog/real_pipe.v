module CPU(clk, reset);
   input clk, reset;
   
   localparam FALSE = 1'b0,
     TRUE = 1'b1;
   
   localparam CACHE_READ = 1'b0,
     CACHE_WRITE = 1'b1;

   localparam STAGE_1 = 3'd0,
     STAGE_2 = 3'd1,
     STAGE_3A = 3'd2,
     STAGE_3B = 3'd3,
     STAGE_3B_2 = 3'd4;

   localparam OP_LOAD = 7'b0000011,
     OP_STORE = 7'b0100011,
     OP_ARITH = 7'b0110011,
     OP_BRANCH = 7'b1100011;

   function automatic [6:0] opcode_bits;
      input [31:0] instruction;
      begin
	 opcode_bits = instruction[6:0];
      end
   endfunction // if

   function automatic [4:0] rd_bits;
      input [31:0] instruction;
      begin
	 rd_bits = instruction[11:7];
      end 
   endfunction // if

   function automatic [2:0] func3_bits;
      input [31:0] instruction;
      begin
	 func3_bits = instruction[14:12];
      end
   endfunction // if

   function automatic [4:0] rs1_bits;
      input [31:0] instruction;
      begin
	 rs1_bits = instruction[19:15];
      end
   endfunction

   function automatic [4:0] rs2_bits;
      input [31:0] instruction;
      begin
	 rs2_bits = instruction[24:20];
      end
   endfunction // if

   function automatic [11:0] imm12_bits;
      input [31:0] instruction;
      begin
	 imm12_bits = instruction[31:20];
      end
   endfunction      

   function automatic [6:0] func7_bits;
      input [31:0] instruction;
      begin
	 func7_bits = instruction[31:25];
      end
   endfunction // if
   
   function automatic take_br;
      input [6:0] opcode;
      input [31:0] arg1, arg2;
      begin
      if (opcode == OP_BRANCH)
	 take_br = arg1 == arg2;
      else
	 take_br = FALSE;
      end
   endfunction

   //Sequential
   reg[31:0]  s1_pc;
   reg 	      s1_pc_valid; 
   //Combinational
   wire       s1_valid, s1_ready;
   
   wire       s1_imem_ready, s1_imem_valid;
   wire [31:0]       s1_imem_addr;
   //Seq
   reg [31:0] s2_pc;
   reg 	      s2_pc_valid;
   reg [31:0] s2_insn;
   reg 	      s2_insn_valid;
   reg 	       s2_rf_rw;

   //Comb
   wire       s2_valid, s2_ready, s2_imem_valid, s2_imem_ready;
   wire [31:0] s2_imem_data;
        
   
   cache  imem(.clk (clk),
	       .reset (reset),
	       .ready_in (s1_imem_ready),
	       .valid_in (s1_imem_valid),
	       .addr_in  (s1_imem_addr),
	       .op_in (CACHE_READ), //constant read
	       .write_data_in (32'b0), //not used
	       .ready_out (s2_imem_ready),
	       .valid_out (s2_imem_valid),
	       .data_out  (s2_imem_data));
   //Comb
   wire [6:0]  s2_opcode;
   wire [31:0] s2_rs1,s2_rs2,s2_dest;
   wire [2:0]  s2_next_stage;
   wire [4:0]  rf_write_addr, rf_read_1_addr, rf_read_2_addr;   
   wire [31:0] rf_write_val;
   wire [31:0] rf_read_1_out, rf_read_2_out;
   wire 	       rf_read_1_en, rf_read_2_en, rf_write_en;
	       
   regfile  rf(.clk(clk),
	       .write_en (rf_write_en),
	       .write_addr (rf_write_addr),
	       .write_val (rf_write_val),
	       .read_1_addr(rf_read_1_addr),
	       .read_1_en(rf_read_1_en),
	       .read_1_out(rf_read_1_out),
	       .read_2_addr(rf_read_2_addr),
	       .read_2_en(rf_read_2_en),
	       .read_2_out(rf_read_2_out));
   //Seq
   reg [31:0]  sa3_pc;
   reg 	       sa3_pc_valid;
   reg [31:0]  sa3_insn;
   reg 	       sa3_insn_valid;
   reg [6:0]   sa3_opcode;
   reg         sa3_opcode_valid;
   reg [31:0]  sa3_arg1;
   reg 	       sa3_arg1_valid;   
   reg [31:0]  sa3_arg2;
   reg 	       sa3_arg2_valid;
   //Comb
   wire        sa3_ready;
   wire [31:0] sa3_next_cpu;
   
   //Seq
   reg [31:0]  sb3_pc;
   reg 	       sb3_pc_valid;
   reg [6:0]   sb3_opcode;
   reg 	       sb3_opcode_valid;
   reg [31:0]  sb3_insn;
   reg 	       sb3_insn_valid;
   reg [31:0]  sb3_arg1;
   reg 	       sb3_arg1_valid;   
   reg [31:0]  sb3_arg2;
   reg 	       sb3_arg2_valid;
   reg [4:0]   sb3_dest;
   reg 	       sb3_dest_valid;
 	       
   //Comb
   wire        sb3_ready, sb3_case_arith, sb3_case_ld, sb3_case_st;
   wire [31:0] alu_arg_1_in, alu_arg_2_in;
   wire [31:0] alu_result_out;
   wire [31:0] sb3_result;

   wire [6:0]   alu_op_in;

   alu arith_unit(.arg_1 (alu_arg_1_in),
	    .arg_2 (alu_arg_2_in),
	    .alu_op (alu_op_in),
	    .result (alu_result_out));
   //Comb
   wire [31:0]  dmem_addr, dmem_write_data_in;
   wire         dmem_valid_in, dmem_ready_out, dmem_op;
   wire [31:0] dmem_data;   
   wire        dmem_ready_in, dmem_valid_out;
   
   cache dmem(.clk(clk),
	      .reset(reset),
	      .ready_in (dmem_ready_in),
	      .valid_in (dmem_valid_in),
	      .addr_in  (dmem_addr),
	      .write_data_in  (dmem_write_data_in),
	      .op_in    (dmem_op),
	      .ready_out (dmem_ready_out),
	      .valid_out (dmem_valid_out),
	      .data_out  (dmem_data));
   //Seq
   reg 	       sb3_2_valid, sb3_2_val_valid;
   reg [6:0]   sb3_2_opcode;
   reg [31:0]  sb3_2_res, sb3_2_val;
   reg [4:0]   sb3_2_dest;
   
   
   //Comb
   wire        sb3_2_ready, sb3_2_case_arith, sb3_2_case_ld, sb3_2_case_st;
   

   initial begin
//      $monitor($time, " s1_pc = %h, s1_valid=%b,imemr=%b,imemv=%b", s1_pc, s1_valid,s1_imem_ready,s1_imem_valid);
      $monitor($time, " 1->2=%b, 2->3a=%b, 2->3b=%b, 2->1=%b, 3a->1=%b, 3b->3b2=%b",s1_to_s2, s2_to_sa3, s2_to_sb3, s2_to_s1, sa3_to_s1, sb3_to_sb3_2);
//      $monitor($time, " s2_pc = %h, s2_valid=%b, s2_opcode=%d,2->3a=%b, 2->3b=%b, next_stage=%b, sb3r=%b, s1r=%b",
//	       s2_pc, s2_valid, s2_opcode, s2_to_sa3, s2_to_sb3, s2_next_stage, sb3_ready, s1_ready);
      
   end
     
   always@(posedge clk) begin
      if (reset) begin
	 s1_pc <= 0;
	 s1_pc_valid <= TRUE;
	 s2_rf_rw <= TRUE;
	 s2_pc <= 0;
	 s2_pc_valid <= FALSE;
	 s2_insn <= 0;
	 s2_insn_valid <= FALSE;
	 sa3_pc <= 0;
	 sa3_pc_valid <= FALSE;
	 sa3_insn <= 0;
	 sa3_insn_valid <= FALSE;
	 sa3_opcode <= 0;
	 sa3_opcode_valid <= FALSE;
	 sa3_arg1 <= 0;
	 sa3_arg1_valid <= FALSE;
	 sa3_arg2 <= 0;
	 sa3_arg2_valid <= FALSE;
	 sb3_pc <= 0;
	 sb3_pc_valid <= FALSE;
	 sb3_opcode <= 0;
	 sb3_opcode_valid <= FALSE;
	 sb3_insn <= 0;
	 sb3_insn_valid <= FALSE;
	 sb3_arg1 <= 0;
	 sb3_arg1_valid <= FALSE;
	 sb3_arg2 <= 0;
	 sb3_arg2_valid <= FALSE;
	 sb3_dest <= 0;
	 sb3_dest_valid <= FALSE;
	 sb3_2_valid <= FALSE;
	 sb3_2_val_valid <= FALSE;
	 sb3_2_opcode <= 0;
	 sb3_2_res <= 0;
	 sb3_2_val <= 0;
	 sb3_2_dest <= 0;
      end
   end

//pipeline stages:
//s1 (imem access) -> s2
//s2 (imem recv, rf access) ->  sa3 || (sb3 && s1)
//sa3 (br) -> s1
//sb3 (alu/ld/st) -> sb3_2
//sb3_2 -> none

//         START   
//           |
//           V
// --------> s1 <--------
// |         |          |
// |         s2         |
// |   ______|_____     |
// |  /            \----|
// |  V            V
// |-sa3           sb3
//                  |   
//                  V   
//                sb3_2

// if (s1 valid && imem ready && s2 ready) : s1 -> s2
// if (s2 valid && insn valid && rf_can_rw && sa3 ready && op == "br") : s2 -> sa3
// if (s2 valid && insn valid && rf_can_rw && sb3 ready && op != "br" && s1 ready) : s2 -> sb3 && s2 -> s1
// if (sa3 valid && s1 ready) : sa3 -> s1

// if (sb3 valid && op == "arith" && sb3_2 ready) : sb3 -> sb3_2
// if (sb3 valid && op == "ld" || op == "st" && dmem_ready && sb3_2 ready) : sb3 -> sb3_2

//Valid signals and ready valid control logic

   wire s1_to_s2, s2_to_sa3, s2_to_sb3, s2_to_s1, sa3_to_s1, sb3_to_sb3_2, sb3_2_to_done;
   
   //when are all of stage 1 inputs valid
   assign s1_valid = s1_pc_valid;
   //when are all stage 1 outputs valid and all
   //stage 1 receivers are ready
   assign s1_to_s2 = s1_imem_ready & s2_ready & s1_valid;
   //when are all of stage 2 inputs valid
   assign s2_valid = s2_pc_valid & s2_insn_valid;   
   //when are all stage 2 inputs valid
   //and all stage 2 receivers are ready
   assign s2_to_sa3 = s2_valid & (s2_next_stage == STAGE_3A) & sa3_ready & s2_rf_rw;
   //send both to s1 and sb3 since this executes a `spawn`
   assign s2_to_sb3 = s2_valid & s2_next_stage == STAGE_3B & sb3_ready & s1_ready & s2_rf_rw;
   assign s2_to_s1 = s2_to_sb3;
   //when are all of the stage 3a inputs valid
   assign sa3_valid = sa3_insn_valid & sa3_arg1_valid & sa3_arg2_valid & sa3_opcode_valid;
   //when s3a inputs are valid and s1 receivers are ready
   assign sa3_to_s1 = sa3_valid && s1_ready;
   //when sb3 inputs are all valid
   assign sb3_valid = sb3_insn_valid & sb3_arg1_valid & sb3_arg2_valid & sb3_opcode_valid;
   //when are all of the stage 3b inputs are valid and stage3b receivers are ready
   wire dmem_rv;
   assign dmem_rv = sb3_case_arith | (dmem_ready_in & dmem_valid_in);
   assign sb3_to_sb3_2 = sb3_valid & dmem_rv & sb3_2_ready;
   assign sb3_2_to_done = sb3_2_valid & ((sb3_2_case_arith) | (sb3_2_val_valid & sb3_2_case_ld));
   
   //stage 1
   //first establish datapath connections
   assign s1_imem_valid = s1_pc_valid;
   assign s1_imem_addr = s1_pc;
   //now readiness
   assign s1_ready = !s1_pc_valid;

//execute the 1 cycle latency operations
always@(posedge clk) begin
   if (s1_to_s2) begin
      s2_pc <= s1_pc;
      s2_pc_valid <= s1_pc_valid;
   end
end

   //stage 2
   //first establish datapath connections
   //ready when we don't have an instruction in the s2 stage already
   assign s2_imem_ready = ~s2_insn_valid;
   assign s2_opcode = opcode_bits(s2_insn);
   assign s2_rs1 = rs1_bits(s2_insn);
   assign s2_rs2 = rs2_bits(s2_insn);   
   assign s2_dest = rd_bits(s2_insn);   
   assign s2_next_stage = (s2_opcode == OP_BRANCH) ? STAGE_3A : STAGE_3B; //comes from the if
   assign rf_read_1_addr = s2_rs1;
   assign rf_read_2_addr = s2_rs2;
   assign rf_read_1_en = s2_to_sa3 | s2_to_sb3;
   assign rf_read_2_en = s2_to_sa3 | s2_to_sb3;
   assign s2_next_cpu = s2_pc + 32'd4;

   assign s2_ready = !s2_pc_valid | s2_to_sa3 | s2_to_sb3;
   
   //receive input from imem   
   always@(posedge clk) begin
      if (s2_imem_valid && s2_imem_ready) begin
	 s2_insn <= s2_imem_data;
	 s2_insn_valid <= TRUE;
      end
   end
   //execute the 1 cycle latency operations
   always@(posedge clk) begin
      if (s2_to_sa3) begin
	 sa3_pc <= s2_pc;
	 sa3_pc_valid <= s2_pc_valid;
	 sa3_insn <= s2_insn;
	 sa3_insn_valid <= s2_insn_valid;
	 sa3_opcode <= s2_opcode;
	 sa3_opcode_valid <= TRUE;      
	 sa3_arg1 <= rf_read_1_out;
	 sa3_arg1_valid <= TRUE;      
	 sa3_arg2 <= rf_read_2_out;
	 sa3_arg2_valid <= TRUE;
      end
      if (s2_to_sb3) begin
	 //this also sends a new pc to s1
	 s1_pc <= s2_next_cpu;      
	 s1_pc_valid <= s2_pc_valid;
	 //
	 sb3_pc <= s2_pc;
	 sb3_pc_valid <= s2_pc_valid;
	 sb3_insn <= s2_insn;
	 sb3_insn_valid <= s2_insn_valid;      
	 sb3_opcode <= s2_opcode;
	 sb3_opcode_valid <= TRUE;
	 sb3_arg1 <= rf_read_1_out;      
	 sb3_arg1_valid <= TRUE;      
	 sb3_arg2 <= rf_read_2_out;
	 sb3_arg2_valid <= TRUE;
	 sb3_dest <= s2_dest;
	 sb3_dest_valid <= TRUE;      
      end
   end

   //sa3+sb3 readieness
   assign sb3_ready = !sb3_insn_valid | sb3_to_sb3_2;
   assign sa3_ready = !sa3_insn_valid | sa3_to_s1;
   
   //stage sa3 (BR instruction) datapath
   assign sa3_take_br = take_br(sa3_opcode, sa3_arg1, sa3_arg2);
   assign sa3_next_cpu = sa3_pc + ((sa3_take_br) ? { 18'b0, sa3_insn[31:18] } : 32'd4);


   //stage sa3 1 cycle latency operations
   always@(posedge clk) begin
      if (sa3_to_s1) begin
	 s1_pc <= sa3_next_cpu;
	 s1_pc_valid <= TRUE;
      end
   end

   //stage sb3 (OTHER INSTS) datapath
   assign sb3_case_arith = sb3_opcode == OP_ARITH;
   assign sb3_case_ld = sb3_opcode == OP_LOAD;
   assign sb3_case_st = sb3_opcode == OP_STORE;

   assign alu_arg_1_in = sb3_arg1;
   assign alu_arg_2_in = sb3_arg2;
   assign alu_op_in = func7_bits(sb3_insn);
   assign sb3_result = alu_result_out;

   assign sb3_dmem_addr = sb3_arg1 + { 18'b0, sb3_insn[31:18] };   
   assign dmem_addr = sb3_dmem_addr;
   assign dmem_op = (sb3_case_st) ? CACHE_WRITE : CACHE_READ;   
   assign dmem_write_data_in = (sb3_case_st) ? sb3_arg2 : 32'b0;   
   assign dmem_valid_in = sb3_pc_valid & (sb3_case_ld | sb3_case_st);

   //stage sb3 1 cycle latency operations
   always@(posedge clk) begin
      if (sb3_to_sb3_2) begin
	 sb3_2_valid <= TRUE;      
	 sb3_2_opcode <= sb3_opcode;      
	 sb3_2_res <= sb3_result;
	 sb3_2_dest <= sb3_dest;
      end
   end

   //readieness
   assign sb3_2_ready = !sb3_2_valid | sb3_2_to_done;
   
   //receive input from dmem   
   always@(posedge clk) begin
      if (dmem_ready_out && dmem_valid_out) begin
	 sb3_2_val <= dmem_data;      
	 sb3_2_val_valid <= TRUE;
      end
   end

   //stage sb3_2 datapath
   assign sb3_2_case_arith = sb3_2_opcode == OP_ARITH;
   assign sb3_2_case_ld = sb3_2_opcode == OP_LOAD;
   assign sb3_2_case_st = sb3_2_opcode == OP_STORE;
   
   assign rf_write_en = sb3_2_to_done;
   assign rf_write_addr = sb3_2_dest;
   assign rf_write_val = (sb3_case_ld) ? sb3_2_val : sb3_2_res;


   //clear all of the valid bits:
   always@(posedge clk) begin
      if (s1_to_s2 & !(s2_to_s1 | sa3_to_s1)) begin
	 s1_pc_valid <= FALSE;
      end
      if ((s2_to_sa3 | s2_to_sb3) & !(s1_to_s2)) begin
	 s2_pc_valid <= FALSE;
	 s2_insn_valid <= FALSE;
      end
      if (sa3_to_s1 & !s2_to_sa3) begin
	 sa3_insn_valid <= FALSE;
	 sa3_arg1_valid <= FALSE;
	 sa3_arg2_valid <= FALSE;
	 sa3_opcode_valid <= FALSE;
      end
      if (sb3_to_sb3_2 & !s2_to_sb3) begin
	 sb3_insn_valid <= FALSE;
	 sb3_arg1_valid <= FALSE;
	 sb3_arg2_valid <= FALSE;
	 sb3_opcode_valid <= FALSE;
      end
      if (sb3_2_to_done & !sb3_to_sb3_2) begin
	 sb3_2_valid <= FALSE;
	 sb3_2_val_valid <= FALSE;
      end
end

endmodule
