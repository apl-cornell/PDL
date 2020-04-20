module CPU;

   localparam CACHE_READ = 1'b0,
     CACHE_WRITE = 1'b1;

   localparam STAGE_1 = 3'd0,
     STAGE_2 = 3'd1,
     STAGE_3A = 3'd2,
     STAGE_3B = 3'd3,
     STAGE_3B_2 = 3'd4;

   localparam OP_LD = 3'd0,
     OP_ST = 3'd1,
     OP_ADD = 3'd2,
     OP_MUL = 3'd3,
     OP_SUB = 3'd4,
     OP_DIV = 3'd5;

   //Sequential
   reg[31:0]  s1_pc;
   reg 	      s1_pc_valid; 
   //Combinational
   reg       s1_valid; 	     
   reg       s1_ready;
   
   wire       s1_imem_ready;
   reg        s1_imem_valid;
   reg [31:0]       s1_imem_addr;
   //Seq
   reg [31:0] s2_pc;
   reg 	      s2_pc_valid;

   reg [31:0] s2_insn;
   reg 	      s2_insn_valid;   

   //Comb
   reg       s2_valid; 	     
   reg       s2_ready;      

   wire         s2_imem_valid;
   reg        s2_imem_ready;
   wire [31:0] s2_imem_data;
   
   
   cache  imem(.ready_in (s1_imem_ready),
	       .valid_in (s1_imem_valid),
	       .addr_in  (s1_imem_addr),
	       .op_in (CACHE_READ), //constant read
	       .write_data_in (32'b0), //not used
	       .ready_out (s2_imem_ready),
	       .valid_out (s2_imem_valid),
	       .data_out  (s2_imem_data));
   //Comb
   reg [2:0]  opcode;
   reg [31:0] s2_rs1,s2_rs2,s2_dest;
   reg [4:0]  s2_next_stage;   

   regfile  rf(.write_en (rf_write_en),
	       .write_addr (rf_write_addr),
	       .write_val (rf_write_val),
	       .read_1_addr(rf_read_1_addr),
	       .read_1_en(rf_read_1_en),
	       .read_1_out(rf_read_1_out),
	       .read_2_addr(rf_read_2_addr),
	       .read_2_en(rf_read_2_en),
	       .read_2_out(rf_read_2_out));
   //Seq
   reg         rf_can_rw;

   reg [31:0]  sa3_pc;
   reg 	       sa3_pc_valid;
   reg [31:0]  sa3_insn;
   reg 	       sa3_insn_valid;
   
   reg [31:0]  sa3_arg1;
   reg 	       sa3_arg1_valid;   
   reg [31:0]  sa3_arg2;
   reg 	       sa3_arg2_valid;

   reg [31:0]  sb3_pc;
   reg 	       sb3_pc_valid;
   reg [2:0]   sb3_opcode;
   reg 	       sb3_opcode_valid;
   reg [31:0]  sb3_insn;
   reg 	       sb3_insn_valid;
   reg [31:0]  sb3_arg1;
   reg 	       sb3_arg1_valid;   
   reg [31:0]  sb3_arg2;
   reg 	       sb3_arg2_valid;   

   //Comb
   reg [31:0]  alu_arg_1_in, alu_arg_2_in, result;
   reg [1:0]   alu_op_in;
   
   alu arith_unit(.arg_1 (alu_arg_1_in),
	    .arg_2 (alu_arg_2_in),
	    .alu_op (alu_op_in),
	    .result (alu_result_out));
   //Comb
   reg [31:0]  dmem_addr, dmem_write_data_in, dmem_data;
   reg 	       dmem_ready_in, dmem_valid_in, dmem_ready_out, dmem_valid_out, dmem_op;
   
   cache dmem(.ready_in (dmem_ready_in),
	       .valid_in (dmem_valid_in),
	       .addr_in  (dmem_addr),
	       .write_data_in  (dmem_write_data_in),
	       .op_in    (dmem_op),
	       .ready_out (dmem_ready_out),
	       .valid_out (dmem_valid_out),
	       .data_out  (dmem_data));
   //Seq
   reg 	       sb3_2_valid;
   reg [4:0]   sb3_2_opcode;
   reg [31:0]  sb3_2_res;
   reg [4:0]   sb3_2_dest;
   
   
   //Comb
   reg 	       sb3_case_arith, sb3_case_ld, sb3_case_st;


   
always@(posedge clk) begin
   if (reset) begin
      s1_pc <= 0;
      s1_pc_valid <= true;
      rf_can_rw <= true;      
      //reset everything else too
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
always@(*) begin
   //when are all of stage 1 inputs valid
   s1_valid = s1_pc_valid;
   //when are all stage 1 outputs valid and all
   //stage 1 receivers are ready
   if (s1_imem_ready && s2_ready && s1_valid) begin
      s1_to_s2 = true;
   end
   else begin
      s1_to_s2 = false;
   end
   //when are all of stage 2 inputs valid
   s2_valid = s2_pc_valid && s2_insn_valid;   
   //when are all stage 2 inputs valid
   //and all stage 2 receivers are ready
   if (s2_valid && s2_next_stage == STAGE_3A 
      		&& sa3_ready && s2_rf_rw) begin
      s2_to_sa3 = true;
   end else begin
      s2_to_sa3 = false;
   end

   //send both to s1 and sb3 since this executes a `spawn`
   if (s2_valid && s2_next_stage == STAGE_3B && sb3_ready
      		&& s1_ready && s2_rf_rw) begin
      s2_to_sb3 = true;
      s2_to_s1 = true;
   end else begin
      s2_to_sb3 = false;
      s2_to_s1 = false;
   end
   //when are all of the stage 3a inputs valid
   sa3_valid = sa3_insn_valid & sa3_arg1_valid
   	       & sa3_arg2_valid & sa3_opcode_valid;
   //when s3a inputs are valid and s1 receivers are ready
   if (sa3_valid && s1_ready) begin
      sa3_to_s1 = true;
   end else begin
      sa3_to_s1 = false;    
   end

   sb3_valid = sb3_insn_valid & sb3_arg1_valid
   	       & sb3_arg2_valid & sb3_opcode_valid;
   //when are all of the stage 3b inputs are valid and stage3b receivers are ready
   if (sb3_valid && (sb3_case_arith || (dmem_ready_in && dmem_valid_in)) && s3_2_ready) begin
      sb3_to_sb3_2 = true;
   end else begin
      sb3_to_sb3_2 = false;      
   end
end

//stage 1
//first establish datapath connections
always@(*) begin
   s1_imem_valid = s1_pc_valid;
   s1_imem_addr = s1_pc;
   //now readiness
   s1_ready = ~s1_pc_valid;
end // always@ begin

//execute the 1 cycle latency operations
always@(posedge clk) begin
   if (s1_to_s2) begin
      s2_pc <= s1_pc;
      s2_pc_valid <= s1_pc_valid;
   end
end

//stage 2
//first establish datapath connections
always@(*) begin
   //ready when we don't have an instruction in the s2 stage already
   s2_imem_ready = ~s2_insn_valid;
   s2_opcode = opcode(s2_insn);   
   s2_rs1 = rs1(s2_insn);
   s2_rs2 = rs2(s2_insn);
   s2_dest = dest(s2_insn);
   s2_next_stage = (op == BR) ? STAGE_3A : STAGE_3B; //comes from the if
   rf_read_1_addr = s2_rs1;
   rf_read_2_addr = s2_rs2;
   rf_read_1_en = s2_to_sa3 | s2_to_sb3;
   rf_read_2_en = s2_to_sa3 | s2_to_sb3;
   s2_next_cpu = s2_pc + 4;
end // always@ begin
   
//receive input from imem   
always@(posedge clk) begin
   if (s2_imem_valid && s2_imem_ready) begin
      s2_insn <= s2_imem_data;
      s2_insn_valid <= true;
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
      sa3_opcode_valid <= true;      
      sa3_arg1 <= rf_read_1_out;
      sa3_arg1_valid <= true;      
      sa3_arg2 <= rf_read_2_out;
      sa3_arg2_valid <= true;
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
      sb3_opcode_valid <= true;
      sb3_arg1 <= rf_read_1_out;      
      sb3_arg1_valid <= true;      
      sb3_arg2 <= rf_read_2_out;
      sb3_arg2_valid <= true;
      sb3_dest <= sb2_dest;
      sb3_dest_valid <= true;      
   end
end

//stage sa3 (BR instruction) datapath
always@(*) begin
   sa3_take_br = br_unit(sa3_opcode, sa3_arg1, sa3_arg2);
   sa3_next_cpu = sa3_pc + ((sa3_take_br) ? imm(sa3_insn) : 4);
end

//stage sa3 1 cycle latency operations
always@(posedge clk) begin
   if (sa3_to_s1) begin
      s1_pc <= sa3_next_cpu;
      s1_pc_valid <= true;
   end
end

//stage sb3 (OTHER INSTS) datapath
always@(*) begin
   sb3_case_arith = sb3_opcode != LD && sb3_opcode != ST;
   sb3_case_ld = sb3_opcode == LD;
   sb3_case_st = sb3_opcode == ST;

   alu_arg_1_in = sb3_arg1;
   alu_arg_2_in = sb3_arg2;
   alu_op_in = aop(sb3_insn);   
   sb3_result = alu_result_out;

   sb3_dmem_addr = sb3_arg1 + imm(sb3_insn);   
   dmem_addr = sb3_dmem_addr;
   dmem_op = (sb3_case_st) ? CACHE_WRITE : CACHE_READ;   
   dmem_write_data_in = (sb3_case_st) ? sb3_arg2 : 32'b0;   
   dmem_valid_in = sb3_pc_valid && (sb3_case_ld || sb3_case_st);

end

//stage sb3 1 cycle latency operations
always@(posedge clk) begin
   if (sb3_to_sb3_2) begin
      sb3_2_valid <= true;      
      sb3_2_opcode <= sb3_opcode;      
      sb3_2_res <= sb3_result;
      sb3_2_dest <= sb3_dest;
   end
end

//receive input from dmem   
always@(posedge clk) begin
   if (dmem_ready_out && dmem_valid_out) begin
      sb3_2_val <= dmem_data;      
      sb3_2_val_valid <= true;      
   end
end

//stage sb3_2 datapath
always@(*) begin
   sb3_2_case_arith = sb3_2_opcode == ARITH;
   sb3_2_case_ld = sb3_2_opcode == LD;
   sb3_2_case_st = sb3_2_opcode == ST;
   
   rf_write_en = sb3_2_valid && (sb3_2_case_ld || sb3_2_case_arith);
   rf_write_addr = sb3_2_dest;
   rf_write_val = (sb3_case_ld) ? sb3_2_val : sb3_2_res;
end // always@ begin


endmodule
