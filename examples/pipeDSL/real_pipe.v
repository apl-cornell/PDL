module CPU ();
   reg[31:0]  s1_pc;
   reg 	      s1_pc_valid;	      

   wire       s1_valid; 	     
   wire       s1_ready;
   
   wire       s1_imem_ready;
   wire       s1_imem_valid;
   wire       s1_imem_addr;
   
   reg [31:0] s2_pc;
   reg 	      s2_pc_valid;

   reg [31:0] s2_insn;
   reg 	      s2_insn_valid;   

   reg        s2_rf_rw;
 	      
   wire       s2_valid; 	     
   wire       s2_ready;      

   wire        s2_imem_valid;
   wire        s2_imem_ready;
   wire [31:0] s2_imem_data;
   
   
   Cache imem (.ready_in (s1_imem_ready),
	       .valid_in (s1_imem_valid),
	       .addr_in  (s1_imem_addr),
	       .op_in (CACHE_READ), //constant read
	       .write_data_in (0), //not used
	       .ready_out (s2_imem_valid),
	       .valid_out (s2_imem_valid),
	       .data_out  (s2_imem_data));
   reg [31:0]  s2_pc;
   reg [31:0]  s2_insn;
   wire [7:0]  opcode;
   wire [31:0] s2_rs1;
   wire [31:0] s2_rs2;
   wire [31:0] s2_dest;
   wire        STAGE s2_next_stage;   

   RegFile rf (.write_en (rf_write_en),
	       .write_addr (rf_write_addr),
	       .write_val (rf_write_val),
	       .read_1_addr(rf_read_1_addr),
	       .read_1_en(rf_read_1_en)
	       .read_1_out(rf_read_1_out),
	       .read_2_addr(rf_read_2_addr),
	       .read_2_en(rf_read_2_en),
	       .read_2_out(rf_read_2_out));

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
   reg [7:0]   sb3_opcode;
   reg 	       sb3_opcode_valid;
   reg [31:0]  sb3_insn;
   reg 	       sb3_insn_valid;
   reg [31:0]  sb3_arg1;
   reg 	       sb3_arg1_valid;   
   reg [31:0]  sb3_arg2;
   reg 	       sb3_arg2_valid;   
   
   ALU alu (.arg_1 (alu_arg_1_in),
	    .arg_2 (alu_arg_2_in),
	    .alu_op (alu_op_in),
	    .result (alu_result_out));

   Cache dmem (.ready_in (dmem_ready_in),
	       .valid_in (dmem_valid_in),
	       .addr_in  (dmem_addr),
	       .write_data_in  (dmem_write_data_in),
	       .op_in    (dmem_op),
	       .ready_out (dmem_ready_out),
	       .valid_out (dmem_valid_out),
	       .data_out  (dmem_data));
   
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

//control logic
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
   to_s2 = s1_to_s2;
   //when are all of stage 2 inputs valid
   s2_valid = s2_pc_valid && s2_insn_valid;   
   //when are all stage 2 inputs valid
   //and all stage 2 receivers are ready
   if (s2_valid && s2_next_stage == "sa" && sa3_ready && s2_rf_rw) begin
      s2_to_sa3 = true;
   end else begin
      s2_to_sa3 = false;
   end
   to_sa3 = s2_to_sa3;
   //send both to s1 and sb3 since this executes a `spawn`
   if (s2_valid && s2_next_stage == "sb" && sb3_ready && s1_ready && s2_rf_rw) begin
      s2_to_sb3 = true;
   end else begin
      s2_to_sb3 = false;
   end
   to_sb3 = s2_to_sb3;   
   //when are all of the stage 3a inputs valid
   sa3_valid = sa3_insn_valid & sa3_arg1_valid & sa3_arg2_valid & sa3_opcode_valid;
   //when s3a inputs are valid and s1 receivers are ready
   //note that this is not quite ready, if s1_pc_valid is true but it's being sent this cycle we can still send
   if (sa3_valid && ~s1_pc_valid) begin
      sa3_to_s1 = true;
   end else begin
      sa3_to_s1 = false;      
   end

   //when are all of the stage 3b inputs are valid and stage3b receivers are ready
   sb3_valid = sb3_insn_valid & sb3_arg1_valid & sb3_arg2_valid & sb3_opcode_valid;
   if (sb3_valid && dmem_ready_in && dmem_valid_in && s3_2_ready && s1_ready) begin
      sb3_to_sb3_2 = true;
   end else begin
      sb3_to_sb3_2 = false;      
   end
   
   to_s1 = sa3_to_s1 & ..._to_s1;
end
//stage 1
//first establish datapath connections
always@(*) begin
   s1_imem_valid = s1_pc_valid;
   s1_imem_addr = s1_pc;
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
   //ready when we don't currently have an insn saved
   //into the register we're saving the output to
   s2_imem_ready = ~s2_insn_valid;
   s2_opcode = opcode(s2_insn);   
   s2_rs1 = rs1(s2_insn);
   s2_rs2 = rs2(s2_insn);
   s2_dest = dest(s2_insn);
   s2_next_stage = (op == BR) ? "sa" : "sb"; //comes from the if
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
      sa3_insn_valid<= s2_insn_valid;
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
   sb3_case_arith = sb3_opcode == ARITH;
   sb3_case_ld = sb3_opcode == LD;
   sb3_case_st = sb3_opcode == ST;

   alu_arg_1_in = sb3_arg1;
   alu_arg_2_in = sb3_arg2;
   alu_op_in = aop(sb3_insn);   
   sb3_result = alu_result_out;

   sb3_dmem_addr = sb3_arg1 + imm(sb3_insn);   
   dmem_addr = sb3_dmem_addr;
   dmem_op = (sb3_case_ld) ? READ : ((sb3_case_st) ? WRITE : <dontcare>);
   dmem_write_data_in = (sb3_case_st) ? sb3_arg2 : <dontcare>;
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
   sb3_2_val = dmem_data;   
   
   rf_write_en = sb3_2_valid && (sb3_2_case_ld || sb3_2_case_arith);
   rf_write_addr = sb3_2_dest;
   rf_write_val = (sb3_case_ld) ? sb3_2_val : sb3_2_res;
end // always@ begin
