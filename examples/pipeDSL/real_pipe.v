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

   wire       s2_valid; 	     
   wire       s2_ready;      

   wire        s2_imem_valid;
   wire        s2_imem_ready;
   wire [31:0] s2_imem_data;
   
   
   Cache imem (.ready_in (s1_imem_ready),
	       .valid_in (s1_imem_valid),
	       .addr_in  (s1_imem_addr),
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
   
   reg [31:0]  s3_pc;
   reg 	       s3_pc_valid;   
   reg [31:0]  s3_arg1;
   reg 	       s3_arg1_valid;   
   reg [31:0]  s3_arg2;
   reg 	       s3_arg2_valid;   
   
   
always@(posedge clk) begin
   if (reset) begin
      s1_pc <= 0;
      s1_pc_valid <= true;
      //reset everything else too
   end
end
   
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
   //when are all of stage 2 inputs valid
   s2_valid = s2_pc_valid && s2_insn_valid;   
   //when are all stage 2 inputs valid
   //and all stage 2 receivers are ready
   if (s2_valid && s3_ready) begin
      s2_to_s3 = true;
   end else begin
      s2_to_s3 = false;
   end
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
   s2_next_stage = (op == BR) ? "s3" : "s4"; //comes from the if
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
   if (s2_to_s3) begin
      s3_pc <= s3_pc;
      s3_pc_valid <= s3_pc_valid;
      s3_arg1 <= rf.read1(s2_rs1);
      s3_arg1_valid <= true;      
      s3_arg2 <= rf.read2(s2_rs2);
      s3_arg2_valid <= true;      
   end
end
   
cpu(pc) {

  insn <- imem.send(pc);
  //
  op = opcode(insn); #selects some bits from the instruction
  rs1 = rs1(insn);
  rs2 = rs2(insn);
  dest = dest(insn);
  arg1 <- rf.read1(rs1);
  arg2 <- rf.read2(rs2);

  if (op == BR) {
     //
     take_br = br_unit(insn, arg1, arg2) # checks the kind of comparison and executes it     
     if (take_br) {
     	return cpu(pc + imm(insn));
     } else {
        return cpu(pc + 4);
     }
  } else {
     spawn cpu(pc + 4);
     match (opcode) {
       case ARITH: {
         //
	 res <- alu(arg1, arg2, aop(insn));
	 //
	 rf.write(dest, res);
       }
       case LD: {
         //
	 val <- dmem.read(arg1 + imm(insn));
	 //
	 rf.write(dest, val);
       }
       case ST: {
         //
	 dmem.write(arg1 + imm(insn), arg2);
       }
     }
  }
}

#actually instantiate and inintialize the cpu
main {
  cpu = new cpu();
  cpu.send(0);
}
