module cache (ready_in, valid_in, addr_in, op_in,
	      write_data_in, ready_out, valid_out, data_out);

   input valid_in, ready_out, op_in;
   input [31:0] addr_in, write_data_in;
   output 	ready_in, valid_out;
   output [31:0] data_out;
   
endmodule
   
module regfile (write_en, write_addr, write_val,
		read_1_addr, read_1_en, read_1_out,
		read_2_addr, read_2_en, read_2_out);

   input write_en, read_1_en, read_2_en;
   input [4:0] write_addr, read_1_addr, read_2_addr;
   input [31:0] write_val;
   output [31:0] read_1_out, read_2_out;
   
endmodule   


module alu (arg_1, arg_2, alu_op, result);

   input [31:0] arg_1, arg_2;
   input [1:0] 	alu_op;
   output [31:0] result;
   
endmodule
