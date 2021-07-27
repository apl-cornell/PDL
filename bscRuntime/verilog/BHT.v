`ifdef BSV_ASSIGNMENT_DELAY
`else
`define BSV_ASSIGNMENT_DELAY
`endif

module BHT(CLK,
	   RST,
	   PC_IN_PRED, SKIP_OFF_IN, TAKE_OFF_IN, TAKE_OUT, //predict req
	   PC_IN_RES, TAKE_IN, WE //update history
	   );


   parameter num_entries = 4;
   parameter addr_width = 32;

   localparam entry_width = $clog2(num_entries);
   
   localparam SKIP_S = 2'd0,
     SKIP_W = 2'd1,
     TAKE_W = 2'd2,
     TAKE_S = 2'd3;   

   input CLK;
   input RST;   
   
   //get prediction
   input [addr_width - 1 : 0] PC_IN_PRED;
   input [addr_width - 1 : 0] SKIP_OFF_IN;   
   input [addr_width - 1 : 0] TAKE_OFF_IN;
   output [addr_width - 1 : 0] TAKE_OUT;
   

   //update prediction
   input  [addr_width - 1 : 0] PC_IN_RES;
   input  TAKE_IN;
   input  WE;


   
   //history file
   reg [1:0]  hist[0:num_entries-1];

   //Comb logic
   reg [1:0]  NEXT_PRED;
   reg [addr_width-1: 0] REQ_PC;   
   wire [entry_width-1: 0] UPD_ENT;
   wire [entry_width-1: 0] REQ_ENT;   
   wire [1:0] CUR_PRED;
   wire [1:0] REQ_PRED;   
   //inititalization vars
   integer    initi;
   
   assign REQ_ENT = PC_IN_PRED[entry_width-1:0];
   assign REQ_PRED = hist[REQ_ENT];
   assign TAKE_OUT = REQ_PC;

   assign UPD_ENT = PC_IN_RES[entry_width-1:0];
   assign CUR_PRED = hist[UPD_ENT];   


   //request logic
   always@(*)
     begin
	case (REQ_PRED)
	  SKIP_S: begin
	     REQ_PC = PC_IN_PRED + SKIP_OFF_IN;
	  end
	  SKIP_W: begin
	     REQ_PC = PC_IN_PRED + SKIP_OFF_IN;	     
	  end
	  TAKE_W: begin
	     REQ_PC = PC_IN_PRED + TAKE_OFF_IN;
	  end
	  TAKE_S: begin
	     REQ_PC = PC_IN_PRED + TAKE_OFF_IN;	     
	  end
	endcase // case (REQ_PRED)	
     end // always@ begin
   

   //update pred
   always@(*)     
     begin
	case (CUR_PRED)
	  SKIP_S: begin
	     NEXT_PRED = (TAKE_IN) ? SKIP_W : SKIP_S;	     
	  end
	  SKIP_W: begin
	     NEXT_PRED = (TAKE_IN) ? TAKE_W : SKIP_S;	     
	  end
	  TAKE_W: begin
	     NEXT_PRED = (TAKE_IN) ? TAKE_S : SKIP_W;	     
	  end	 
	  TAKE_S: begin
	     NEXT_PRED = (TAKE_IN) ? TAKE_S : TAKE_W;
	  end
	endcase // case (CUR_PRED)	
     end // always@ (*)

   //reset logic and update prediction
   always@(posedge CLK)
     begin
	if (RST == `BSV_RESET_VALUE)
	  begin
	     for (initi = 0; initi <= num_entries - 1; initi = initi + 1)
	       begin
		  hist[initi] <= TAKE_W; //init to weak pred taken
	       end
	  end
	else
	  begin
	     if (WE)
	       begin
		  hist[UPD_ENT] <= `BSV_ASSIGNMENT_DELAY NEXT_PRED;
	       end
	  end
     end // always@ (posedge CLK)
   
endmodule
