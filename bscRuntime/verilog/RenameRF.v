`ifdef BSV_ASSIGNMENT_DELAY
`else
`define BSV_ASSIGNMENT_DELAY
`endif

module RenameRF(CLK,
		RST,
		ADDR_IN, NAME_OUT, ALLOC_E, ALLOC_READY, //rename req
		ADDR_1, NAME_OUT_1,         //read name 1
		ADDR_2, NAME_OUT_2,         //read name 2
		NAME_IN, D_IN, WE,             //write data
		NAME_1, D_OUT_1,            //read data 1
		NAME_2, D_OUT_2,            //read data 2
		VALID_NAME_1, VALID_OUT_1,    //check valid data 1
		VALID_NAME_2, VALID_OUT_2,    //check valid data 2
		NAME_F, FE                  //free name
		);

   parameter addr_width = 1;
   parameter data_width = 1;
   parameter name_width = 1;
   parameter lo_arch = 0;
   parameter hi_arch = 1;   
   parameter lo_phys = 0;
   parameter hi_phys = 1;
   
   input CLK;
   input RST;
   
   //name read/write
   input [addr_width - 1 : 0] ADDR_IN;
   input 		      ALLOC_E;
   input [addr_width - 1 : 0] ADDR_1;
   input [addr_width - 1 : 0] ADDR_2;

   output 		      ALLOC_READY;
   output [name_width - 1 : 0] NAME_OUT;
   output [name_width - 1 : 0] NAME_OUT_1;
   output [name_width - 1 : 0] NAME_OUT_2;
   
   //data read/write
   input [name_width - 1 : 0] NAME_IN;
   input [data_width - 1 : 0] D_IN;
   input 		      WE;
   input [name_width - 1 : 0] NAME_1;
   input [name_width - 1 : 0] NAME_2;

   output [data_width - 1 : 0] D_OUT_1;
   output [data_width - 1 : 0] D_OUT_2;

   //data busy
   input [name_width - 1 : 0]  VALID_NAME_1;
   input [name_width - 1 : 0]  VALID_NAME_2;
   output 		       VALID_OUT_1;   
   output 		       VALID_OUT_2;
   
   
   //free name
   input [name_width - 1 : 0]  NAME_F;
   input 		       FE;

   //arch reg file (name file)
   reg [name_width - 1 : 0]    names[lo_arch:hi_arch];
   //phys_regfile
   reg [data_width - 1 : 0]    phys[lo_phys:hi_phys];
   //busy file (bit vector, not mem)
   reg [hi_phys : lo_phys]       busy;
   //free list
   reg [hi_phys : lo_phys]       free;
   //old names
   reg [name_width - 1 : 0]    old[lo_phys:hi_phys];

   //priority encoder to pick the next free name from the free list
   reg [name_width - 1 : 0]   nextName;
   reg 			      nextNameValid;
   integer 		       ii = 0;
   
   always@(*)
     begin
	nextName = 0;
	nextNameValid = 0;	
	for(ii = lo_phys; ii<= hi_phys && !nextNameValid; ii = ii+1)
	  begin
	     if (free[ii])
	       begin
		  nextName = ii;
		  nextNameValid = 1;		  
	       end
	  end
`ifdef DEBUG
	$display("nextFreeName %d", nextName);
	$display("freeNameValid %d", nextNameValid);
`endif
     end
   //Read/alloc names
   assign ALLOC_READY = nextNameValid;
   assign NAME_OUT = nextName;
   assign NAME_OUT_1 = names[ADDR_1];
   assign NAME_OUT_2 = names[ADDR_2];

   //Read data
   assign D_OUT_1 = phys[NAME_1];
   assign D_OUT_2 = phys[NAME_2];
   //Readiness of data
   assign VALID_OUT_1 = !busy[VALID_NAME_1];
   assign VALID_OUT_2 = !busy[VALID_NAME_2];   
   
   //For freeing old name
   wire [name_width - 1 : 0]   oldName;
   assign oldName = old[NAME_F];
   
   integer 		       initi;
   integer 		       initf;

   `ifdef DEBUG
   always@(posedge CLK)
     begin
	$display("FreeList %b", free);
     end
   `endif
   //update my stateful elements
   always@(posedge CLK)
     begin
     if (RST == `BSV_RESET_VALUE)
       begin
	  `ifdef DEBUG
	  $display("Reseting");
	  `endif
	  for (initi = lo_arch; initi <= hi_arch; initi = initi + 1)
	    begin
	       names[initi] <= initi;
	       free[initi] <= 0;
	       busy[initi] <= 0;	       
	    end
	  for (initf = hi_arch + 1; initf <= hi_phys; initf = initf + 1)
	    begin
	       free[initf] <= 1;
	    end
       end
     else
       begin
	  if (ALLOC_E && nextNameValid)
	    begin
	       busy[nextName] <= `BSV_ASSIGNMENT_DELAY 1;
	       free[nextName] <= `BSV_ASSIGNMENT_DELAY 0;
	       old[nextName] <= `BSV_ASSIGNMENT_DELAY names[ADDR_IN];
	       names[ADDR_IN] <= `BSV_ASSIGNMENT_DELAY nextName;	     
	    end
	  if (WE)
	    begin
	       phys[NAME_IN] <= `BSV_ASSIGNMENT_DELAY D_IN;
	       busy[NAME_IN] <= `BSV_ASSIGNMENT_DELAY 0;	     
	    end
	  if (FE)
	    begin
	       free[oldName] <= `BSV_ASSIGNMENT_DELAY 1;	     
	    end
       end // else: !if(RST)
     end // always@ (posedge CLK)

   
endmodule
