`ifdef BSV_ASSIGNMENT_DELAY
`else
`define BSV_ASSIGNMENT_DELAY
`endif

module RenameRF(CLK,
		ADDR_IN, NAME_OUT, ALLOC_E, ALLOC_READY, //rename req
		ADDR_1, NAME_OUT_1,         //read name 1
		ADDR_2, NAME_OUT_2,         //read name 2
		NAME, D_IN, WE,             //write data
		NAME_1, D_OUT_1,            //read data 1
		NAME_2, D_OUT_2,            //read data 2
		BUSY_NAME_1, BUSY_OUT_1,    //check valid data 1
		BUSY_NAME_2, BUSY_OUT_2,    //check valid data 2
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
   input [name_width - 1 : 0] NAME;
   input [data_width - 1 : 0] D_IN;
   input 		      WE;
   input [name_width - 1 : 0] NAME_1;
   input [name_width - 1 : 0] NAME_2;

   output [data_width - 1 : 0] D_OUT_1;
   output [data_width - 1 : 0] D_OUT_2;

   //data busy
   input [name_width - 1 : 0]  BUSY_NAME_1;
   input [name_width - 1 : 0]  BUSY_NAME_2;
   output 		       BUSY_OUT_1;   
   output 		       BUSY_OUT_2;
   
   
   //free name
   input [name_width - 1 : 0]  NAME_F;
   input 		       FE;

   //arch reg file (name file)
   reg [name_width - 1 : 0]    names[lo_arch:hi_arch];
   //phys_regfile
   reg [data_width - 1 : 0]    phys[lo_phys:hi_phys];
   //busy file (bit vector, not mem)
   reg [hi_phys:lo_phys]       busy;
   //free list
   reg [hi_phys:lo_phys]       free;
   //old names
   reg [name_width - 1 : 0]    old[lo_phys:hi_phys];

   //TODO get this by searching freelist (i.e., min index where free[idx] = 1)
   wire [name_width - 1 : 0]   nextName;
   wire 		       nextNameValid;
   assign nextNameValid = |free; //valid if any index is 1

   //Read/alloc names
   assign ALLOC_READY = nextNameValid;
   assign NAME_OUT = nextName;
   assign NAME_OUT_1 = names[ADDR_1];
   assign NAME_OUT_2 = names[ADDR_2];

   //Read data
   assign D_OUT_1 = phys[NAME_1];
   assign D_OUT_2 = phys[NAME_2];
   //Readiness of data
   assign BUSY_OUT_1 = busy[BUSY_NAME_1];
   assign BUSY_OUT_2 = busy[BUSY_NAME_2];   
   
   //For freeing old name
   wire [name_width - 1 : 0]   oldName;
   assign oldName = old[NAME_F];
   
   
   //update my stateful elements
   always@(posedge CLK)
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
	     phys[NAME] <= `BSV_ASSIGNMENT_DELAY D_IN;
	     busy[NAME] <= `BSV_ASSIGNMENT_DELAY 0;	     
	  end
	if (FE)
	  begin
	     free[oldName] <= `BSV_ASSIGNMENT_DELAY 1;	     
	  end
     end
   
endmodule
