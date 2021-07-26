`ifdef BSV_ASSIGNMENT_DELAY
`else
`define BSV_ASSIGNMENT_DELAY
`endif
`ifdef BSV_RESET_VALUE
`else
 `define BSV_RESET_VALUE 1
`endif

module BypassRF(CLK,
		RST,
		ADDR_IN, NAME_OUT, ALLOC_E, ALLOC_READY, //write res req
		ADDR_1, RNAME_OUT_1, RRESE_1, RRES_READY_1,  //read res 1
		ADDR_2, RNAME_OUT_2, RRESE_2, RRES_READY_2,  //read res 2
		NAME_IN_1, D_IN_1, WE_1,     //write data 1
		NAME_IN_2, D_IN_2, WE_2,     //write data 2		
		NAME_1, D_OUT_1,            //read data 1
		NAME_2, D_OUT_2,            //read data 2
		VALID_NAME_1, VALID_OUT_1,    //check valid data 1
		VALID_NAME_2, VALID_OUT_2,    //check valid data 2
		W_F, W_FE, W_FREADY,          //free write port
		RD_F_1, FE_1,                 //free rd port 1
		RD_F_2, FE_2                  //free rd port 2
		);

   parameter addr_width = 1;
   parameter data_width = 1;
   parameter name_width = 1;
   parameter lo_arch = 0;
   parameter hi_arch = 1;   
   parameter binaryInit = 0;
   parameter file = "";   

   localparam RS1 = 1'd0,
     RS2 = 1'd1;
   
   input CLK;
   input RST;
   
   //read/write reservation
   input [addr_width - 1 : 0] ADDR_IN;
   input 		      ALLOC_E;
   input [addr_width - 1 : 0] ADDR_1;
   input 		      RRESE_1;   
   input [addr_width - 1 : 0] ADDR_2;
   input 		      RRESE_2;
   
   output 		      ALLOC_READY;
   output [name_width - 1 : 0] NAME_OUT;
   output [name_width - 1 : 0] RNAME_OUT_1;
   output 		       RRES_READY_1;   
   output [name_width - 1 : 0] RNAME_OUT_2;
   output 		       RRES_READY_2;   
   
   //data read/write
   input [addr_width - 1 : 0]  WADDR_IN_1;   
   input [name_width - 1 : 0] NAME_IN_1;
   input [data_width - 1 : 0] D_IN_1;
   input 		      WE_1;
   input [addr_width - 1 : 0]  WADDR_IN_2;      
   input [name_width - 1 : 0] NAME_IN_2;
   input [data_width - 1 : 0] D_IN_2;
   input 		      WE_2;   
   input [name_width - 1 : 0] NAME_1;
   input [name_width - 1 : 0] NAME_2;

   output [data_width - 1 : 0] D_OUT_1;
   output [data_width - 1 : 0] D_OUT_2;

   //data busy
   input [name_width - 1 : 0]  VALID_NAME_1;
   input [name_width - 1 : 0]  VALID_NAME_2;
   output 		       VALID_OUT_1;   
   output 		       VALID_OUT_2;
   

   //free write port
   input [name_width - 1 : 0]  W_F;
   input 		       W_FE;
   output 		       W_FREADY;
   
   //free rd ports
   input [name_width - 1 : 0]  RD_F_1;   
   input 		       FE_1;
   input [name_width - 1 : 0]  RD_F_2;   
   input 		       FE_2;

   //phys_regfile
   reg [data_width - 1 : 0]    rf[lo_arch:hi_arch];

   //pending reads
   
   reg [data_width - 1 : 0]    rf1;
   reg [name_width - 1 : 0]    rf1_write;
   reg 			       rf1_valid;
   reg 			       rf1_inUse;
 			       
   
   reg [data_width - 1 : 0]    rf2;
   reg [name_width - 1 : 0]    rf2_write;
   reg 			       rf2_valid;
   reg 			       rf2_inUse;
   
   //write queue
   reg [name_width - 1 : 0]    wQueueOwner;   
   reg [name_width - 1 : 0]    wQueueHead;
   reg [addr_width - 1 : 0]    wQueueAddr[ 0 : name_width - 1];   
   reg 			       wQueueValid[ 0 : name_width - 1];

   //conflicting pending writes
   reg [name_width - 1 : 0]    rf1_conflict, rf2_conflict;
   reg 			       rf1_hasc, rf2_hasc;
 			       
   integer 		       ii = 0;
   integer 		       jj = 0;   
   always@(*)
     begin
	rf1_hasc = 0;
	rf2_hasc = 0;	
	for (ii = 0; ii < name_width & !rf1_hasc; ii = ii + 1)
	  begin
	     if (wQueueValid[wQueueHead - ii] &&
		 wQueueAddr[wQueueHead - ii] == ADDR_1)
	       begin
		  rf1_hasc = 1;
		  rf1_conflict = wQueueHead - ii;		  
	       end	     	     	     
	  end
	for (jj = 0; jj < name_width & !rf2_hasc; jj = jj + 1)
	  begin
	     if (wQueueValid[wQueueHead - jj] &&
		 wQueueAddr[wQueueHead - jj] == ADDR_2)
	       begin
		  rf2_hasc = 1;
		  rf2_conflict = wQueueHead - jj;		  
	       end	     	     
	  end	
     end // always@ begin
   
   //data from actual regfile + forwarding
   wire [ data_width - 1 : 0 ] RF_DATA_1, RF_DATA_2;
   wire 		       RF_FWD11, RF_FWD12, RF_FWD21, RF_RFW22;
   wire 		       stillConflict1,stillConflict2;
   
   assign RF_FWD11 = rf1_hasc && NAME_IN_1 == rf1_conflict && WE_1;
   assign RF_FWD12 = rf2_hasc && NAME_IN_1 == rf2_conflict && WE_1;
   assign RF_FWD21 = rf1_hasc && NAME_IN_2 == rf1_conflict && WE_2;
   assign RF_FWD22 = rf2_hasc && NAME_IN_2 == rf2_conflict && WE_2;   

   assign RF_DATA_1 = (RF_FWD11) ? D_IN_1 : ((RF_FWD21) ? D_IN_2 : rf[ADDR_1]);
   assign RF_DATA_1 = (RF_FWD12) ? D_IN_1 : ((RF_FWD22) ? D_IN_2 : rf[ADDR_2]);   

   assign stillConflict1 = rf1_hasc && !RF_FWD11 && !RF_FWD21;
   assign stillConflict2 = rf2_hasc && !RF_FWD12 && !RF_FWD22;   
   
/*   
   always@(*)
     begin
	for(ii = lo_phys; ii<= hi_phys && !nextNameValid; ii = ii+1)
	  begin
	     if (free[ii])
	       begin
		  nextName = ii;
		  nextNameValid = 1;		  
	       end
	  end
     end */ // UNMATCHED !!
   
   //write res req
   assign ALLOC_READY = !wQueueValid[wQueueHead];   
   assign NAME_OUT = wQueueHead;
   
   //read res req
   assign NAME_OUT_1 = RS1;   
   assign NAME_OUT_2 = RS2;
   assign RRES_READY_1 = !rf1_inUse | (FE_1 && RD_F_1 == RS1) | (FE_2 && RD_F_2 == RS1);   
   assign RRES_READY_2 = !rf2_inUse | (FE_1 && RD_F_1 == RS2) | (FE_2 && RD_F_2 == RS2);
   

   //Forward from either write port to rfx_data;
   wire FWD11, FWD12, FWD21, FWD22;
   assign FWD11 = WE_1 & (NAME_IN_1 == rf1_write);
   assign FWD21 = WE_2 & (NAME_IN_2 == rf2_write);
   assign FWD12 = WE_1 & (NAME_IN_1 == rf1_write);
   assign FWD22 = WE_2 & (NAME_IN_2 == rf2_write);
   
   //Read data from saved reg unless forwarding
   wire [data_width - 1 : 0 ] RF1_OUT, RF2_OUT;
   
   assign RF1_OUT = (rf1_valid) ? rf1 : ((FWD11) ? D_IN_1 : D_IN_2);
   assign RF2_OUT = (rf2_valid) ? rf2 : ((FWD12) ? D_IN_1 : D_IN_2);   

   assign D_OUT_1 = (NAME_1 == RS1) ? RF1_OUT : RF2_OUT;
   assign D_OUT_2 = (NAME_2 == RS1) ? RF1_OUT : RF2_OUT;   
   
   //Readiness of data
   wire RF1_VALID, RF2_VALID;   
   assign RF1_VALID = rf1_valid | FWD11 | FWD21;   
   assign RF2_VALID = rf2_valid | FWD12 | FWD22;

   assign VALID_OUT_1 = (VALID_NAME_1 == RS1) ? RF1_VALID : RF2_VALID;
   assign VALID_OUT_2 = (VALID_NAME_2 == RS1) ? RF1_VALID : RF2_VALID;   

   //free write
   assign W_FREADY = W_F == wQueueOwner;
   
   integer 		       initq;
   integer 		       siminit;  
   //simulation initialization
   initial
     begin
	if (binaryInit)
	  $readmemh(file, rf, lo_arch, hi_arch);
	else
	  begin
	     for (siminit = lo_arch; siminit <= hi_arch; siminit = siminit + 1)
	       rf[siminit] = 0;	     
	  end
     end
   
   //update my stateful elements
   always@(posedge CLK)
     begin
     if (RST == `BSV_RESET_VALUE)
       begin
	  `ifdef DEBUG
	  $display("Reseting");
	  `endif
	  rf1_valid <= 0;
	  rf2_valid <= 0;
	  rf1_inUse <= 0;
	  rf2_inUse <= 0;	  
	  wQueueOwner <= 0;
	  wQueueHead <= 0;
	  for (initq = 0; initq < name_width ; initq = initq + 1)
	    begin
	       wQueueValid[initq] <= 0;
	    end	  
       end
     else
       begin
	  //write reservation
	  if (ALLOC_E && ALLOC_READY)
	    begin
	       wQueueValid[wQueueHead] <= `BSV_ASSIGNMENT_DELAY 1;
	       wQueueAddr[wQueueHead] <= `BSV_ASSIGNMENT_DELAY ADDR_IN;	       
	       wQueueHead <= `BSV_ASSIGNMENT_DELAY wQueueHead + 1;
	    end
	  //write 1
	  if (WE_1)
	    begin
	       rf[wQueueAddr[NAME_IN_1]] <=  `BSV_ASSIGNMENT_DELAY D_IN_1;	       
	    end
	  //write 2
	  if (WE_2)
	    begin
	       rf[wQueueAddr[NAME_IN_2]] <=  `BSV_ASSIGNMENT_DELAY D_IN_2;
	    end
	  //free write queue
	  if (W_FE && W_FREADY)
	    begin
	       wQueueOwner <= `BSV_ASSIGNMENT_DELAY wQueueOwner + 1;
	       wQueueValid[wQueueOwner] <= `BSV_ASSIGNMENT_DELAY 0;	       
	    end
	  //rf1 res regs
	  if (RRES_READY_1)
	    begin
	       rf1_inUse <= `BSV_ASSIGNMENT_DELAY 1;
	       rf1_valid <= `BSV_ASSIGNMENT_DELAY !stillConflict1;	       
	       rf1_write <= `BSV_ASSIGNMENT_DELAY rf1_conflict;
	       rf1 <= `BSV_ASSIGNMENT_DELAY RF_DATA_1;	       
	    end
	  //freeing
	  else if ((FE_1 && RD_F_1 == RS1) || (FE_2 && RD_F_2 == RS1))
	    begin
	       rf1_inUse <= `BSV_ASSIGNMENT_DELAY 0;
	       rf1_valid <= `BSV_ASSIGNMENT_DELAY 0;	       
	    end
	  //forwarding from writes
	  else if (FWD11 || FWD21)
	    begin
	       rf1_valid <= `BSV_ASSIGNMENT_DELAY 1;
	       rf1 <= `BSV_ASSIGNMENT_DELAY (FWD11) ? D_IN_1 : D_IN_2;	       
	    end
	  //rf2 res regs
	  if (RRES_READY_2)
	    begin
	       rf2_inUse <= `BSV_ASSIGNMENT_DELAY 1;
	       rf2_valid <= `BSV_ASSIGNMENT_DELAY !stillConflict2;	       
	       rf2_write <= `BSV_ASSIGNMENT_DELAY rf2_conflict;
	       rf2 <= `BSV_ASSIGNMENT_DELAY RF_DATA_2;	       
	    end
	  //freeing
	  else if ((FE_1 && RD_F_1 == RS2) || (FE_2 && RD_F_2 == RS2))
	    begin
	       rf2_inUse <= `BSV_ASSIGNMENT_DELAY 0;
	       rf2_valid <= `BSV_ASSIGNMENT_DELAY 0;	       
	    end
	  //forwarding from writes
	  else if (FWD12 || FWD22)
	    begin
	       rf2_valid <= `BSV_ASSIGNMENT_DELAY 1;
	       rf2 <= `BSV_ASSIGNMENT_DELAY (FWD12) ? D_IN_1 : D_IN_2;	       
	    end	  
       end // else: !if(RST)
     end // always@ (posedge CLK)

   
endmodule
