`ifdef BSV_ASSIGNMENT_DELAY
`else
`define BSV_ASSIGNMENT_DELAY
`endif
`ifdef BSV_RESET_VALUE
`else
 `define BSV_RESET_VALUE 1
`endif

//`define DEBUG 1

module CheckpointBypassRF(CLK,
		RST,
		ADDR_IN, NAME_OUT, ALLOC_E, ALLOC_READY, //write res req
		ADDR_1, RNAME_OUT_1, RRESE_1, RRES_READY_1,  //read res 1
		ADDR_2, RNAME_OUT_2, RRESE_2, RRES_READY_2,  //read res 2
		NAME_IN_1, D_IN_1, WE_1,     //write data 1
		NAME_IN_2, D_IN_2, WE_2,     //write data 2		
		RD_NAME_1, D_OUT_1,            //read data 1
		RD_NAME_2, D_OUT_2,            //read data 2
		VALID_OUT_1,    //check valid data 1		
		VALID_OUT_2,    //check valid data 2
		W_F, WFE, F_READY,                     //free write port
		FE_1,                 //free rd port 1
		FE_2,                  //free rd port 2
		CHK_OUT, CHK_E, //checkpoint req
		ROLLBK_IN, DO_ROLL, DO_REL, ROLLBK_E //rollback req
		);

   parameter addr_width = 1;
   parameter data_width = 1;
   parameter name_width = 1;
   parameter numNames = 2 ** name_width;
   parameter lo_arch = 0;
   parameter hi_arch = 1;   
   parameter binaryInit = 0;
   parameter file = "";   
   
   wire [name_width-1 : 0] RS1,RS2;
   assign RS1 = 0;
   assign RS2 = 1;
   
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
   input [name_width - 1 : 0] NAME_IN_1;
   input [data_width - 1 : 0] D_IN_1;
   input 		      WE_1;
   input [name_width - 1 : 0] NAME_IN_2;
   input [data_width - 1 : 0] D_IN_2;
   input 		      WE_2;   

   input [name_width - 1 : 0] RD_NAME_1;
   input [name_width - 1 : 0] RD_NAME_2;   
   output [data_width - 1 : 0] D_OUT_1;
   output [data_width - 1 : 0] D_OUT_2;

   //data busy
   output 		       VALID_OUT_1;   
   output 		       VALID_OUT_2;

   //free w port
   input [name_width - 1 : 0]  W_F;
   input 		       WFE;   
   output                      F_READY;
 		       
   //free rd ports
   input 		       FE_1;
   input 		       FE_2;

   //checkpoint
   input 		       CHK_E;
   output [name_width - 1 : 0] CHK_OUT;

   //rollback
   input [name_width - 1 : 0]  ROLLBK_IN;
   input 		       DO_ROLL, DO_REL, ROLLBK_E;   
   
   
   //phys_regfile
   reg [data_width - 1 : 0]    rf[lo_arch:hi_arch];

   //pending reads
   reg [name_width - 1 : 0]    rf1_owner;   
   
   reg [data_width - 1 : 0]    rf1;
   reg [name_width - 1 : 0]    rf1_write;
   reg 			       rf1_valid;
   reg 			       rf1_inUse;
 			       

   reg [name_width - 1 : 0]    rf2_owner;      
   reg [data_width - 1 : 0]    rf2;
   reg [name_width - 1 : 0]    rf2_write;
   reg 			       rf2_valid;
   reg 			       rf2_inUse;
   
   //write queue
   reg [name_width - 1: 0]    wQueueHead;
   reg [addr_width - 1 : 0]    wQueueAddr[ 0 : numNames - 1];   
   reg 			       wQueueValid[ 0 : numNames - 1];
   reg 			       wQueueWritten[ 0 : numNames - 1];
   reg [data_width - 1 : 0]    wQueueData[ 0 : numNames - 1 ];
   
   reg [name_width - 1 : 0]    wQueueOwner;

   function automatic [0:0] isNewer(input [name_width - 1 : 0] a, b, h);
      reg 		       nohmid;      
      reg 		       hmid;
      reg 		       isOlder;
      begin
	 nohmid = a < b && !(a < h && b >= h);
	 hmid = (b < h) && (a >= h);
	 isOlder = nohmid || hmid;
	 isNewer = !isOlder;
      end
   endfunction // isNewer
   
   
   //find conflicting writes   
   wire [numNames - 1 : 0]   rf1_foundc_bv;   
   wire 				       rf1_foundc;
   wire 				       rf1_foundc_stg [0: numNames]; 				       
   wire [name_width - 1 : 0] 		       rf1_conflict_stg [0: numNames];
   wire [name_width - 1 : 0] 		       rf1_conflict;

   generate genvar k;
      for(k=0; k <numNames; k = k + 1)
	begin
	   assign rf1_foundc_bv[k] = (wQueueAddr[k] == ADDR_1) && wQueueValid[k];
	end
   endgenerate
   //found if found in any entry
   assign rf1_foundc = |rf1_foundc_bv;

   assign rf1_foundc_stg[0] = 0;
   assign rf1_conflict_stg[0] = 0;   

   generate genvar i;
      for(i=0; i < numNames; i = i + 1)
	begin
	   assign rf1_foundc_stg[i+1] = (rf1_foundc_bv[i]) ? 1 : rf1_foundc_stg[i];
	   assign rf1_conflict_stg[i+1] = (rf1_foundc_bv[i] && (isNewer(i, rf1_conflict_stg[i], wQueueHead) || !rf1_foundc_stg[i])) ?
					   i : rf1_conflict_stg[i];
	end
   endgenerate

   assign rf1_conflict = rf1_conflict_stg[numNames];

   wire [numNames - 1 : 0]   rf2_foundc_bv;   
   wire 				       rf2_foundc;
   wire 				       rf2_foundc_stg [0: numNames]; 				       
   wire [name_width - 1 : 0] 		       rf2_conflict_stg [0: numNames];
   wire [name_width - 1 : 0] 		       rf2_conflict;

   generate genvar j;
      for(j=0; j <numNames; j = j + 1)
	begin
	   assign rf2_foundc_bv[j] = (wQueueAddr[j] == ADDR_2) && wQueueValid[j];	   
	end
   endgenerate
   //found if found in any entry
   assign rf2_foundc = |rf2_foundc_bv;

   assign rf2_foundc_stg[0] = 0;
   assign rf2_conflict_stg[0] = 0;   

   generate genvar l;
      for(l=0; l < numNames; l = l + 1)
	begin
	   assign rf2_foundc_stg[l+1] = (rf2_foundc_bv[l]) ? 1 : rf2_foundc_stg[l];
	   assign rf2_conflict_stg[l+1] = (rf2_foundc_bv[l] && (isNewer(l, rf2_conflict_stg[l], wQueueHead) || !rf2_foundc_stg[l])) ?
					   l : rf2_conflict_stg[l];
	end
   endgenerate

   assign rf2_conflict = rf2_conflict_stg[numNames];



   wire rf1_hasc,rf2_hasc;
   assign rf1_hasc = rf1_foundc && !wQueueWritten[rf1_conflict];
   assign rf2_hasc = rf2_foundc && !wQueueWritten[rf2_conflict];
   
   //data from actual regfile + forwarding
   wire [ data_width - 1 : 0 ] RF_DATA_1, RF_DATA_2;
   wire 		       RF_FWD11, RF_FWD12, RF_FWD21, RF_FWD22;   
   wire 		       stillConflict1, stillConflict2;
   
   assign RF_FWD11 = WE_1 && NAME_IN_1 == rf1_conflict && rf1_hasc;   
   assign RF_FWD12 = WE_1 && NAME_IN_1 == rf2_conflict && rf2_hasc;   
   assign RF_FWD21 = WE_2 && NAME_IN_2 == rf1_conflict && rf1_hasc;   
   assign RF_FWD22 = WE_2 && NAME_IN_2 == rf2_conflict && rf2_hasc;

   //forward from concurrent write combinationally or from the write queue,
   //read from RF if no conflict
   assign RF_DATA_1 = (RF_FWD11) ? D_IN_1 :
		      ((RF_FWD21) ? D_IN_2 :
		      (rf1_foundc ? wQueueData[rf1_conflict] :
		       rf[ADDR_1]));
      
   assign RF_DATA_2 = (RF_FWD12) ? D_IN_1 :
		      ((RF_FWD22) ? D_IN_2 :
		       (rf2_foundc ? wQueueData[rf2_conflict] :
			rf[ADDR_2]));   
   
   assign stillConflict1 = rf1_hasc && (!RF_FWD11) & (!RF_FWD21);
   assign stillConflict2 = rf2_hasc && (!RF_FWD12) & (!RF_FWD22);   
   
   //write res req
   assign ALLOC_READY = !wQueueValid[wQueueHead];   
   assign NAME_OUT = wQueueHead;

   //write free rdy
   assign F_READY = wQueueOwner == W_F;
   
   
   //read res req
   assign RNAME_OUT_1 = RS1;   
   assign RNAME_OUT_2 = RS2;
   assign RRES_READY_1 = !rf1_inUse | FE_1;
   assign RRES_READY_2 = !rf2_inUse | FE_2;   
   
   //Forward from either write port to rfx_data;
   wire FWD11, FWD12, FWD21, FWD22;
   assign FWD11 = WE_1 & (NAME_IN_1 == rf1_write);   
   assign FWD21 = WE_2 & (NAME_IN_2 == rf1_write);
   assign FWD12 = WE_1 & (NAME_IN_1 == rf2_write);   
   assign FWD22 = WE_2 & (NAME_IN_2 == rf2_write);   
   
   //Read data from saved reg unless forwarding
   reg [data_width - 1 : 0 ] RF1_OUT, RF2_OUT;
   
//   assign RF1_OUT = (rf1_valid) ? rf1 : ((FWD11) ? D_IN_1 : D_IN_2);
//   assign RF2_OUT = (rf2_valid) ? rf2 : ((FWD12) ? D_IN_1 : D_IN_2);   

   always@(*)
     begin
	case ({rf1_valid, FWD11, FWD21})
	  3'b010 : RF1_OUT <= D_IN_1;
	  3'b001 : RF1_OUT <= D_IN_2;
	  default : RF1_OUT <= rf1;	  
	endcase // case ({rf1_valid, FWD11, FWD21})
	case ({rf2_valid, FWD12, FWD22})
	  3'b010 : RF2_OUT <= D_IN_1;
	  3'b001 : RF2_OUT <= D_IN_2;
	  default : RF2_OUT <= rf2;	  	  
	endcase // case ({rf2_valid, FWD12, FWD22})	
     end // always@ (*)   
   
   assign D_OUT_1 =  RF1_OUT;   
   assign D_OUT_2 =  RF2_OUT;
   
   //Readiness of data
   wire RF1_VALID, RF2_VALID;   
   assign RF1_VALID = rf1_inUse & (rf1_valid | FWD11 | FWD21);   
   assign RF2_VALID = rf2_inUse & (rf2_valid | FWD12 | FWD22);   

   assign VALID_OUT_1 = RF1_VALID;   
   assign VALID_OUT_2 = RF2_VALID;


   //Checkpoint info:
   //next wqueuehead is the checkpoint id (i.e., the thing we reset the queue head to)
   assign CHK_OUT = (ALLOC_E && ALLOC_READY) ? wQueueHead + 1 : wQueueHead;   

   
   integer initq;   
   integer 		       siminit;
   integer 		       rbi;
   
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
	  rf1_valid <= `BSV_ASSIGNMENT_DELAY 0;
	  rf2_valid <= `BSV_ASSIGNMENT_DELAY 0;
	  rf1_inUse <= `BSV_ASSIGNMENT_DELAY 0;
	  rf2_inUse <= `BSV_ASSIGNMENT_DELAY 0;	  
	  wQueueHead <= `BSV_ASSIGNMENT_DELAY 0;
	  wQueueOwner <= `BSV_ASSIGNMENT_DELAY 0;	  
	  for (initq = 0; initq < numNames ; initq = initq + 1)
	    begin
	       wQueueValid[initq] <= `BSV_ASSIGNMENT_DELAY 0;
	       wQueueWritten[initq] <= `BSV_ASSIGNMENT_DELAY 0;	       
	    end	  
       end
     else
       begin
	  //do rollback
	  if (ROLLBK_E && DO_ROLL)
	    begin
	       //reset head of write queue
	       wQueueHead <= `BSV_ASSIGNMENT_DELAY ROLLBK_IN;
	       //reset validity of ALL queue entries NEWER than current head but older than ROLLBK_IN
	       for (rbi = 0; rbi < numNames; rbi = rbi + 1)
		 begin
		    if (!isNewer(rbi, ROLLBK_IN, wQueueHead))
		      begin
			 wQueueValid[rbi] <= `BSV_ASSIGNMENT_DELAY 0;
			 wQueueWritten[rbi] <= `BSV_ASSIGNMENT_DELAY 0;
		      end			 
		 end
	       //if the person making the checkpoint is not the owner of rd slots, reset their status
	       if (rf1_owner != ROLLBK_IN) rf1_inUse <= `BSV_ASSIGNMENT_DELAY 0;
	       if (rf2_owner != ROLLBK_IN) rf2_inUse <= `BSV_ASSIGNMENT_DELAY 0;	       	       
	    end	 
	  //write reservation
	  if (ALLOC_E && ALLOC_READY)
	    begin
	       wQueueWritten[wQueueHead] <= `BSV_ASSIGNMENT_DELAY 0;	       
	       wQueueValid[wQueueHead] <= `BSV_ASSIGNMENT_DELAY 1;
	       wQueueAddr[wQueueHead] <= `BSV_ASSIGNMENT_DELAY ADDR_IN;	       
	       wQueueHead <= `BSV_ASSIGNMENT_DELAY wQueueHead + 1;
	  `ifdef DEBUG	       
	       $display("ALLOC W entry %d for addr %d", wQueueHead, ADDR_IN);
	  `endif
	    end
	  //write 1 to write queue
	  if (WE_1)
	    begin
	       wQueueWritten[NAME_IN_1] <= `BSV_ASSIGNMENT_DELAY 1;
	       wQueueData[NAME_IN_1] <= `BSV_ASSIGNMENT_DELAY D_IN_1;	       
	    end
	  //write 2 to write queue
	  if (WE_2)
	    begin
	       wQueueWritten[NAME_IN_2] <= `BSV_ASSIGNMENT_DELAY 1;
	       wQueueData[NAME_IN_2] <= `BSV_ASSIGNMENT_DELAY D_IN_2;	       	       
	    end
	  //free writes and commit to rf
	  if (WFE & F_READY)
	    begin
	       wQueueValid[W_F] <= `BSV_ASSIGNMENT_DELAY 0;
	       wQueueWritten[W_F] <= `BSV_ASSIGNMENT_DELAY 0;	       
	       wQueueOwner <= `BSV_ASSIGNMENT_DELAY wQueueOwner + 1;
	       rf[wQueueAddr[W_F]] <= `BSV_ASSIGNMENT_DELAY wQueueData[W_F];	       
	  `ifdef DEBUG
	       $display("Freeing entry %d for addr %d", W_F, wQueueAddr[W_F]);	       
	  `endif
	    end
	  //rf1 res regs
	  if (RRES_READY_1 & RRESE_1)
	    begin
	       rf1_owner <= `BSV_ASSIGNMENT_DELAY CHK_OUT;	       
	       rf1_inUse <= `BSV_ASSIGNMENT_DELAY 1;
	       rf1_write <= `BSV_ASSIGNMENT_DELAY rf1_conflict;	       
	       rf1 <= `BSV_ASSIGNMENT_DELAY RF_DATA_1;
	       rf1_valid <= `BSV_ASSIGNMENT_DELAY !stillConflict1;
	  `ifdef DEBUG
	       $display("RF1 Res found conflict %b, with entry %d for addr %d", rf1_foundc, rf1_conflict, ADDR_1);
	  `endif
	    end
	  //freeing
	  else if (FE_1)
	    begin	       
	       rf1_inUse <= `BSV_ASSIGNMENT_DELAY 0;
	       rf1_valid <= `BSV_ASSIGNMENT_DELAY 0;
	    end
	  //forwarding from writes
	  else if ((!rf1_valid) && (FWD11 || FWD21))
	    begin
	       rf1_valid <= `BSV_ASSIGNMENT_DELAY 1;
	       rf1 <= `BSV_ASSIGNMENT_DELAY (FWD11) ? D_IN_1 : D_IN_2;
	    end
	  //rf2 res regs
	  if (RRES_READY_2 & RRESE_2)
	    begin
	       rf2_owner <= `BSV_ASSIGNMENT_DELAY CHK_OUT;	       
	       rf2_inUse <= `BSV_ASSIGNMENT_DELAY 1;
	       rf2_write <= `BSV_ASSIGNMENT_DELAY rf2_conflict;
	       rf2_valid <= `BSV_ASSIGNMENT_DELAY !stillConflict2;	       
	       rf2 <= `BSV_ASSIGNMENT_DELAY RF_DATA_2;
	  `ifdef DEBUG
	       $display("RF2 Res found conflict %b, with entry %d for addr %d", rf2_foundc, rf2_conflict, ADDR_2);
	  `endif
	    end
	  //freeing
	  else if (FE_2)
	    begin
	       rf2_inUse <= `BSV_ASSIGNMENT_DELAY 0;
	       rf2_valid <= `BSV_ASSIGNMENT_DELAY 0;
	    end
	  //forwarding from writes
	  else if ((!rf2_valid) && (FWD12 || FWD22))
	    begin
	       rf2_valid <= `BSV_ASSIGNMENT_DELAY 1;
	       rf2 <= `BSV_ASSIGNMENT_DELAY (FWD12) ? D_IN_1 : D_IN_2;
	    end	  
       end // else: !if(RST)
     end // always@ (posedge CLK)

   
endmodule
