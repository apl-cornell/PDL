`ifdef BSV_ASSIGNMENT_DELAY
`else
`define BSV_ASSIGNMENT_DELAY
`endif

`ifdef BSV_RESET_VALUE
`else
 `define BSV_RESET_VALUE 1
`endif

module CheckpointRenameRF(CLK,
		RST,
		ADDR_IN, NAME_OUT, ALLOC_E, ALLOC_READY, //rename req
		ADDR_1, NAME_OUT_1,                      //read name 1
		ADDR_2, NAME_OUT_2,                      //read name 2
		NAME_IN_1, D_IN_1, WE_1,                 //write data 1
		NAME_IN_2, D_IN_2, WE_2,                 //write data 2		
		NAME_1, D_OUT_1,                         //read data 1
		NAME_2, D_OUT_2,                         //read data 2
		VALID_NAME_1, VALID_OUT_1,               //check valid data 1
		VALID_NAME_2, VALID_OUT_2,               //check valid data 2
		NAME_F, FE,                              //free name
		CHK_OUT, CHK_E, CHK_READY,               //checkpoint req
		ROLLBK_IN, DO_ROLL, DO_REL, ROLLBK_E        //rollback req
		);

   parameter addr_width = 1;
   parameter data_width = 1;
   parameter name_width = 1;
   parameter replica_width = 1;   
   
   parameter lo_arch = 0;
   parameter hi_arch = 1;   
   parameter lo_phys = 0;
   parameter hi_phys = 1;
   parameter num_replicas = 2;   
   parameter binaryInit = 0;
   parameter file = "";   
   
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
   input [name_width - 1 : 0] NAME_IN_1;
   input [data_width - 1 : 0] D_IN_1;
   input 		      WE_1;
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
      
   //free name
   input [name_width - 1 : 0]  NAME_F;
   input 		       FE;


   //checkpoint
   input 		       CHK_E;
   output [replica_width - 1 : 0] CHK_OUT;
   output 			  CHK_READY;

   //rollback
   input [replica_width - 1 : 0]  ROLLBK_IN;
   input 			  ROLLBK_E, DO_ROLL, DO_REL;
   

   parameter copy_width = name_width * (hi_arch-lo_arch+1);      
   //arch reg file (name file)
   reg [copy_width - 1 : 0] 	  names;   
   //checkpoint mapping files
   reg [copy_width - 1 : 0]    name_copies[0:num_replicas - 1];
   reg [copy_width - 1 : 0]    currentNameSnapshot;   
      

   //looks up a name given a copy
   function automatic [ name_width - 1 : 0 ] getName(input [copy_width - 1 : 0] copy, input [addr_width - 1 : 0] addr);
      reg [copy_width - 1 : 0] shift;      
      begin
	 shift = copy >> (2 ** addr);	 
	 getName = shift[name_width - 1 : 0];	 
      end
   endfunction // getName
   
   
   //phys_regfile
   reg [data_width - 1 : 0]    phys[lo_phys:hi_phys];
   //busy file (bit vector, not mem)
   reg [hi_phys : lo_phys]       busy;
   //free list
   reg [hi_phys : lo_phys]       free;
   //checkpoint free list
   reg [hi_phys : lo_phys] 	 free_copies[0: num_replicas - 1];   
   reg [hi_phys : lo_phys] 	 currentFreeSnapshot;
   
 	 
   //old names
   reg [name_width - 1 : 0]    old[lo_phys:hi_phys];

   //replica free list
   reg [num_replicas - 1 : 0] freeReplicas;
   reg [num_replicas - 1 : 0] nextFreeReplicas;   
   
   reg [replica_width - 1 : 0] nextReplica;   
   reg 			      nextReplicaValid;
   integer 		      jj = 0;
   
   

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
     end // always@ (*)

   always@(*)
   begin
      nextReplica = 0;
      nextReplicaValid = 0;
      for (jj = 0; jj < num_replicas && !nextReplicaValid; jj = jj+1)
	begin
	   if (freeReplicas[jj])
	     begin
		nextReplica = jj;
		nextReplicaValid = 1;		
	     end
	end
   end

   always@(*)
     begin
	if (ROLLBK_E)
	  begin
	     case ({DO_REL, DO_ROLL})
	       2'b00 : nextFreeReplicas = freeReplicas;	       
	       2'b01 : nextFreeReplicas =  ~(1 << ROLLBK_IN); // free everything but ROLLBK_IN
	       2'b10 : nextFreeReplicas =  freeReplicas | (1 << ROLLBK_IN);  //only free ROLLBK_IN
	       2'b11 : nextFreeReplicas =  ~0; // free everything
	     endcase // case ({DO_REL, DO_ROLL})	     
	  end
	else
	  begin
	     nextFreeReplicas = freeReplicas;	     
	  end // else: !if(ROLLBK_E)
	if (CHK_E & nextReplicaValid)
	  begin
	     nextFreeReplicas[nextReplica] = 0;	     
	  end	
     end // always@ begin
   
   assign CHK_READY = nextReplicaValid;
   assign CHK_OUT = nextReplica;
   
     
   //Read/alloc names
   assign ALLOC_READY = nextNameValid;
   assign NAME_OUT = nextName;
   assign NAME_OUT_1 = names[(ADDR_1 * name_width) +: name_width];   
   assign NAME_OUT_2 = names[(ADDR_2 * name_width) +: name_width];   

   //Read data
   assign D_OUT_1 = phys[NAME_1];
   assign D_OUT_2 = phys[NAME_2];
   //Readiness of data
   assign VALID_OUT_1 = !busy[VALID_NAME_1];
   assign VALID_OUT_2 = !busy[VALID_NAME_2];   
   
   //For freeing old name
   wire [name_width - 1 : 0]   oldName;
   assign oldName = old[NAME_F];

   //Snapshots for making checkpoints:
   always@(*)
     begin
	currentNameSnapshot = names;	
	currentFreeSnapshot = free;
	if (ALLOC_E && nextNameValid) //add
	  begin
	     currentNameSnapshot[(ADDR_IN * name_width) +: name_width] = nextName;
	     currentFreeSnapshot[nextName] = 0;
	     //no alloc and RB at same time
	  end	
	if (FE)
	  begin
	     currentFreeSnapshot[oldName] = 1;
	  end
     end // always@ begin   

   integer 		       initi;
   integer 		       initf;
   integer 		       initrep;   
   integer 		       siminit;
   
   //simulation initialization
   initial
     begin
	if (binaryInit)
	  $readmemh(file, phys, lo_arch, hi_arch);
	else
	  begin
	     for (siminit = lo_arch; siminit <= hi_arch; siminit = siminit + 1)
	       phys[siminit] = 0;	     
	  end
     end
   
`ifdef DEBUG
   integer debugi;   
   always@(posedge CLK)
     begin
	$display("FreeList %b", free);
	for (debugi = lo_arch; debugi <= hi_arch; debugi = debugi + 1)
	  begin
	     $display("%d -> %d", debugi, names[(debugi * name_width) +: name_width]);	     
	  end
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
	       names[(initi * name_width) +: name_width] <= initi;
	       free[initi] <= 0;
	       busy[initi] <= 0;	       
	    end
	  for (initf = hi_arch + 1; initf <= hi_phys; initf = initf + 1)
	    begin
	       free[initf] <= 1;
	    end
	  for (initrep = 0; initrep < num_replicas; initrep = initrep + 1)
	    begin
	       freeReplicas[initrep] <= 1;	       
	    end
       end
     else
       begin
	  //ROLLBK_E & DO_ROLL => !CHK_E & !ALLOC_E
	  if (ROLLBK_E | (CHK_E & nextReplicaValid))
	    begin
	       freeReplicas <= `BSV_ASSIGNMENT_DELAY nextFreeReplicas;
	    end
	  if (CHK_E & nextReplicaValid)
	    begin
	       //copy current state to replica
	       name_copies[nextReplica] <= `BSV_ASSIGNMENT_DELAY currentNameSnapshot;
	       free_copies[nextReplica] <= `BSV_ASSIGNMENT_DELAY currentFreeSnapshot;	       
	    end
	  if (ALLOC_E && nextNameValid) //cannot run with ROLLBK_E & DO_ROLL
	    begin
	       busy[nextName] <= `BSV_ASSIGNMENT_DELAY 1;
	       free[nextName] <= `BSV_ASSIGNMENT_DELAY 0;
	       old[nextName] <= `BSV_ASSIGNMENT_DELAY names[(ADDR_IN * name_width) +: name_width];
	       names[(ADDR_IN * name_width) +: name_width] <= `BSV_ASSIGNMENT_DELAY nextName;	     
	    end
	  if (WE_1)
	    begin
	       phys[NAME_IN_1] <= `BSV_ASSIGNMENT_DELAY D_IN_1;
	       busy[NAME_IN_1] <= `BSV_ASSIGNMENT_DELAY 0;	     
	    end
	  if (WE_2)
	    begin
	       phys[NAME_IN_2] <= `BSV_ASSIGNMENT_DELAY D_IN_2;
	       busy[NAME_IN_2] <= `BSV_ASSIGNMENT_DELAY 0;	     
	    end	  
	  if (ROLLBK_E && DO_ROLL)
	    begin
	       names <= `BSV_ASSIGNMENT_DELAY name_copies[ROLLBK_IN];	       
	       free <= `BSV_ASSIGNMENT_DELAY free_copies[ROLLBK_IN] | (FE << oldName) | free;	       	      
	    end
	  else if (FE)
	    begin
	       free[oldName] <= `BSV_ASSIGNMENT_DELAY 1;	       
	    end
       end // else: !if(RST)
     end // always@ (posedge CLK)

   
endmodule
