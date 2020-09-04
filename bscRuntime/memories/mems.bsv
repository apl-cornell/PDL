//these are the memory interfaces we suppport
//the first is used for memories that support combinational reads

interface MemCombRead#(type elem, type addr);
   method elem read(addr a);
   method Action write(addr a, elem b);
endinterface

//this one is used for asynchronous reads which involve a request and response
interface AsyncMem#(type elem, type addr);
    method Action readReq(addr a);
    method elem peekRead();
    method Action readResp();
    method Action write(addr a, elem b);
endinterface

