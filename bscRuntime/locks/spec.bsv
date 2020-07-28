
typedef Bit#(4) ID;

typedef struct { a pred; a realval; } Entry#(type a) deriving(Bits, Eq);

interface SpeculationTable#(type data);
   method ActionValue#(ID) speculate(data prediction);
   method Action update(data realval, ID entry);
   method Entry#(data) check(ID entry);
   method ActionValue#(Entry#(data)) remove(ID entry);
endinterface

//speculate creates a new used entry
//check reads entry
//remove frees the entry
