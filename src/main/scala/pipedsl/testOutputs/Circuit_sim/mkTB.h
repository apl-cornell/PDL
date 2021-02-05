/*
 * Generated by Bluespec Compiler
 * 
 */

/* Generation options: */
#ifndef __mkTB_h__
#define __mkTB_h__

#include "bluesim_types.h"
#include "bs_module.h"
#include "bluesim_primitives.h"
#include "bs_vcd.h"
#include "mkCircuit.h"


/* Class declaration for the mkTB module */
class MOD_mkTB : public Module {
 
 /* Clock handles */
 private:
  tClock __clk_handle_0;
 
 /* Clock gate handles */
 public:
  tUInt8 *clk_gate[0];
 
 /* Instantiation parameters */
 public:
 
 /* Module state */
 public:
  MOD_mkCircuit INST_m;
  MOD_Reg<tUInt8> INST_reg_unused_0;
  MOD_Reg<tUInt8> INST_started;
 
 /* Constructor */
 public:
  MOD_mkTB(tSimStateHdl simHdl, char const *name, Module *parent);
 
 /* Symbol init methods */
 private:
  void init_symbols_0();
 
 /* Reset signal definitions */
 private:
  tUInt8 PORT_RST_N;
 
 /* Port definitions */
 public:
 
 /* Publicly accessible definitions */
 public:
 
 /* Local definitions */
 private:
 
 /* Rules */
 public:
  void RL_initTB();
  void RL_stopTB();
 
 /* Methods */
 public:
 
 /* Reset routines */
 public:
  void reset_RST_N(tUInt8 ARG_rst_in);
 
 /* Static handles to reset routines */
 public:
 
 /* Pointers to reset fns in parent module for asserting output resets */
 private:
 
 /* Functions for the parent module to register its reset fns */
 public:
 
 /* Functions to set the elaborated clock id */
 public:
  void set_clk_0(char const *s);
 
 /* State dumping routine */
 public:
  void dump_state(unsigned int indent);
 
 /* VCD dumping routines */
 public:
  unsigned int dump_VCD_defs(unsigned int levels);
  void dump_VCD(tVCDDumpType dt, unsigned int levels, MOD_mkTB &backing);
  void vcd_defs(tVCDDumpType dt, MOD_mkTB &backing);
  void vcd_prims(tVCDDumpType dt, MOD_mkTB &backing);
  void vcd_submodules(tVCDDumpType dt, unsigned int levels, MOD_mkTB &backing);
};

#endif /* ifndef __mkTB_h__ */