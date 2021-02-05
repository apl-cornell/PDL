/*
 * Generated by Bluespec Compiler
 * 
 */
#include "bluesim_primitives.h"
#include "mkTB.h"


/* Constructor */
MOD_mkTB::MOD_mkTB(tSimStateHdl simHdl, char const *name, Module *parent)
  : Module(simHdl, name, parent),
    __clk_handle_0(BAD_CLOCK_HANDLE),
    INST_m(simHdl, "m", this),
    INST_reg_unused_0(simHdl, "reg_unused_0", this, 3u, (tUInt8)0u, (tUInt8)0u),
    INST_started(simHdl, "started", this, 1u, (tUInt8)0u, (tUInt8)0u),
    PORT_RST_N((tUInt8)1u)
{
  symbol_count = 5u;
  symbols = new tSym[symbol_count];
  init_symbols_0();
}


/* Symbol init fns */

void MOD_mkTB::init_symbols_0()
{
  init_symbol(&symbols[0u], "m", SYM_MODULE, &INST_m);
  init_symbol(&symbols[1u], "RL_initTB", SYM_RULE);
  init_symbol(&symbols[2u], "RL_stopTB", SYM_RULE);
  init_symbol(&symbols[3u], "reg_unused_0", SYM_MODULE, &INST_reg_unused_0);
  init_symbol(&symbols[4u], "started", SYM_MODULE, &INST_started);
}


/* Rule actions */

void MOD_mkTB::RL_initTB()
{
  tUInt8 DEF_m__inthg_req___d5;
  tUInt8 DEF_AVMeth_m__inthg_req;
  DEF_AVMeth_m__inthg_req = INST_m.METH__inthg_req(0u);
  DEF_m__inthg_req___d5 = DEF_AVMeth_m__inthg_req;
  INST_reg_unused_0.METH_write(DEF_m__inthg_req___d5);
  INST_started.METH_write((tUInt8)1u);
}

void MOD_mkTB::RL_stopTB()
{
  if (!(PORT_RST_N == (tUInt8)0u))
    dollar_finish(sim_hdl, "32", 1u);
}


/* Methods */


/* Reset routines */

void MOD_mkTB::reset_RST_N(tUInt8 ARG_rst_in)
{
  PORT_RST_N = ARG_rst_in;
  INST_started.reset_RST(ARG_rst_in);
  INST_reg_unused_0.reset_RST(ARG_rst_in);
  INST_m.reset_RST_N(ARG_rst_in);
}


/* Static handles to reset routines */


/* Functions for the parent module to register its reset fns */


/* Functions to set the elaborated clock id */

void MOD_mkTB::set_clk_0(char const *s)
{
  __clk_handle_0 = bk_get_or_define_clock(sim_hdl, s);
}


/* State dumping routine */
void MOD_mkTB::dump_state(unsigned int indent)
{
  printf("%*s%s:\n", indent, "", inst_name);
  INST_m.dump_state(indent + 2u);
  INST_reg_unused_0.dump_state(indent + 2u);
  INST_started.dump_state(indent + 2u);
}


/* VCD dumping routines */

unsigned int MOD_mkTB::dump_VCD_defs(unsigned int levels)
{
  vcd_write_scope_start(sim_hdl, inst_name);
  vcd_num = vcd_reserve_ids(sim_hdl, 3u);
  unsigned int num = vcd_num;
  for (unsigned int clk = 0u; clk < bk_num_clocks(sim_hdl); ++clk)
    vcd_add_clock_def(sim_hdl, this, bk_clock_name(sim_hdl, clk), bk_clock_vcd_num(sim_hdl, clk));
  vcd_write_def(sim_hdl, bk_clock_vcd_num(sim_hdl, __clk_handle_0), "CLK", 1u);
  vcd_write_def(sim_hdl, num++, "RST_N", 1u);
  num = INST_reg_unused_0.dump_VCD_defs(num);
  num = INST_started.dump_VCD_defs(num);
  if (levels != 1u)
  {
    unsigned int l = levels == 0u ? 0u : levels - 1u;
    num = INST_m.dump_VCD_defs(l);
  }
  vcd_write_scope_end(sim_hdl);
  return num;
}

void MOD_mkTB::dump_VCD(tVCDDumpType dt, unsigned int levels, MOD_mkTB &backing)
{
  vcd_defs(dt, backing);
  vcd_prims(dt, backing);
  if (levels != 1u)
    vcd_submodules(dt, levels - 1u, backing);
}

void MOD_mkTB::vcd_defs(tVCDDumpType dt, MOD_mkTB &backing)
{
  unsigned int num = vcd_num;
  if (dt == VCD_DUMP_XS)
  {
    vcd_write_x(sim_hdl, num++, 1u);
  }
  else
    if (dt == VCD_DUMP_CHANGES)
    {
      if ((backing.PORT_RST_N) != PORT_RST_N)
      {
	vcd_write_val(sim_hdl, num, PORT_RST_N, 1u);
	backing.PORT_RST_N = PORT_RST_N;
      }
      ++num;
    }
    else
    {
      vcd_write_val(sim_hdl, num++, PORT_RST_N, 1u);
      backing.PORT_RST_N = PORT_RST_N;
    }
}

void MOD_mkTB::vcd_prims(tVCDDumpType dt, MOD_mkTB &backing)
{
  INST_reg_unused_0.dump_VCD(dt, backing.INST_reg_unused_0);
  INST_started.dump_VCD(dt, backing.INST_started);
}

void MOD_mkTB::vcd_submodules(tVCDDumpType dt, unsigned int levels, MOD_mkTB &backing)
{
  INST_m.dump_VCD(dt, levels, backing.INST_m);
}