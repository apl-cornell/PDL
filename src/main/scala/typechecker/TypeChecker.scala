package pipedsl.typechecker

import pipedsl.common.Syntax.Prog

//TODO kinds of typechecking we need to do:

//Normal stuff
//  Assignment
//  Function calls and returns
//  Module Instantiations
//
//Pipeline Stuff
//  Call statment exactly once in each path w/ typed args
//  Dynamic Lock Checking
//  More complex lock types (indexed / dependent)
//  Speculation Restrictions
//
//Security Types
//  Normal IFC
//  How to Check "call"s
//  Speculation Labels
object TypeChecker {

  def typeCheck(p: Prog): Unit = {
    val Prog(fdefs, mdefs, cir) = p
  }
}
