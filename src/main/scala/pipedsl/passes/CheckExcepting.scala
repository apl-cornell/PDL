//package pipedsl.passes
//
//import pipedsl.common.Errors.{ReleaseInExnBlock, ReleaseWhenMaybeExcepting}
//import pipedsl.common.Locks.Released
//import pipedsl.common.Syntax._
//
//object CheckExcepting
// {
//  def run(p :Prog) :Unit =
//   {
//    p.moddefs.foreach(checkModule)
//   }
//  private def checkModule(m :ModuleDef) :Unit =
//   if (is_excepting(m))
//    {
//     check_exn_block(m.except_blk.get)
//     check_body(m.body)
//    }
//
//  private def check_body(c :Command) :Unit = c match
//  {
//   case CSeq(c1, c2) => check_body(c1); check_body(c2);
//   case CTBar(c1, c2) => check_body(c1); check_body(c2);
//   case CIf(_, cons, alt) => check_body(cons); check_body(alt)
//   case CSplit(cases, default) => cases.foreach(c => check_body(c.body))
//   case CLockOp(_, Released, lt, _, _) if !lt.contains(LockRead) =>
//    throw ReleaseWhenMaybeExcepting(c.pos)
//   case _ => ()
//  }
//
//  private def check_exn_block(c :Command): Unit = c match
//  {
//   case CSeq(c1, c2) => check_exn_block(c1); check_exn_block(c2)
//   case CTBar(c1, c2) => check_exn_block(c1); check_exn_block(c2);
//   case CIf(_, cons, alt) => check_exn_block(cons); check_exn_block(alt)
//   case CSplit(cases, default) => cases.foreach(c => check_exn_block(c.body)); check_exn_block(default)
//   case CLockOp(_, Released, _, _, _) => throw ReleaseInExnBlock(c.pos)
//   case _ => ()
//  }
//
//
// }
