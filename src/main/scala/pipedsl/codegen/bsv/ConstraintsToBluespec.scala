package pipedsl.codegen.bsv

import pipedsl.common.Constraints._
import pipedsl.codegen.bsv.BSVSyntax.{PAdd, PEq, PMax, Proviso}
import pipedsl.common.Syntax.Id

import scala.collection.mutable

/**
 * code to turn a list of constraints into a list of Bluespec Provisos
 */
object ConstraintsToBluespec
 {

  var globl_cnt = 1

  def freshVar() :Id =
  {
   globl_cnt += 1;
   Id("_proviso_fresh" + globl_cnt)
  }

  def collect_vars(e :IntExpr) :Set[Id] = e match
  {
   case IntConst(v) => Set()
   case IntVar(id) => Set(id)
   case IntAdd(a, b) => collect_vars(a).union(collect_vars(b))
   case IntSub(a, b) => collect_vars(a).union(collect_vars(b))
   case IntMax(a, b) => collect_vars(a).union(collect_vars(b))
  }

  def collect_vars(c :Constraint) : Set[Id] = c match
  {
   case RelLt(a, b) => collect_vars(a).union(collect_vars(b))
   case ReGe(a, b) => collect_vars(a).union(collect_vars(b))
   case ReEq(a, b) => collect_vars(a).union(collect_vars(b))
  }


  type memomap = mutable.HashMap[IntExpr, IntValue]


  def tile_one(c :IntExpr, memo :memomap) : (List[Proviso], IntValue) =
   {
    def helper(a :IntExpr, b :IntExpr, tp :(String, String, String) => Proviso) =
     {
      val frsh = freshVar()
      val (prov_left, left_dest) = tile_one(a, memo)
      val (prov_right, right_dest) = tile_one(b, memo)
      ((prov_left ++ prov_right).prepended(tp(left_dest.toString, right_dest.toString, frsh.v)),
        IntVar(frsh))
     }
    memo.get(c) match
    {
     case Some(value) =>
      (List(), value)
     case None => val tmp = c match
     {
      case c :IntConst=> (List(), c)
      case c :IntVar => (List(), c)
      case IntAdd(a, b) => helper(a, b, PAdd)
      case IntSub(a, b) => helper(a, b, (left, right, dest) => PAdd(right, dest, left))
      case IntMax(a, b) => helper(a, b, PMax)
     }
     memo.addOne(c, tmp._2)
     tmp
    }
  }

  def to_provisos_one(c :Constraint, memo :memomap) :List[Proviso] =
   {
    c match
    {
     //max(a, b) = b <=> b >= a
     //b > a <=> b >= a + 1)
     //a < b <=> a + 1 <= b
     //a + 1 <= b <=> (max (1+ a) b) = b
     case RelLt(a, b) =>
      val (provs_left, left) = tile_one(a, memo)
      val (provs_right, right) = tile_one(b, memo)
      val frsh = freshVar()
      val one_p_left = PAdd("1", left.toString, frsh.v)
      val prov = PMax(frsh.v, right.toString, right.toString)
      (provs_left ++ provs_right).prependedAll(List(one_p_left, prov))
     case ReGe(a, b) =>
      val (provs_left, left) = tile_one(a, memo)
      val (provs_right, right) = tile_one(b, memo)
      val prov = PMax(left.toString, right.toString, left.toString)
      (provs_left ++ provs_right).prepended(prov)
     case ReEq(a, b) =>
      val (provs_left, left) = tile_one(a, memo)
      val (provs_right, right) = tile_one(b, memo)
      val prov = PEq(left.toString, right.toString)
      (provs_left ++ provs_right).prepended(prov)
    }
   }

  def to_provisos(cstrts :List[Constraint]) :List[Proviso] =
   {
    val memo :memomap = mutable.HashMap()
    cstrts.flatMap(to_provisos_one(_, memo)).distinct
   }

 }
