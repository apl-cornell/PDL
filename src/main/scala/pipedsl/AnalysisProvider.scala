package pipedsl.typechecker

import pipedsl.common.Syntax.Prog

trait AnalysisProvider[Analysis]
 {
  def get(program :Prog) :Analysis
 }
