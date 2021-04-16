package pipedsl.analysis

import pipedsl.common.Syntax.Prog

trait AnalysisProvider[Analysis] {
  
  def get(program: Prog): Analysis
  
}
