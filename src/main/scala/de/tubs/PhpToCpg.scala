package de.tubs

import de.tubs.PhpToCpg.createCpg
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.passes.KeyPoolCreator
import io.shiftleft.x2cpg.X2Cpg

object PhpToCpg {
  def createCpg(cpg: Cpg): Cpg = {
    val keyPools = KeyPoolCreator.obtain(13).iterator
    new CreateInitialSetupPass(List(0,1,2),cpg, keyPools.next()).createAndApply()
    new DeleteExtStmtCalls(DeleteExtStmtCalls.getMethodDeclarations(cpg), cpg, keyPools.next()).createAndApply()
    new TriggerBugPass(List(0,1,2),cpg,keyPools.next()).createAndApply()
    cpg
  }

}

class PhpToCpg {

  def run(outputPath: String): Cpg = {
    val cpg = X2Cpg.newEmptyCpg(Some(outputPath))
    createCpg(cpg)
  }

}
