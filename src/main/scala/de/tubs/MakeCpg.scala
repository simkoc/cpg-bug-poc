package de.tubs

import de.tubs.MakeCpg.createCpg
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.passes.KeyPoolCreator
import io.shiftleft.x2cpg.X2Cpg

object MakeCpg {
  def createCpg(cpg: Cpg): Cpg = {
    val keyPools = KeyPoolCreator.obtain(1).iterator
    new CreateInitialSetupPass(List(0),cpg, keyPools.next()).createAndApply()
    cpg
  }

}

class MakeCpg {

  def run(outputPath: String): Cpg = {
    val cpg = X2Cpg.newEmptyCpg(Some(outputPath))
    createCpg(cpg)
  }

}
