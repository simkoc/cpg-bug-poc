package de.tubs

import io.shiftleft.codepropertygraph.Cpg


object CpgTestFixture {

  def createCpg(): Cpg = {
    val cpg: Cpg = Cpg.emptyCpg
    PhpToCpg.createCpg(cpg)
  }

}
