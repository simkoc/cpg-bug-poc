package de.tubs

import io.shiftleft.passes.KeyPoolCreator
import io.shiftleft.x2cpg.X2Cpg
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class TestForBug  extends AnyWordSpec with Matchers {

  "the cpg creation test" should {
    "fail" in {
      val cpg = X2Cpg.newEmptyCpg(Some("/tmp/out.cpg"))
      val keyPools = KeyPoolCreator.obtain(1).iterator
      val run = new CreateInitialSetupPass(List(0), cpg, keyPools.next())
      run.createAndApply()
    }
  }

}
