package de.tubs

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class TestForBug  extends AnyWordSpec with Matchers {

  "the cpg creation test" should {
    "fail" in {
      CpgTestFixture.createCpg()
    }
  }

}
