package de.tubs

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, nodes}
import io.shiftleft.semanticcpg.language._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.jdk.CollectionConverters._

class TestForBug  extends AnyWordSpec with Matchers {

  val cpg: Cpg = CpgTestFixture.createCpg()

  "the cpg" should {
    "have a single CFG edge after DO_FCALL" in {
      val out = cpg.call("DO_FCALL").head.out(EdgeTypes.CFG).asScala.toList
      println(out.map(_.asInstanceOf[nodes.CfgNode].code))
      out.length shouldBe 1
      out.map(_.asInstanceOf[nodes.CfgNode].code).toSet shouldBe Set("after call")
    }
  }

}
