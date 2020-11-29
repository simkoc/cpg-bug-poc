package de.tubs

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, nodes}
import io.shiftleft.passes.{DiffGraph, IntervalKeyPool, ParallelCpgPass}

class CreateInitialSetupPass(something : List[Int], cpg : Cpg, keyPool : IntervalKeyPool)
  extends ParallelCpgPass[Int](cpg, keyPools = Some(keyPool.split(something.size))) {

  override def partIterator: Iterator[Int] = something.iterator

  override def runOnPart(part: Int): Iterator[DiffGraph] = {
    implicit val diffGraph: DiffGraph.Builder = DiffGraph.newBuilder
    if(part == 0) {
      createMain()
      createTest()
    }
    Iterator(diffGraph.build())
  }

  def createMain()(implicit diffGraph: DiffGraph.Builder) : Unit = {
    // creating the AST
    val method = nodes.NewMethod("dlr_main()")
    diffGraph.addNode(method)
    val block = nodes.NewBlock("DLR MAIN BLOCK")
    diffGraph.addNode(block)
    diffGraph.addEdge(method, block, EdgeTypes.AST)
    val firstExtStmt = nodes.NewCall("EXT_STMT","EXT_STMT",0)
    diffGraph.addNode(firstExtStmt)
    diffGraph.addEdge(block, firstExtStmt, EdgeTypes.AST)
    val fcall = nodes.NewCall("INIT_FCALL 0 80 string(\"test\")",order = 1, name="INIT_FCALL")
    diffGraph.addNode(fcall)
    diffGraph.addEdge(block, fcall, EdgeTypes.AST)
    val zero = nodes.NewLiteral("0",0)
    diffGraph.addNode(zero)
    diffGraph.addEdge(fcall, zero, EdgeTypes.AST)
    diffGraph.addEdge(fcall, zero, EdgeTypes.ARGUMENT)
    val eighty = nodes.NewLiteral("80",1)
    diffGraph.addNode(eighty)
    diffGraph.addEdge(fcall, eighty, EdgeTypes.AST)
    diffGraph.addEdge(fcall, eighty, EdgeTypes.ARGUMENT)
    val string = nodes.NewLiteral("test",2)
    diffGraph.addNode(string)
    diffGraph.addEdge(fcall, string, EdgeTypes.AST)
    diffGraph.addEdge(fcall, string, EdgeTypes.ARGUMENT)
    val doFcall = nodes.NewCall("DO_FCALL",order = 2, name = "DO_FCALL")
    diffGraph.addNode(doFcall)
    diffGraph.addEdge(block, doFcall, EdgeTypes.AST)
    val secondExtStmt = nodes.NewCall("EXT_STMT","EXT_STMT",3)
    diffGraph.addNode(secondExtStmt)
    diffGraph.addEdge(block, secondExtStmt, EdgeTypes.AST)
    val echo = nodes.NewCall("ECHO string(\"after call\")", order = 4, name = "ECHO")
    diffGraph.addNode(echo)
    diffGraph.addEdge(block, echo, EdgeTypes.AST)
    val echoString = nodes.NewLiteral("after call",0)
    diffGraph.addNode(echoString)
    diffGraph.addEdge(echo, echoString, EdgeTypes.AST)
    // creating the CFG
    diffGraph.addEdge(method, block, EdgeTypes.CFG)
    diffGraph.addEdge(block, firstExtStmt, EdgeTypes.CFG)
    diffGraph.addEdge(firstExtStmt, zero, EdgeTypes.CFG)
    diffGraph.addEdge(zero, eighty, EdgeTypes.CFG)
    diffGraph.addEdge(eighty, string, EdgeTypes.CFG)
    diffGraph.addEdge(string, fcall, EdgeTypes.CFG)
    diffGraph.addEdge(fcall, doFcall, EdgeTypes.CFG)
    diffGraph.addEdge(doFcall, secondExtStmt, EdgeTypes.CFG)
    diffGraph.addEdge(secondExtStmt, echoString, EdgeTypes.CFG)
    diffGraph.addEdge(echoString, doFcall, EdgeTypes.CFG)
  }

  def createTest()(implicit diffGraph: DiffGraph.Builder) : Unit = {
    diffGraph.addNode(nodes.NewMethod("test()",name = "test",fullName = "test"))
  }
}
