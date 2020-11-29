package de.tubs

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, nodes}
import io.shiftleft.passes.{DiffGraph, IntervalKeyPool, ParallelCpgPass}
import io.shiftleft.semanticcpg.language._

class TriggerBugPass(something: List[Int], cpg: Cpg, keyPool: IntervalKeyPool)
    extends ParallelCpgPass[Int](
      cpg,
      keyPools = Some(keyPool.split(something.size))) {

  override def partIterator: Iterator[Int] = something.iterator

  override def runOnPart(part: Int): Iterator[DiffGraph] = {
    implicit val diffGraph: DiffGraph.Builder = DiffGraph.newBuilder
    if (part == 0) {
      // find the DO_FCALL call
      val doFcall = cpg.call.codeExact("DO_FCALL").head
      // adding new AST node to represent the called method name
      val nameNode = nodes.NewLiteral(code = "test", 0, typeFullName = "target")
      diffGraph.addNode(nameNode)
      diffGraph.addEdge(doFcall, nameNode, EdgeTypes.AST)
      diffGraph.addEdge(doFcall, nameNode, EdgeTypes.ARGUMENT)
      // find the called method
      val method = cpg.method("test").head
      diffGraph.addEdge(doFcall, method, EdgeTypes.CALL)
    }
    val build = diffGraph.build()
    //build.iterator.foreach{
    //  change => println(change)
    //}
    Iterator(build)
  }
}
