package de.tubs

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, nodes}
import io.shiftleft.passes.{DiffGraph, IntervalKeyPool, ParallelCpgPass}

class CreateInitialSetupPass(something: List[Int],
                             cpg: Cpg,
                             keyPool: IntervalKeyPool)
    extends ParallelCpgPass[Int](
      cpg,
      keyPools = Some(keyPool.split(something.size))) {

  override def partIterator: Iterator[Int] = something.iterator

  override def runOnPart(part: Int): Iterator[DiffGraph] = {
    implicit val diffGraph: DiffGraph.Builder = DiffGraph.newBuilder
    val method = nodes.NewMethod().name("dlr_main()")
    diffGraph.addNode(method)
    val block = nodes.NewBlock().code("DLR MAIN BLOCK")
    diffGraph.addNode(block)
    diffGraph.addEdge(method, block, EdgeTypes.POST_DOMINATE)
    Iterator(diffGraph.build())
  }
}
