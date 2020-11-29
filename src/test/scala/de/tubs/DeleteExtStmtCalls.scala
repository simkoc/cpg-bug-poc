package de.tubs

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.semanticcpg.language._
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, nodes}
import io.shiftleft.passes.{DiffGraph, IntervalKeyPool, ParallelCpgPass}

import scala.jdk.CollectionConverters._

class DeleteExtStmtCalls(methods: Seq[nodes.Method],
                             cpg: Cpg,
                             keyPool: IntervalKeyPool)
  extends ParallelCpgPass[nodes.Method](
    cpg: Cpg,
    keyPools = Some(keyPool.split(methods.length))) {

  private val emptyOpcodeNames = List("EXT_NOP", "EXT_STMT")

  override def partIterator: scala.Iterator[nodes.Method] = methods.iterator

  override def runOnPart(part: nodes.Method): Iterator[DiffGraph] = {
    //todo: this breaks if we ever start labeling CFG edges!
    implicit val diffGraph: DiffGraph.Builder = DiffGraph.newBuilder
    val emptyOpcodeSet =
      emptyOpcodeNames.foldRight(Set[nodes.Call]())((name, set) =>
        part.astMinusRoot.isCall(name).toSet ++ set)
    if (emptyOpcodeSet.nonEmpty) {
      skipEmptyOpcodes(emptyOpcodeSet)
      emptyOpcodeSet.foreach(diffGraph.removeNode)
    }
    Iterator(diffGraph.build)
  }

  // we assert here that a loop only involving empty opcodes is impossible and the edges are added AFTER the whole
  // pass is done thus we cannot enter a loop
  def getSkipStart(call: nodes.Call, remainingEmptyOpcodeSet: Set[nodes.Call])
  : (List[nodes.CfgNode], Set[nodes.Call]) = {
    var startList: List[nodes.CfgNode] = Nil
    var currentEmptyOpcodeSet = remainingEmptyOpcodeSet
    // go over each incoming cfg edge
    // the below assertion is possible in case of dead code starting with a EmptyOpcode
    call._cfgIn.asScala.foreach {
      // if the incoming edge is a call
      case incoming: nodes.Call =>
        // check if it is a call to an empty opcode
        if (emptyOpcodeNames.contains(incoming.name)) {
          // if so recurse on that opcode
          val (returnedSkipStartList, returnedEmptyOpcodeSet) =
            getSkipStart(incoming, currentEmptyOpcodeSet)
          // append the listBuilder by the returned skipStart list
          startList = startList ++ returnedSkipStartList
          assert(startList.nonEmpty)
          // reduce the opcode set by the current empty opcode
          assert(returnedEmptyOpcodeSet.contains(incoming))
          currentEmptyOpcodeSet = returnedEmptyOpcodeSet - incoming
        } else {
          startList = incoming :: startList
        }
      // if the incoming edge is not a call then we cannot have an empty opcode
      case any: nodes.CfgNode =>
        // append the listBuilder by the current call
        startList = any :: startList
        assert(startList.nonEmpty)
      case _ =>
        throw new RuntimeException(
          "a empty opcode cannot be CFG preceded by a non CFG node")
    }
    // return the listBuilder as a list, return the remaining empty opcodes
    (startList, currentEmptyOpcodeSet)
  }

  def getSkipEnd(call: nodes.Call, remainingEmptyOpcodeSet: Set[nodes.Call])
  : (nodes.CfgNode, Set[nodes.Call]) = {
    assert(call._cfgOut.asScala.toList.length == 1,
      "an empty opcode has more than one outgoing edge")
    call._cfgOut.next() match {
      case outgoing: nodes.Call =>
        if (emptyOpcodeNames.contains(outgoing.name)) {
          assert(remainingEmptyOpcodeSet.contains(outgoing))
          val (skipEnd, returnedRemainingEmptyOpcodeSet) =
            getSkipEnd(outgoing, remainingEmptyOpcodeSet)
          (skipEnd, returnedRemainingEmptyOpcodeSet - outgoing)
        } else {
          (outgoing, remainingEmptyOpcodeSet)
        }
      case other: nodes.CfgNode => (other, remainingEmptyOpcodeSet)
    }
  }

  def getParentCall(node: nodes.CfgNode): nodes.CfgNode = {
    node._astIn().next() match {
      case parent: nodes.Call => getParentCall(parent)
      case _ =>
        assert(node.isInstanceOf[nodes.Call] ||
          node.isInstanceOf[nodes.Method] ||
          node.isInstanceOf[nodes.Block],
          s"${node.code} has no direct call node parent")
        node
    }
  }

  def skipEmptyOpcodes(emptyOpcodeSet: Set[nodes.Call])(
    implicit diffGraph: DiffGraph.Builder): Unit = {
    if (emptyOpcodeSet.nonEmpty) {
      val (starts, remainingOpcodeSet) = {
        getSkipStart(emptyOpcodeSet.head, emptyOpcodeSet)
      }
      val (end, actuallyRemainingOpcodeSet) =
      getSkipEnd(emptyOpcodeSet.head, remainingOpcodeSet)
      starts.foreach { start =>
        diffGraph.addEdge(start, end, EdgeTypes.CFG)
      }
      assert(actuallyRemainingOpcodeSet.size <= emptyOpcodeSet.size,
        "after a step the remaining set has to be smaller")
      skipEmptyOpcodes(actuallyRemainingOpcodeSet - emptyOpcodeSet.head)
    }
  }
}

object DeleteExtStmtCalls {

  def getMethodDeclarations(cpg: Cpg): Seq[nodes.Method] = {
    cpg.method.l
  }

}