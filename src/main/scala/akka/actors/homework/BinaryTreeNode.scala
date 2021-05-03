package akka.actors.homework

import akka.actor.{Actor, ActorRef, ActorSystem, PoisonPill, Props}
import akka.util.Timeout

import scala.concurrent.{Await, ExecutionContext}
import scala.concurrent.duration.DurationInt

object BinaryTreeNode {
  private sealed trait Position

  private case object Left extends Position
  private case object Right extends Position

  def props(elem: Int, initiallyRemoved: Boolean): Props = Props(new BinaryTreeNode(elem, initiallyRemoved))
}

final class BinaryTreeNode(val elem: Int, initiallyRemoved: Boolean) extends Actor {
  import BinaryTreeNode._
  import BinaryTreeSet.Operation._
  import BinaryTreeSet.OperationReply._

  private var subtrees = Map[Position, ActorRef]()
  private var removed = initiallyRemoved

  override def receive: Receive = {
    case m: Insert => doInsert(m)
    case m: Remove => doRemove(m)
    case m: Contains => doContains(m)
  }

  private def doInsert(m: Insert): Unit = {
    if (m.elem == elem) {
      removed = false
      m.requester ! OperationFinished(m.id)
    } else {
      sendToChildOrCreate(m)
    }
  }

  private def doContains(m: Contains): Unit = {
    if (m.elem == elem) {
      m.requester ! ContainsResult(m.id, !removed)
    } else {
      if (!sendToExistingChild(m)) {
        m.requester ! ContainsResult(m.id, result = false)
      }
    }
  }

  private def doRemove(m: Remove): Unit = {
    if (m.elem == elem) {
      removed = true
      m.requester ! OperationFinished(m.id)
    } else {
      sendToChildOrCreate(m)
    }
  }

  private def sendToChildOrCreate(m: BinaryTreeSet.Operation): Unit = {
    if (m.elem < elem) {
      val leftChild = getChild(Left, m.elem)
      leftChild forward m
    } else {
      val rightChild = getChild(Right, m.elem)
      rightChild forward m
    }
  }

  private def sendToExistingChild(m: BinaryTreeSet.Operation): Boolean = {
    val position = if (m.elem < elem) Left else Right
    subtrees.get(position).fold(false) { child =>
      child forward m
      true
    }
  }

  private def getChild(position: Position, elem: Int): ActorRef = {
    subtrees.getOrElse(position, {
      val child = context.actorOf(BinaryTreeNode.props(elem, initiallyRemoved = true))
      subtrees = subtrees + (position -> child)
      child
    })
  }
}

object Homework extends App {
  import BinaryTreeSet.Operation._

  final class Printer extends Actor {
    override def receive: Receive = println(_)
  }

  implicit val system: ActorSystem = ActorSystem("Exercise3")
  implicit val ec: ExecutionContext = system.dispatcher
  implicit val timeout: Timeout = Timeout(2.seconds)

  private def pause(): Unit = {
    Thread.sleep(100)
    println("==============")
  }

  val treeSet = system.actorOf(Props[BinaryTreeSet](), name = "treeSet")
  val printer = system.actorOf(Props[Printer](), "printer")

  treeSet ! Contains(printer, 0, 1)
  treeSet ! Contains(printer, 1, 0)
  pause()
  treeSet ! Insert(printer, 2, 1)
  treeSet ! Insert(printer, 3, -1)
  treeSet ! Insert(printer, 4, 10)
  pause()
  treeSet ! Contains(printer, 5, 1)
  treeSet ! Contains(printer, 6, 0)
  treeSet ! Contains(printer, 7, 5)
  treeSet ! Contains(printer, 8, 10)
  pause()
  treeSet ! Remove(printer, 9, 1)
  pause()
  treeSet ! Contains(printer, 10, 1)
  treeSet ! Contains(printer, 11, 5)
  treeSet ! Contains(printer, 12, 10)

  Thread.sleep(500)

  println("done.")

  treeSet ! PoisonPill
  printer ! PoisonPill
}
