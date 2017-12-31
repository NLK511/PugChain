
object PugChainApp extends App {
  println("Welcome to PugChain")

  var c = Peer("Miner 0")
  val nBlocks = 30

  for (i <- 1 to nBlocks){
    val b = Block(c.chain.last, BlockData("block number"+i))
    c = c.addBlock(b)
  }
  c.chain.map(println)

}