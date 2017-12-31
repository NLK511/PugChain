import org.joda.time.DateTime

case class Peer(name:String, chain:List[Block] = List(Block())) {

  def addBlock(b:Block):Peer = {
    val mb = proofOfWork(b)
    if (validateBlock(mb))
      this.copy(chain = chain:+mb)
    else this
  }

  def validateBlock(b:Block) = {
    val isValidIndex = chain.last.index < b.index
    //FIXME this toy version seems to be too fast for this kind of check. Adjacent blocks are sometimes dropped because their timestamp it's identical
    val isValidTime = true
    //val isValidTime = chain.last.timestamp.isBefore(b.timestamp) && b.timestamp.isBefore(DateTime.now)
    val isValidOldHash = chain.last.hash == b.pHash
    val validHashPattern = 0.toString*b.diff
    val isValidHash = b.hash.toString.startsWith(validHashPattern)

    isValidIndex && isValidTime && isValidOldHash && isValidHash
  }

  def proofOfWork(b:Block):Block = {
    val n = b.nonce
    val d = b.diff
    val validHashPattern = 0.toString*b.diff

    val newNonce = (1 to Int.MaxValue).find{ x =>
      val newBlock = Block(chain.last, b.data, x)
      newBlock.hash.toString.startsWith(validHashPattern)
    }
    //FIXME if Int range does not provide a solution the function fails and return the original (not resolved) block
    Block(chain.last, b.data, newNonce.getOrElse(0))
  }

}

