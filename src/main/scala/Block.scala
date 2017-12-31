import java.security.MessageDigest
import org.joda.time.DateTime
import org.apache.commons.codec.binary._


case class Block(
                  index: Long,
                  timestamp: DateTime,
                  data:BlockData,
                  hash:String,
                  pHash:String,
                  diff:Int,
                  nonce:Int
                )
object Block {
  private def sha256(s:String):String = {
    Hex.encodeHex(MessageDigest.getInstance("SHA-256").digest(s.getBytes("UTF-8"))).mkString("")
  }
  def apply(oldBlock:Block, d:BlockData, newNonce:Int = 0):Block ={
    val i = oldBlock.index+1
    val t = DateTime.now()
    val p = oldBlock.hash
    val x = oldBlock.diff
    Block(i, t, d, sha256(""+i+t+d.s+p+x), p, x, newNonce)
  }

  def apply():Block ={
    val i = 0
    val t = DateTime.now()
    val d = BlockData("GENESIS_BLOCK")
    val p = ""
    val x = 1
    Block(0, t, d, sha256(""+i+t+d.s+p+x), p, x, 0)
  }
}

case class BlockData(s:String)
