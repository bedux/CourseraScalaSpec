/**
  * Created by bedux on 15.06.17.
  */
import patmat.Huffman._

object main {

  def main(srg:Array[String]):Unit = {

    println(createCodeTree(string2Chars("CiaoMondo")))


  }
}
