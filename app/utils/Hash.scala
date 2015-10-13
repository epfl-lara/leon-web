package leon.web
package utils

import java.security.MessageDigest

object Hash {

  def hash(str: String, offset: Int): String = {
    val bytes : Array[Byte] = str.getBytes("UTF-8")
    val digest = MessageDigest.getInstance("MD5")

    val sb = new StringBuilder()
    for (b <- digest.digest(bytes)) {
      sb.append(Integer.toHexString((b & 0xFF) | 0x100).substring(1,3))
    }

    sb.toString + "-" + offset
  }

}

