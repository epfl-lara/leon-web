import leon.Utils._

/* VSTTE 2008 - Dafny paper */

/*
  Add the loop invariants so that Leon can verify this function.
*/

object Mult {

  def mult(x : Int, y : Int): Int = ({
    var r = 0
    if(y < 0) {
      var n = y
      (while(n != 0) {
        r = r - x
        n = n + 1
      }) //invariant(...)
    } else {
      var n = y
      (while(n != 0) {
        r = r + x
        n = n - 1
      }) //invariant(...)
    }
    r
  }) ensuring(_ == x*y)

}


