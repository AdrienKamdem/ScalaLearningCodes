object td2 {

  import scala.math.sqrt
  import scala.util.Random
  import scala.annotation.tailrec
  import scala.math.abs

  ///////////////////////////////////// EXERCICE 1

  // Qts A)
  def isLeap(year:Int) = {
    if (year < 0) None
    else Some((year % 400 == 0) || ((year % 4 == 0) && (year % 100 != 0)))
  }

  // Qts B)
  def nbDays (mois:Int, Leap:Boolean) = {
    mois match{
      case 1|3|5|7|8|10|12 => Some(31)
      case 4|6|9|11 => Some(30)
      case 2 if Leap => Some(29)
      case 2 if !Leap => Some(28)
      case _ => None
    }

  }

  // Qts C)
  // Leap devient lazy,
  // nous utilisons leap que lorsque leap est utilisé conditions cad
  // pour fevrier :: leap est évalué en différé uniquement lors de son appel

  ///////////////////////////////////// EXERCICE 2

  // Qts A)
  def piApp (n:Int) = {
    var k = 1.0
    var s = 0.0
    while (k<=n){
      s += (1/(k*k))
      k += 1
    }
    scala.math.sqrt(6*(s))
  }

  def piAppRec(n:Int):Double = {
    @tailrec // pour récursion terminale
    def piBisRec(n:Int,acc:Double=1.0):Double={
      if (n==1)acc
      else piBisRec(n-1,acc+1.0/(n*n))
    }
    sqrt(6*piBisRec(n))
  }

  // Qts B)
  def piMC(n : Int)={
    @tailrec
    def aux(x : Int, n : Int) : Int={
      if (n == 1)x
      else {
        var res = x
        val rand = new util.Random()
        val x1 = rand.nextDouble()
        val x2 = rand.nextDouble()
        if (x1*x1 + x2*x2 <= 1.0)res = res+1
        aux(res, n-1)
      }
    }
    4*aux(0, n)/n.toDouble
  }

  // Qts C)
  // Une fonction est pure si
  // elle opère que par rapport a ses arguments,
  // aucune déclaration de variables

  ///////////////////////////////////// EXERCICE 3

  // Qts A)
  def squareRoot(a:Double, esp:Double):Double={
    @tailrec
    def squareRoot_TR(guess:Double):Double = {
      if(abs(guess*guess-a) < esp)guess
      else squareRoot_TR(guess-((guess*guess-a)/(2*guess)))
    }
    squareRoot_TR(1.0)
  }

  // Qts B)
  def guess[T](isGoodEnough : T => Boolean, improveGuess : T => T, initialGuess : T) : T = {
    if (isGoodEnough(initialGuess)) initialGuess
    else guess(isGoodEnough, improveGuess, improveGuess(initialGuess))
  }

  // Execution de tous les codes
  def main(args:Array[String]): Unit ={
    println(isLeap(2012))
    println(nbDays(2,true))
    println(piApp(1000))
    println(piAppRec(1000))
    println(piMC(1000))
    println(squareRoot(16.0, 0.0001))
  }

}
