//Object is the way you make a Singleton class in Scala
object Main {
       def main(args: Array[String])
       {

       //In case someone places a string or some other value in that won't convert we do
       //a try-catch block.
       try {
          //val and var are dynamic types meaning they infer the data type.
	  //val is immutable meaning it doesn't change.
	  //var is mutable meaning it can be changed. (Think normal variables in C++ or Java)
	  //map takes the data on the left and applies the function on the right across a
	  //collection.
	  val elms = args map Integer.parseInt
	  println("The sum of my arguments is: " + elms.foldRight(0) (_ + _))
	  } catch {
	     case e: NumberFormatException =>
	      println("Improper data provided intergers only.")
	  }
}}
	      
