import scala.annotation.tailrec

/**
  * Created by yuanping on 16/9/28.
  */
object mytest {
  def main(args: Array[String]): Unit = {
    println("start running ")
    val intList = List(1,2,3,4)
    val strList = intList.map{x => x.toString}

    //1get last element in list
    @tailrec
    def last(l : List[Int]):Int = l match {
      case Nil => 0
      case x::Nil => x
      case x::y => last(y)
    }
    println("1 the last element of the array is :"+last(intList))

    //2find the Kth element of a list
    //if the index greater than the length return the last one
    def nth(n:Int,l:List[Int]):Int = {
      @tailrec
     def getNth(k:Int , l1 :List[Int]):Int = {
       l1 match {
         case x::Nil =>x
         case x::y if k ==0 => x
         case x::y if k >=0 => getNth(k-1,y)
       }
     }
      getNth(n,l)
    }
    println("2 the element 3 in int list is :" +nth(3,intList))
    //3Reverse a list
    def reverse(l:List[Any]):List[Any] = {
      @tailrec
      def reverseList(original:List[Any],result:List[Any]):List[Any] = original match {
        case Nil => result
        case x::y =>reverseList(y,x::result)
      }
      reverseList(l,List())
    }
    println("3 the reversed list: "+reverse(intList))
    //used for the next 4,5
    val duplicateList = List("a","a","a","a","b","b","c","c","c","c","d")
    //4eliminate consecutive duplicates of list elements
    def compress(l :List[String]):List[String] = {
      @tailrec
        def compressList(original :List[String],result :List[String]):List[String] = original match {
          case Nil => Nil
          case x::Nil => x::result
          case x::y if x == y.head => compressList(y,result)
          case x::y if x !=y.head =>compressList(y,x::result)
          //case x::y if x!=y.head =>compressList(y,result::x)?? why this is not work?
        }
      compressList(l,List())
    }


    println("4 the simple element in array is :"+compress(duplicateList))

    //5run-length encoding of a list
    def encode(l:List[String]):List[(Int,String)] = {
     def encodeFun(origin:List[String],result :List[(Int,String)]): List[(Int,String)] ={
       origin match {
         case Nil => Nil
         //case x::Nil if x == result.head.copy()..toString() =>
         case x :: y if x != y.head => encodeFun(y, (1, x) :: result)
         case x :: y if x == y.head & result == Nil => encodeFun(y, (1, x) :: result)
         case x :: y if x == y.head & result != Nil => encodeFun(y, ((result.head.copy().x.toString().toInt + 1): Int, x)::result)
       }
     }
      encodeFun(l,List())
    }
    def encode1(l:List[String]):List[(Int,String)] = {

      l match {
        case Nil => Nil
        case x::y => encode1(y) match {
          case (c,  `x`)::rest =>(c+1,x)::rest
          case rest => (1, x)::rest
        }
      }
    }
    println("5 encoding 1 :" +encode1(duplicateList))
//    println("after encoding :"+ encode(duplicateList))

    // 6 any type
    def modifiedEncodeType(l:List[Any]):List[Any] = {

      l match {
        case Nil => Nil
        case x::y => modifiedEncodeType(y) match {
          case (c,  `x`)::rest =>(c.toString().toInt+1,x)::rest
          case rest => (x)::rest
        }
      }
    }
    def modifiedEncodeType2(l:List[String]):List[Any] = {
      val list = encode1(l)
        list.map(e => if( e._1==1 ) e._2 else e)
    }

    println("6 modiyfy the result "+modifiedEncodeType2(duplicateList))

    // problem 7 decode a run-length encoded list
    def decode(list :List[(Int, String)]):List[String] = {

      //val result = List[String]()
      //list.map(l => outputCharWithTime(l._1,l._2,Nil))
      //result


      def excuteDecode(list:List[(Int,String)],result:List[String]):List[String] = list match {
        case Nil => Nil
        case x::Nil=>outputCharWithTime(x._1,x._2,result)
        case x::y =>excuteDecode(y,outputCharWithTime(x._1,x._2,result))
      }

      def outputCharWithTime(times:Int,str:String , result :List[String]):List[String]={
        times match{
          case 0 => result
          case x => outputCharWithTime(times-1,str,str::result)
        }
      }
      reverse(excuteDecode(list,List[String]())).map(e=>e.toString)

    }
    def decodeFlatMap(l:List[(Int,String)]):List[String] ={
      @tailrec
      def convertEachPair(original:(Int,String),result:List[String]):List[String]= {
        original match {
          case (0,_) => result
          case (n,a) => convertEachPair( (n-1,a),a::result)
        }
      }
      l.map(x=>convertEachPair(x,List[String]())).flatten
//      l.flatmap(x => convertEachPair(x,List[String]()))
    }
    println("7 decode " + decode(encode1(duplicateList)))
    println("7 decode "+ decodeFlatMap(encode1(duplicateList)))
  }
}
