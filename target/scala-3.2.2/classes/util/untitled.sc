
val list: List[List[Integer]] = Nil
 val size = 8
    def makeTriangle(l: List[Integer], counter: Integer): List[Integer] = {
      if (counter > size) return Nil
      else {
        l match
          case x::Nil => x.tolList :: x
          case x::xs => l.sliding(2).map(x=>(x._1+x._2))
      }
    }