object FilteringForGood {
    
    implicit class ExtendedList[T](val value: List[T]) extends AnyVal {
        def myFilter (pred: (T => Boolean)) {
          def filter (current: List[T], filtered: List[T]): List[T] = {
              if (current.isEmpty) {
                  filtered.reverse
              }  else {
                  val head = current.head
                  filter(current.tail, if(pred(head)) filtered else head :: filtered)
              }
          }
          filter(value, List())
        }
    }
    
    def main(args: Array[String]) {
        println(List(4, 1,2,3).myFilter((x:Int) => x < 3))
    }
}