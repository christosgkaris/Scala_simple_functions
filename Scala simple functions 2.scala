
object Exercises {
  	

	def from(n: Int, m: Int): List[Int] = {
    	if (n > m) Nil
    	else n :: from(n+1, m)
	}
	//Exercises.from(3,7)
	//List(3,4,5,6,7)


	def pascalNewRow(l: List[Int]): List[Int] = l match {
		case Nil => Nil
		case a :: Nil => a :: Nil // a will be 1
		case a :: b :: t => if (a == 1) 1 :: (a+b) :: pascalNewRow(b :: t)
							else (a+b) :: pascalNewRow(b :: t)
	}
	//Exercises.pascalNewRow(List(1,3,3,1))
	//List(1,4,6,4,1)


	def interleave(s1: Stream[Int], s2: Stream[Int]): Stream[Int] = {
		s1.head #:: s2.head #:: interleave(s1.tail, s2.tail)
	}
	//Exercises.interleave(Stream(1,2,3,4),Stream(10,20,30,40)).take(8).toList
	//List(1, 10, 2, 20, 3, 30, 4, 40)


	def append(l1: List[Int], l2: List[Int]): List[Int] = {
		if (l1 != Nil) l1.head :: append(l1.tail, l2)
		else if ((l1 == Nil) && (l2 != Nil)) l2.head :: append (Nil, l2.tail)
		else Nil
	}
	//Exercises.append(List(1,2,3),List(3,2,1))
	//List(1,2,3,3,2,1)


	def combinations(n: Int): List[List[Int]] = {
		
		def numToBin(l: List[Int], n: Int): List[Int] = {
			if ((n == 0) || (n == 1)) List(n) ::: l.tail
			else n % 2 :: numToBin(l.tail, n / 2)
		}
		
		def loop(first: List[Int], n: Int, acum: Int): List[List[Int]] = {
			if (acum >= Math.pow(2, n)) Nil
			else List(numToBin(first, acum).reverse) ::: loop(List.fill(n)(0), n, acum+1)
		}
		
		loop(List.fill(n)(0), n, 0)
	}
	//Exercises.combinations(3)
	//List(List(0, 0, 0), List(0, 0, 1), List(0, 1, 0), List(0, 1, 1), List(1, 0, 0), List(1, 0, 1), List(1, 1, 0), List(1, 1, 1))


	def appendHOF(l1: List[Int], l2: List[Int]): List[Int] = {
		
		def loop(l: List[(Int, Int)]): List[Int] = l match {
			case Nil => Nil
			case h :: t => (h._1 :: loop(t)) :+ h._2
		}

		loop(l1.zip(l2.reverse))
	}
	//Exercises.appendHOF(List(1,2,3),List(4,5,6))
	//List(1,2,3,4,5,6)


	def addSelected(l: List[(Double,Int)], n: Int): Double = {
		
		def addSelected_2(l: List[(Double,Int)], accu: Double): Double = {
			if (l.isEmpty) accu
			else addSelected_2(l.tail, accu + l.head._1)
		}
		
		addSelected_2(l.filter(_._2 == n), 0)
	}
	//Exercises.addSelected(List((2.0,0),(4.5,1),(1.2,1),(3.0,3),(4.4,1),(4.5,0),(1.7,0),(5.3,2),(2.0,3)),3)
	//5

	def addAll2Num(l: List[(Double,Int)], n: Int, sum: Double): Double = l match {
		case Nil => sum
		case h :: t => if (h._2 == n) addAll2Num(t, n, sum + h._1)
					   else addAll2Num(t, n, sum)
	}
	def addAll2(l1: List[(Double,Int)], l2: List[Int]): List[Double] = l2 match {
		case Nil => Nil
		case h :: t => addAll2Num(l1, h, 0) :: addAll2(l1, t)
	}
	//Exercises.addAll2(List((2.0,0),(4.5,1),(1.2,1),(3.0,3),(4.4,1),(4.5,0),(1.7,0),(5.3,2),(2.0,3)),List(0,1,2,3))
	//List(8.2, 10.100000000000001, 5.3, 5.0)


	def seqSin(): Stream[Double] = {
		def seqSin2(i: Int = 1): Stream[Double] = math.sin(i / 2) #:: seqSin2(i + 1)
		seqSin2()
	}
	//Exercises.seqSin().take(5).toList
	//List(0.0, 0.8414709848078965, 0.8414709848078965, 0.9092974268256817, 0.9092974268256817)
}
