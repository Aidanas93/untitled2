val data: List[(String, String)] = List(
  ("2", "2"),
  ("3", "6"),
  ("6", "4"),
  ("King", "4"),
  ("5", "Bartukas")
)

case class Data(i: Int)

val sum = data.foldLeft(Data(0))((acc, x) => Data(acc.i + x))

/*Sum(21)*/

/*
0 <- initial state

1 acc = 0
2 acc = 1
3 acc = 3
4 acc = 6
5 acc = 1
6 acc = 15


*/
