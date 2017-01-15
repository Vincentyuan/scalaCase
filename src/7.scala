def decode(list:list[(Int, String)]) :List[String] = list match{
	list.flatMap(case(n,c) =>(1 to n).map(x=>c))
}