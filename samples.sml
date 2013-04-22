
structure MySampleStructure = 
	struct 
		val parsed_term = CalcTest.% `lambda  x:X->Y.( x  x)`
		val SOME term = #1 parsed_term
	end
