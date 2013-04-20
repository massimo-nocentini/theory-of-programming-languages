structure LambdaCore = 
	struct
		datatype term = 
			  TmVar of string
			| TmAbs of string * term
			| TmApp of term * term
	end
