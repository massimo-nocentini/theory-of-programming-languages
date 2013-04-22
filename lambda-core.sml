structure LambdaCore = 
	struct
		datatype ty =
			  TyArr of ty * ty
			| TyId of string	
		datatype term = 
			  TmVar of string
			| TmAbs of string * term
			| TmApp of term * term
	end
