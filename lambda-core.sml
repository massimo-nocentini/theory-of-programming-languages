structure LambdaCore = 
	struct
		datatype ty =
			  TyArr of ty * ty
			| TyId of string	
			| TyBool
			| TyNat
		datatype term = 
			  TmVar of int * int
			| TmAbs of string * ty * term
			| TmApp of term * term
	end
