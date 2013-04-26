structure PrettyPrinter =
    struct
        structure Syntax = LambdaCore
        structure Ctx = LambdaContext 

        open Syntax

        fun     string_of_type outer ty = string_of_arrow_type outer ty
        and     string_of_arrow_type outer (TyArr (tyT1, tyT2)) = 
                    (string_of_atomic_type false tyT1) ^
                    " -> " ^ 
                    (string_of_arrow_type outer tyT2)
            |   string_of_arrow_type outer other_type = 
                    string_of_atomic_type outer other_type
        and     string_of_atomic_type outer TyBool = "bool"
            |   string_of_atomic_type outer TyNat = "nat"
            |   string_of_atomic_type outer (TyId ty) = ty
            |   string_of_atomic_type outer ty = 
                    "(" ^ (string_of_type outer ty) ^ ")"
                    

        fun     string_of_context Ctx.Empty = ""
            |   string_of_context (Ctx.ContainsOneElement (x, _, rest)) =
                    (string_of_context rest) ^ x

        fun     is_small_term (TmVar (_,_)) = true
            |   is_small_term _ = false
        
        fun     string_of_term outer ctx (TmAbs (x, tyT1, t2)) =
                    let
                        val (ctx', x') = Ctx.pickfreshname ctx x
                    in
                        "lambda " ^ x' ^ ":" ^
                            (string_of_type false tyT1) ^ "." ^
                            (if is_small_term t2 andalso (not outer)
                                then "\n" else " ") ^ 
                            string_of_term outer ctx' t2
                    end
            |   string_of_term outer ctx other_term =
                    string_of_app_term outer ctx other_term
        and     string_of_atomic_term outer ctx (TmVar (x, n)) =
                    if Ctx.ctxlength ctx = n 
                    then Ctx.index2name ctx x
                    else "[bad index: " ^ (Int.toString x) ^ "/" 
                            ^ (Int.toString n) ^ " in {"
                            ^ (string_of_context ctx) ^ " }]"
            |   string_of_atomic_term outer ctx other_term =
                    "(" ^ (string_of_term outer ctx other_term) ^ ")"
        and     string_of_app_term outer ctx (TmApp (t1, t2)) = 
                    (string_of_app_term false ctx t1) ^ " " ^
                    (string_of_atomic_term false ctx t2)
            |   string_of_app_term outer ctx other_term = 
                    string_of_atomic_term outer ctx other_term

    end
