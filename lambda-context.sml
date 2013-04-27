structure LambdaContext = 
    struct
        exception context_exception of string
        structure Syntax = LambdaCore
        datatype binding =
                  NameBind 
                | VarBind of Syntax.ty
        
        datatype context = 
                  Empty
                | ContainsOneElement of string * binding * context

        val emptycontext = Empty 

        fun ctxlength Empty = 0
          | ctxlength (ContainsOneElement (_,_,rest)) = 1 + (ctxlength rest)

        fun addbinding ctx x bind = ContainsOneElement (x, bind, ctx)

        fun addname ctx x = addbinding ctx x NameBind

        fun isnamebound Empty x = false
          | isnamebound (ContainsOneElement (y,_,rest)) x = 
                if y=x then true
                else isnamebound rest x
        
        fun pickfreshname ctx x =
          if isnamebound ctx x 
          then pickfreshname ctx (x ^ "'")
          else (addname ctx x, x) 

        fun index2name (ContainsOneElement (x, _, _)) 0 = x
          | index2name (ContainsOneElement (_, _, rest)) n =
                index2name rest (n-1) 
          | index2name _ n = raise context_exception ("index2name: wrong index"
                                                        ^ (Int.toString n))

        fun name2index Empty x = 
                raise context_exception 
                        ("name2index: Identifier " ^ x ^ " is unbound")
          | name2index (ContainsOneElement (y, _, rest)) x =
                if y=x then 0
                else 1 + (name2index rest x)
        
        fun     get_binding (ContainsOneElement (_, binding, _)) 0 = binding
            |   get_binding (ContainsOneElement (_, _, rest)) n = 
                    get_binding rest (n - 1)
            |   get_binding ctx n =
                    raise context_exception ("Fail to find binding for variable " 
                            ^ (Int.toString n)) 

        fun get_type_from_context ctx i = 
                case get_binding ctx i of
                        VarBind tyT => tyT
                    |   _ => raise context_exception ("get_type_from_context: " ^
                                "wrong kind of binding for variable" ^ 
                                (Int.toString i))
    end
