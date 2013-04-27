structure MySampleStructure = 
    struct 
        structure Parser = CalcTest
        structure OpSem = OperationalSemantics
        structure PP = PrettyPrinter
        structure Ctx = LambdaContext
        structure TI = TypeInference

        val SOME term_option = Parser.parse "lambda  x:X->Y.( x  x)"
        val SOME constraing_main_example = Parser.parse 
            "lambda x:X.(lambda y:Y.(lambda z:Z.((x z) (y z))))"
        
        fun handle_term ctx nextuvar constr t = 
            let
                val input_representation = 
                    PP.string_of_atomic_term true ctx t

                val evaluated_term = OpSem.eval t
                
                val eval_representation = 
                    PP.string_of_atomic_term true ctx evaluated_term

                val (tyT, nextuvar', constr_t) = 
                    TI.recon ctx nextuvar t

                val constr' = TI.combineconstr (constr, constr_t)

                val constr'' = TI.unify ctx 
                                        "Could not simplify constraints"
                                        constr'
                
                val evaluation_information = 
                    (ctx, nextuvar', constr'') 
            in
                "input: " ^ 
                    input_representation ^ "\n" ^
                    "eval: " ^ eval_representation ^ "\n" ^
                    ": " ^ (PP.string_of_type_top (TI.applysubst constr'' tyT))
            end

        (* Add here a function that implement a ``mini'' repl, 
           when the user inputs a string call the parser to obtain
           the term and pass it to the type reconstructor to infer
           the type *)
        fun start_repl () = 
            let 
                val input_str = ""
                val parsed_term = Parser.parse input_str
            in
                case parsed_term of
                        NONE => "The grammar cannot generate the phrase {" ^ 
                                    input_str ^ "}"
                    |   SOME term => handle_term 
                                        Ctx.emptycontext 
                                        TI.uvargen 
                                        TI.emptyconstr 
                                        term
            end

        fun describe str= 
            let 
                val SOME parsed_term = Parser.parse str
            in
                print (handle_term 
                            Ctx.emptycontext 
                            TI.uvargen 
                            TI.emptyconstr 
                            parsed_term)
            end
    end
