
structure MySampleStructure = 
    struct 
        structure Parser = CalcTest
        structure OpSem = OperationalSemantics
        structure PP = PrettyPrinter
        structure Ctx = LambdaContext

        val SOME term_option = Parser.parse "lambda  x:X->Y.( x  x)"
        
        fun handle_term ctx t = 
            let
                val input_representation = 
                    PP.string_of_atomic_term true ctx t

                val evaluated_term = OpSem.eval t
                
                val eval_representation = 
                    PP.string_of_atomic_term true ctx evaluated_term
            in
                "input: " ^ 
                    input_representation ^ "\n" ^
                    "eval: " ^ eval_representation ^ "\n"
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
                    |   SOME term => handle_term Ctx.emptycontext term
            end

        fun describe str= 
            let 
                val SOME parsed_term = Parser.parse str
            in
                print (handle_term Ctx.emptycontext parsed_term)
            end
    end
