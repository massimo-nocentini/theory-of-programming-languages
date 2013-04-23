
structure MySampleStructure = 
    struct 
        structure Parser = CalcTest
        structure OpSem = OperationalSemantics

        val parsed_term = Parser.parse "lambda  x:X->Y.( x  x)"
        val SOME term = parsed_term
        
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
                        NONE => ""
                    |   SOME term => 
                            let
                                val evaluated_term = OpSem.eval term
                            in
                                "" (* temporary we return the empty string *)
                            end
            end
    end
