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
                val SOME input_str = TextIO.inputLine TextIO.stdIn
                val parsed_term = Parser.parse input_str
            in
                (print 
                    (case parsed_term of
                        NONE => "The grammar cannot generate the phrase {" ^ 
                                    input_str ^ "}"
                    |   SOME term => handle_term 
                                        Ctx.emptycontext 
                                        TI.uvargen 
                                        TI.emptyconstr 
                                        term);
                print "\n\n";
                start_repl ())
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

(*        val hard_to_type_check = 
            let val f0 = fn x => (x,x) in
                let val f1 = fn y => f0 (f0 y) in
                    let val f2 = fn y => f1 (f1 y) in
                        let val f3 = fn y => f2 (f2 y) in
                            let val f4 = fn y => f3 (f3 y) in
                                let val f5 = fn y => f4 (f4 y) in
                                    f5 (fn z => z)
                                end
                            end
                        end
                    end
                end
            end*)
        val term_from_extended_example =
            fn x => fn y => fn z => ((x z) (y z))

        (* the following is the term which is used during lecture:
            lambda x:X.(lambda y:Y.(lambda z:Z. ((x z) (y z)))) *)
    end
