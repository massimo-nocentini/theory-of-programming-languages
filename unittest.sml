structure UnitTest =
    struct
        structure OpSem = OperationalSemantics
        structure Syntax = LambdaCore
        
        open Syntax

        datatype check_result =
                Pass
            |   Fail of Syntax.term * Syntax.term

        fun term_shifting () = 
                let
                    val input =    TmAbs ("x", TyBool, 
                                        TmAbs ("y", TyNat,
                                            TmApp ( TmVar (1,3),
                                                    TmApp(  TmVar(0, 3),
                                                            TmVar(2, 3)))))
                    val expected =  TmAbs("x", TyBool,
                                        TmAbs("y", TyNat,
                                            TmApp ( TmVar (1, 5),
                                                TmApp ( TmVar(0, 5),
                                                        TmVar(4, 5)))))
                    val actual = OpSem.termShift 2 input
                in
                    if actual = expected
                    then Pass
                    else Fail (expected, actual)
                end

        fun term_substituting () =
                let
                    val t = TmAbs ("x", TyNat,
                                TmApp ( TmVar (1, 3),
                                        TmVar (0, 3)))

                    val s = TmAbs ("y", TyBool, TmVar (0, 3))
                    
                    val expected =  TmAbs ("x",TyNat,TmApp 
                                (TmAbs ("y",TyBool,TmVar (0,4)),
                                        TmVar (0,2)))
                                                    
                    val actual = OpSem.termSubstTop s t 
                in
                    if actual = expected
                    then Pass
                    else Fail (expected, actual)
                end
                
    end
