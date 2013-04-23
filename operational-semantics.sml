structure OperationalSemantics = 
    struct

        exception NoRuleApplies

        structure Syntax = LambdaCore
        open Syntax

        fun     is_val (TmAbs (_, _, _)) = true
            |   is_val _ = false

        fun tnmap onvar c t = 
            let
                fun     walk c (TmVar (x, n)) = onvar c x n
                    |   walk c (TmAbs (x, tyT1, t1)) = 
                            TmAbs (x, tyT1, walk (c+1) t1) 
                    |   walk c (TmApp (t1, t2)) = TmApp (walk c t1, walk c t2)
            in walk c t end

        (* the following function implements the definition 6.2.1 on pag. 79 *)
        fun termShiftAbove d c t = 
            tnmap   (fn c => fn x => fn n =>
                        if x >= c 
                        then TmVar (x+d, n+d)
                        else TmVar (x, n+d))
                    c t

        fun termShift d t = termShiftAbove d 0 t

        fun termSubst j s t = 
            tnmap   (fn c => fn x => fn n =>
                        if x = c (* the book use the condition x = c + j *)
                        then termShift c s
                        else TmVar (x, n))
                    j t

        fun termSubstTop s t =
            termShift ~1 (termSubst 0 (termShift 1 s) t)

        fun     eval_one_step (TmApp ((TmAbs (_, _, t12)), v2)) =
                    if is_val v2                
                    then termSubstTop v2 t12  
                    else raise NoRuleApplies
            |   eval_one_step (TmApp (t1, t2)) =
                    if is_val t1                
                    then 
                        let val t2' = eval_one_step t2 
                        in TmApp (t1, t2') end
                    else
                        let val t1' = eval_one_step t1
                        in TmApp (t1', t2) end
            |   eval_one_step _ = raise NoRuleApplies

        fun eval t = 
            let
                val t' = 
                    SOME (eval_one_step t)
                    handle NoRuleApplies => NONE
            in
                case t' of
                        SOME evaluated_term => eval evaluated_term
                    |   NONE => t
            end
    end
