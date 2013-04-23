structure OperationalSemantics = 
    struct

        exception NoRuleApplies

        fun eval t = 
            let
                val t' = 
                    SOME (eval_one_step t)
                    handle NoRuleApplies => NONE
            in
                case t' of
                    SOME evaluated_term => eval evaluated_term
                    NONE => t
            end

        fun eval_one_step (TmApp (TmAbs _, _, t12), v2) =
                if is_val v2                
                then term_subst_top v2 t12  
                else raise NoRuleApplies
            eval_one_step (TmApp v1, t2) =
                if is_val v1                
                then 
                    let val t2' = eval_one_step t2 
                    in TmApp (v1, t2') end
                else raise NoRuleApplies
            eval_one_step (TmApp t1, t2) =
                let val t1' = eval_one_step t1
                in TmApp (t1', t2) end
            eval_one_step _ = raise NoRuleApplies

        fun is_val (TmAbs (_, _, _)) = true
            is_val _ = false

    end
