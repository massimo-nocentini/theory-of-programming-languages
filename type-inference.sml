structure TypeInference = 
    struct
        structure Parser = CalcTest
        structure Syntax = LambdaCore
        structure Ctx = LambdaContext

        open Syntax

        exception inference_exception of string

        type constr = (Syntax.ty * Syntax.ty) list

        datatype nextuvar = NextUVar of string * uvargenerator
            withtype uvargenerator = unit -> nextuvar

        val emptyconstr : constr = []

        local
            fun f n () = NextUVar ("?X" ^ (Int.toString n), f (n+1))
        in
            fun uvargen () = f 0
        end

        fun     recon ctx nextuvar (TmVar (x, n)) = 
                    let val tyT = Ctx.get_type_from_context ctx x
                    in (tyT, nextuvar, []) end
            |   recon ctx nextuvar (TmAbs (x, tyT1, t2)) =
                    let
                        val ctx' = Ctx.addbinding ctx x (Ctx.VarBind tyT1)
                        val (tyT2, nextuvar2, constr2) = 
                            recon ctx' nextuvar t2 
                    in (TyArr (tyT1, tyT2), nextuvar2, constr2) end
            |   recon ctx nextuvar (TmApp (t1, t2)) =
                    let
                        val (tyT1, nextuvar1, constr1) = 
                            recon ctx nextuvar t1

                        val (tyT2, nextuvar2, constr2) = 
                            recon ctx nextuvar1 t2

                        val NextUVar (tyX, nextuvar') = nextuvar2 () 

                        val newconstr : constr = [ (tyT1, TyArr (tyT2, TyId tyX))]
                    in  
                        (   TyId tyX, 
                            nextuvar', 
                            List.concat ([newconstr, constr1, constr2]))
                    end

        val combineconstr =  List.@

        fun substinty tyX tyT tyS = 
            let
                fun     f (TyArr (tyS1, tyS2)) = TyArr (f tyS1, f tyS2)
                    |   f TyNat = TyNat
                    |   f TyBool = TyBool
                    |   f (tyId as (TyId s)) = 
                            if s = tyX then tyT else tyId
            in f tyS end

        fun substinconstr tyX tyT constr = 
            List.map (  fn (tyS1, tyS2) => (
                            substinty tyX tyT tyS1,
                            substinty tyX tyT tyS2))
                        constr

        fun occursin tyX tyT = 
            let
                fun     occ (TyArr (tyT1, tyT2)) = occ tyT1 orelse occ tyT2
                    |   occ TyNat = false
                    |   occ TyBool = false
                    |   occ (TyId s) = s = tyX
            in occ tyT end

        fun unify ctx msg constr = 
            let
                fun     u ([] : constr) : constr = [] 
                    |   u ((tyS, tyId as (TyId tyX)) :: rest)  =
                            if tyS = tyId
                            then u rest else 
                                if occursin tyX tyS 
                                then raise inference_exception 
                                        (msg ^ ": circular constraints")
                                else List.@ (
                                        u (substinconstr tyX tyS rest),
                                        [(tyId, tyS)])                                
                    |   u ((tyId as (TyId tyX), tyT) :: rest) =
                            if tyT = tyId
                            then u rest else
                                if occursin tyX tyT
                                then raise inference_exception 
                                        (msg ^ ": circular constraints")
                                else List.@ (
                                        u (substinconstr tyX tyT rest),
                                        [(tyId, tyT)])
                    |   u ((TyNat, TyNat) :: rest) = u rest
                    |   u ((TyBool, TyBool) :: rest) = u rest
                    |   u ((TyArr (tyS1, tyS2), TyArr (tyT1, tyT2)) :: rest) = 
                            u ((tyS1, tyT1) :: (tyS2, tyT2) :: rest)
                    |   u ((tyS, TyT) :: rest) =
                            raise inference_exception "Unsolvable constraints"
            in u constr end
        
        (* foldl f init [x1, x2, ..., xn] *)
        fun applysubst (constr : constr) (tyT:Syntax.ty) : Syntax.ty = 
            List.foldl  (fn (ty_pair, tyS) =>
                            case ty_pair of
                                    (TyId tyX, tyC2) => substinty tyX tyC2 tyS
                                |   other => raise inference_exception
                                                ("applysubst: not defined for " ^
                                                "pair of types with first component " ^
                                                "different from (TyId s)")) 
                        tyT
                        (List.rev constr)

    end
