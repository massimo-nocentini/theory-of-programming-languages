(* calc.g
 *
 * COPYRIGHT (c) 2011 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

%name CalcParse;

(* an example of using the header declaration, although it
 * just describes the default behavior.
 *)
%header (functor CalcParseFn (Lex : ANTLR_LEXER));

%entry Term;

(* The following tokens are the smallest set of tokens
   necessary to implement the pure lambda calculus *)
%tokens
  : ID of string
  | LP       ("(")   | RP      	(")")
  | SEMI     (";")   | LAMBDA	("lambda")
  | DOT	     (".")   | DummyExp of LambdaCore.term
  ;


prog
  : (Term@(AtomMap.empty) ";")*
  ;
Term(env) :
   (* ATerm@(env)*)
   AppTerm@(env)
  (*| IF Term THEN Term ELSE Term
      { fun ctx -> TmIf($1, $2 ctx, $4 ctx, $6 ctx) }*)
  | LAMBDA ID DOT Term@(env) => (LambdaCore.TmAbs (ID, Term))
  ;
AppTerm(env) :
  (* | SUCC ATerm
      { fun ctx -> TmSucc($1, $2 ctx) }
  | PRED ATerm
      { fun ctx -> TmPred($1, $2 ctx) }
  | ISZERO ATerm
      { fun ctx -> TmIsZero($1, $2 ctx) } *)
   ATerm@(env) Term@(env) => 
		(
			let
				val e1 = ATerm
				val e2 = Term
			in
				LambdaCore.TmApp(e1, e2)
			end
		)
  ;
ATerm(env) :
    "(" Term@(env) ")"
(*  | TRUE
      { fun ctx -> TmTrue($1) }
  | FALSE
      { fun ctx -> TmFalse($1) }
  | INTV
      { fun ctx ->
          let rec f n = match n with
              0 -> TmZero($1.i)
            | n -> TmSucc($1.i, f (n-1))
          in f $1.v }
          TmVar($1.i, name2index $1.i ctx $1.v, ctxlength ctx) }
  | NUM
      => ( nums := NUM::(!nums); NUM ) *)
  | ID => ( LambdaCore.TmVar ID)
  | DummyExp
  ;
