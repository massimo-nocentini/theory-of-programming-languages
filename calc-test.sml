(* calc-test.sml
 *
 * COPYRIGHT (c) 2011 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure CalcTest = 
  struct

    structure Tok = CalcParseTokens

    structure ListLex = struct
      type strm = Tok.token list
      fun lex [] = (Tok.EOF, (0, 0), [])
	| lex (t::ts) = (t, (0, 0), ts)
      type pos = AntlrStreamPos.pos
      type span = pos * pos
      fun getPos _ = 0
    end

    structure CP = CalcParseFn(ListLex)

    fun fragToToks (SMLofNJ.QUOTE s) = let
          val sref = ref true
          fun input _ = if !sref then
			  (sref := false; s)
			else ""
	  val lex = CalcLex.lex (AntlrStreamPos.mkSourcemap())
          fun loop ((Tok.EOF, _, _), accum) = rev accum
	    | loop ((s, _, strm), accum) = loop (lex strm, s::accum)
          in
            loop (lex (CalcLex.streamify input), [])
          end
      | fragToToks (SMLofNJ.ANTIQUOTE i) = [Tok.DummyExp i]

    fun % frags = let
      val (r, s', errs) = CP.parseTerm 
				ListLex.lex 
				LambdaContext.emptycontext 
				(List.concat (map fragToToks frags))
    in
      app (fn (pos, repair) => print (AntlrRepair.actionToString Tok.toString repair ^ "\n")) errs;
      (r, s')
    end

	(* The following function is the ``entry point'' to invoke the parsing
	   on a given string (which may be passed after reading from console
	   or opening a file. It should be extended to consume a list of
	   strings as argument instead of a single one as in this version. *)
	fun parse str = 
		let
			val parsed_pair_info = % [SMLofNJ.QUOTE str]
		in
			#1 parsed_pair_info
		end

(*    val _ = Control.quotation := true *)

  end
