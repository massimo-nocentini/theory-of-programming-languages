(* calc.lex
 *
 * COPYRIGHT (c) 2011 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

%name CalcLex;

%let digit = [0-9];
%let int = {digit}+;
%let alpha = [a-zA-Z];
%let id = {alpha}({alpha} | {digit})*;

%defs (
  structure T = CalcParseTokens
  type lex_result = T.token

  fun eof() = T.EOF
);

lambda  => ( T.LAMBDA );
{id}    => ( T.ID (yytext) );
"("     => ( T.LP );
")"     => ( T.RP );
";"	=> ( T.SEMI );
"."	=> ( T.DOT );
" " | \n | \t => ( continue() );
