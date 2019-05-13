{
  open A3
  exception Not_implemented
}

(*
  Below is a dummy implementation. Please note the following
  - Tokens are defined in A3.mly
  - Return type is token and not token list
  - End of buffer is indicated by EOF token below
  - There is no trailer. The scanner function is written in the wrapper file (test_a4.ml)
  - This is sample implementation. Please rewrite them as per the specifications
*)
let whitespace = [' ' '\t']+
let zero='0'
let digit=['1'-'9']
let digits=['0'-'9']
let integer=('-')?(zero|digit(digits*))
let id=['a'-'z'](['a'-'z']|['A'-'Z'] | digits | '\'' | '_')*
let invalid='0'(digits+)
let bools=['T' 'F']

rule read = parse
whitespace                  {read lexbuf}
|invalid as k               {failwith ("invalid_input, cannot start from 0, in "^(k))}
|integer as n               {INT (int_of_string n) }
|"T"                        {BOOL (true) }
|"F"                        {BOOL (false) }
|"+"                        {PLUS}
|"*"                        {TIMES }
|"("                        {LP }
|")"                        {RP }
|"/\\"                      {CONJ }
|"\\/"                      {DISJ }
|"if"                       {IF }
|"then"                     {THEN }
|"else"                     {ELSE }
|"fi"                       {FI }
|"\\"                       {BACKSLASH}
|"."                        {DOT}
|"cmp"						{CMP}
|id as s                    {(ID s) }
|eof                        {EOF}
|_ as c                     {failwith ("illegal_character,"^(Char.escaped c))}

