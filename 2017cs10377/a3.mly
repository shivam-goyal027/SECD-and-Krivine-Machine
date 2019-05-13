%{
    open A5
%}

/*
- Tokens (token name and rules) are modified wrt to A2. Please make necessary changes in A3
- LP and RP are left and right parenthesis
- Write grammar rules to recognize
  - >= <= from GT EQ LT tokens
  - if then else fi
*/
/* Tokens are defined below.  */
%token <int> INT
%token <bool> BOOL
%token <string> ID
%token TILDA PLUS TIMES CONJ DISJ LP RP IF THEN ELSE FI BACKSLASH DOT CMP EOF 
%start exp_parser
%type <A5.expr> exp_parser /* Returns expression */
%%
/* The grammars written below are dummy. Please rewrite it as per the specifications. */

/* Implement the grammar rules for expressions, which may use the parser for definitions */

/* Implement the grammar rules for definitions, which may use the parser for expression  */

exp_parser:
	main_pre EOF								{$1}
;
main_pre:
	disj										{$1}
;
disj:
	disj DISJ conj								{Or($1,$3)}
	|conj										{$1}
;
conj:
	conj CONJ compare							{And($1,$3)}
	|compare									{$1}
;	
compare:
	CMP LP main_pre RP							{Cmp($3)}
	|add										{$1}
;
add:
	add PLUS mult								{Plus($1,$3)}
	|mult										{$1}
;
mult:
	ifte TIMES mult								{Mult($1,$3)}
	|ifte										{$1}
;
ifte:
	IF main_pre THEN main_pre ELSE main_pre FI	{If_Then_Else($2,$4,$6)}
	|func_call									{$1}
;
func_call:
	func_call LP main_pre RP					{App($1,$3)}
	|func_abs									{$1}
;
func_abs:
	BACKSLASH constant DOT func_abs					{Lambda($2,$4)}
	|constant									{$1}
;
constant:
	ID											{V($1)}
	|INT										{Integer($1)}
	|BOOL										{Bool($1)}
	|LP main_pre RP									{$2}
; 



