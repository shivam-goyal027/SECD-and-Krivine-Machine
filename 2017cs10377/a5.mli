type expr = V of string 
		| Lambda of (expr * expr) 
		| App of (expr * expr) 
		| Plus of (expr * expr) 
		| Mult of (expr * expr) 
		| And of (expr * expr) 
		| Or of (expr * expr) 
		| Bool of bool 
		| Integer of int 
		| Cmp of expr 
		| If_Then_Else of (expr * expr * expr)

type opcode= VAR of string | BOOL of bool | INT of int | COMPARE | COND of (opcode list * opcode list) | ADD | MULT | AND | OR | CLOS of (opcode list* opcode list) | RET | APP
type closures = Bconst of (bool* (expr * valo) list) | Iconst of (int* (expr * valo) list) | CLOSER of (expr * (expr * valo) list)
and valo= Closed of closures 
(*type closures=(expr * (string * valo) list)*)
and answers= Boolean of bool | Num of int | Closure of (opcode list * opcode list * (string * answers) list)
type dump=(answers list * (string * answers) list * opcode list) list
type stk=(expr * (expr * valo) list) list

type expr_types=Tint | Tbool | Tfunc of (expr_types * expr_types)
type checker=Checker of expr_types

val typecheck : expr-> expr_types list -> expr_types
val typechecker : expr-> bool
val compile: expr-> opcode list
val secd: answers list-> (string * answers) list-> opcode list-> dump-> answers
val krivine: expr-> (expr * valo) list-> stk-> valo 

