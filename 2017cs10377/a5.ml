exception Type_Error
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
let rec typecheck (e:expr) (g) : expr_types=(match e with
					V(x)->(match g with 
							[]->raise Type_Error	
							|_->(List.hd (g)))
					| Lambda(e1,e2)->(match e1 with V(y) ->(Tfunc((typecheck e1 g),(typecheck e2 g)))
										|_->raise Type_Error) 
									| App(e1,e2)->(match (typecheck e1 ((typecheck e2 g)::g)) with
									Tfunc(t1,t2)->(if ((typecheck e2 g)=t1) then (t2) else (raise Type_Error) )
									|_->raise Type_Error) 
					| Plus(e1,e2)->(match ((typecheck e1 g),(typecheck e2 g)) with
									(Tint,Tint)->Tint
									|_->raise Type_Error )  
					| Mult(e1,e2)->(match ((typecheck e1 g),(typecheck e2 g)) with
									(Tint,Tint)->Tint
									|_->raise Type_Error ) 
					| And(e1,e2)->(match ((typecheck e1 g),(typecheck e2 g)) with
									(Tbool,Tbool)->Tbool
									|_->raise Type_Error ) 
					| Or(e1,e2)->(match ((typecheck e1 g),(typecheck e2 g)) with
									(Tbool,Tbool)->Tbool
									|_->raise Type_Error )
					| Bool(b)->Tbool 
					| Integer(n)->Tint 
					| Cmp(e1)->(match (typecheck e1 g) with
									(Tint)->Tbool
									|_->raise Type_Error )  
					| If_Then_Else(e1,e2,e3)->(match (typecheck e1 g ) with
									Tbool->(if ((typecheck e2 g)=(typecheck e3 g)) then (typecheck e2 g) else raise Type_Error )
									|_->raise Type_Error) 
			)

let typechecker (e:expr):bool=try(
	match Checker(typecheck e []) with Checker(t)->true
								|_->false
)with Type_Error->false
let rec find x e=(match e with 
	[]->failwith "not implemented"
	|(a,b)::ys->if (a=x) then b else (find x ys)
	)

let rec compile (e:expr) :opcode list=(match e with
						V(x)->[VAR(x)]
						|Bool(b)->[BOOL(b)]
						|Integer(n)->[INT(n)]
						|Cmp(e1)->(compile e1)@[COMPARE]
						|If_Then_Else(e1,e2,e3)->(compile e1)@[COND((compile e2),(compile e3))]
						|Plus(e1,e2)->(compile e1)@(compile e2)@[ADD]
						|Mult(e1,e2)->(compile e1)@(compile e2)@[MULT]
						|And(e1,e2)->(compile e1)@(compile e2)@[AND]
						|Or(e1,e2)->(compile e1)@(compile e2)@[OR]
						|Lambda(e1,e2)->[CLOS((compile e1),(compile e2)@[RET])]
						|App(e1,e2)->(compile e1)@(compile e2)@[APP]
						)

let rec secd (s:answers list) (e:(string * answers) list) (c:opcode list) (d:dump) :answers=(match c with
						[]->(match s with
							[]->failwith "not implemented"
							|x::xs->x
							)
						|VAR(x)::xs->(secd ((find x e) ::s) e xs d ) (*find x e is an answer either Num(a) or Boolean(b) but not a closure*)
						|BOOL(b)::xs->(secd (Boolean(b)::s) e xs d ) (*Boolean(b) is an answer type*)
						|INT(n)::xs->(secd (Num(n)::s) e xs d) (*Num(n) is an answer type*)
						|COMPARE::xs->(match s with
							[]->failwith "not implemented"
							|y::ys->(match y with 
								Num(n)->(if (n>0) then (secd (Boolean(true)::ys) e xs d) else (secd (Boolean(false)::ys) e xs d))
								|_->failwith "not implemented"
								)
							)
						|COND(c1,c2)::xs->(match s with
							[]->failwith "not implemented"
							|y::ys->(match y with 
								Boolean(b)->(if (b=true) then (secd (ys) e (c1@xs) d) else (secd (ys) e (c2@xs) d))
								|_->failwith "not implemented"
								)
							)
						|ADD::xs->(match s with
							[]->failwith "not implemented"
							|z::[]->failwith "not implemented"
							|y1::y2::ys->(match (y1,y2) with 
								(Num(n1),Num(n2))->(secd (Num(n1+n2)::ys) e xs d)
								|_->failwith "not implemented"
								)
							)
						|MULT::xs->(match s with
							[]->failwith "not implemented"
							|z::[]->failwith "not implemented"
							|y1::y2::ys->(match (y1,y2) with 
								(Num(n1),Num(n2))->(secd (Num(n1*n2)::ys) e xs d)
								|_->failwith "not implemented"
								)
							)
						|AND::xs->(match s with
							[]->failwith "not implemented"
							|z::[]->failwith "not implemented"
							|y1::y2::ys->(match (y1,y2) with 
								(Boolean(b1),Boolean(b2))->(secd (Boolean(b1 && b2)::ys) e xs d)
								|_->failwith "not implemented"
								)
							)
						|OR::xs->(match s with
							[]->failwith "not implemented"
							|z::[]->failwith "not implemented"
							|y1::y2::ys->(match (y1,y2) with 
								(Boolean(b1),Boolean(b2))->(secd (Boolean(b1 || b2)::ys) e xs d)
								|_->failwith "not implemented"
								)
							)
						|CLOS(c1,c2)::xs->(match c1 with
										[VAR(x)]->(secd (Closure([VAR(x)],c2,e)::s) e xs d)
										|_->failwith "not implemented"
										)
						|APP::xs->(match s with
							[]->failwith "not implemented"
							|z::[]->failwith "not implemented"
							|y1::y2::ys->(match y2 with
											Closure(a1,b1,c1)->(match a1 with [VAR(x)]->(secd [] ((x,y1)::c1) b1 ((ys,e,xs)::d) ))
											|_->failwith "not implemented")
							 )
						|RET::xs->(match s with
									[]->failwith "not implemented"
									|y::ys->(match d with
										[]->failwith "not implemented"
										|(a1,b1,c1)::zs->(secd (y::a1) b1 c1 zs)
										|_->failwith "not implemented"
										)
									)
					)
let rec look e g s=(match g with
	[]->failwith "not implemented"
	|(a,b)::ys->if (a=e) then (match b with (Closed(CLOSER(r,p)))-> (krivine r p s)) else (look e ys s)
	)
and krivine (e:expr) (g:(expr * valo) list) (s:stk) : valo = (match e with
V(x)-> (look e g s) (*will return a closure*)
									
									|App(e1,e2)->(krivine e1 g (((e2,g))::s))
									|Plus(e1,e2)->((krivine e1 g ((Plus(Integer(0),Integer(0)),g)::((e2,g)::s))))
											
									|Mult(e1,e2)->((krivine e1 g ((Mult(Integer(0),Integer(0)),g)::((e2,g)::s))))
									|And(e1,e2)->((krivine e1 g ((And(Bool(true),Bool(true)),g)::((e2,g)::s))))
									|Or(e1,e2)->((krivine e1 g ((Or(Bool(true),Bool(true)),g)::((e2,g)::s))))
									|Bool(b)->(match s with
											(((And(Bool(true),Bool(true)),g1)::((e2,g2)::xs)))->(match e2 with
																	Bool(b2)->(krivine (Bool(b && b2)) g xs)
															|_->(krivine e2 g2 ((And(Bool(true),Bool(true)),g)::((Bool(b),g)::xs))))
										|(((Or(Bool(true),Bool(true)),g1)::((e2,g2)::xs)))->(match e2 with
																	Bool(b2)->(krivine (Bool(b || b2)) g xs)
															|_->(krivine e2 g2 ((Or(Bool(true),Bool(true)),g)::((Bool(b),g)::xs))))
							|(((If_Then_Else(Bool(true),Integer(0),Integer(0))),g1)::((e2,g2)::((e3,g3)::xs)))->if (b=true) then (krivine e2 g2 xs) else (krivine e3 g3 xs)
								|(((Cmp(Integer(0)),g1))::xs)->failwith "not implemented"
								|_->(Closed(Bconst(b,g)))
									)
									

									|Integer(n)->(match s with
										(((Plus(Integer(0),Integer(0)),g1)::((e2,g2)::xs)))->(match e2 with
																	Integer(n2)->(krivine (Integer(n+n2)) g xs)
													|_->(krivine e2 g2 ((Plus(Integer(0),Integer(0)),g)::((Integer(n),g)::xs))))
										|(((Mult(Integer(0),Integer(0)),g1)::((e2,g2)::xs)))->(match e2 with
																	Integer(n2)->(krivine (Integer(n*n2)) g xs)
															|_->(krivine e2 g2 ((Mult(Integer(0),Integer(0)),g)::((Integer(n),g)::xs))))
										|(Cmp(Integer(0)),g1)::xs->(krivine (Bool(n>0)) g1 xs)
										|(((If_Then_Else(Bool(true),Integer(0),Integer(0))),g1)::((e2,g2)::((e3,g3)::xs)))->failwith "not implemented"
										|_->(Closed(Iconst(n,g)))
									)
								|Cmp(e1)->(krivine e1 g ((Cmp(Integer(0)),g)::s))
								|If_Then_Else(e1,e2,e3)->(krivine e1 g (((If_Then_Else(Bool(true),Integer(0),Integer(0))),g)::((e2,g)::((e3,g)::s))))
								|Lambda(e1,e2)->(match e1 with
													V(y)->(match s with
														[]->Closed(CLOSER(e,g))
														|(x::xs)->(krivine e2 ((V(y),Closed(CLOSER(x)))::g) xs)
													)
													|_->failwith "not implemented"
												)
									)


let p1 =  App (Lambda (V "x", Mult (Integer 3, V "x")), Integer 4);;
let p2 = If_Then_Else
   (Cmp (Integer 7),
    App (Lambda (V "x", Plus (Integer 3, V "x")), Integer 31), 
    Integer 0);;
let p3 = If_Then_Else
    (Cmp (Integer 0),
    App (Lambda (V "x", Plus (Integer 3, V "x")), Integer 4),
        Integer 110);;

let q= typechecker p1;;
let r= typechecker p2;;
let s= typechecker p3;;
let t= typechecker (Mult(Integer 3, Bool true));;

