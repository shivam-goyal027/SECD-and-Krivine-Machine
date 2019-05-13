#directory "_build";; (* Consider this folder when looking for files *)
#load "a5.cmo";;
#load "a2.cmo";;
#load "a3.cmo";;
open A5;;
open A2;;
open A3;;



let exp_parser s = A3.exp_parser A2.read (Lexing.from_string s) ;;


let t="\\x.(x+(\\x.(x*5)(2)))(9) + \\y.(y+5)(10)";;
let p=exp_parser t;;
let q=krivine p [] [];;
let r=secd [] [] (compile p) [];;

let t="\\x.(x+5)(2) + \\y.(y+5)(10) * \\x.(x+5)(2*2) + \\z.(10)(2)";;
let p=exp_parser t;;
let q=krivine p [] [];;
let r=secd [] [] (compile p) [];;

let t="if cmp(5) then 5+5 else 4+5 fi";;
let p=exp_parser t;;
let q=krivine p [] [];;
let r=secd [] [] (compile p) [];;

let t="if cmp(-1) then F\\/ cmp(-2) else T/\\cmp(0) fi";;
let p=exp_parser t;;
let q=krivine p [] [];;
let r=secd [] [] (compile p) [];;

let t="10+if cmp(-1) then F\\/ cmp(-2) else if T/\\cmp(1) then  \\x.x(10) else \\y.y(9) fi fi";;
let p=exp_parser t;;
let q=krivine p [] [];;
let r=secd [] [] (compile p) [];;




