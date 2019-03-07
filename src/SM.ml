open GT       
       
(* The type for the stack machine instructions *)
@type insn =
(* binary operator                 *) | BINOP of string
(* put a constant on the stack     *) | CONST of int                 
(* read to stack                   *) | READ
(* write from stack                *) | WRITE
(* load a variable to the stack    *) | LD    of string
(* store a variable from the stack *) | ST    of string with show

(* The type for the stack machine program *)                                                               
type prg = insn list

(* The type for the stack machine configuration: a stack and a configuration from statement
   interpreter
 *)
type config = int list * Syntax.Stmt.config

(* Stack machine interpreter

     val eval : config -> prg -> config

   Takes a configuration and a program, and returns a configuration as a result
 *)        
 let eval_insn config insn = 
	let (stack, stmt_config) = config in
	let (state, input, output) = stmt_config in
	match insn with
		| BINOP operator -> (match stack with
		| y::x::tail -> ([(Syntax.Expr.operator operator) x y]@tail, stmt_config))
		| CONST val -> ([val]@stack, stmt_config)                 
		| READ -> (match input with
		| head::tail -> ([head]@stack, (state, tail, output)))
		| WRITE -> (match stack with
		| head::tail -> (tail, (state, input, output@[head])))
		| LD  value -> ([state value]@stack, stmt_config)
		| ST  value -> (match stack with
		| head::tail -> (tail, (Syntax.Expr.update value head stack, input, output)))

let eval config prg = List.fold_left eval_insn config prg

(* Top-level evaluation

     val run : int list -> prg -> int list

   Takes an input stream, a program, and returns an output stream this program calculates
*)
let run i p = let (_, (_, _, o)) = eval ([], (Syntax.Expr.empty, i, [])) p in o

(* Stack machine compiler

     val compile : Syntax.Stmt.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
 *)

let rec compile_expr exp = match exp with
	| Syntax.Expr.Const const -> [CONST const]
	| Syntax.Expr.Var var -> [LD var]
	| Syntax.Expr.Binop (operator, left, right) -> (compile_expr left)@(compile_expr right)@[BINOP operator];;

let rec compile statement = match statement with
	| Syntax.Stmt.Read value -> [READ; ST value]
	| Syntax.Stmt.Write exp -> (compile_expr exp)@[WRITE]
	| Syntax.Stmt.Assign (value, exp) -> (compile_expr exp)@[ST value]
	| Syntax.Stmt.Seq (st, st_) -> (compile st)@(compile st_);; 
