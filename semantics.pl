% Parse Tree Generator
%:- use_rendering(svgtree).

%:- use_module(library(tabling)).
:- table expr_op/3, term/3, bool/3.

% Update Environment
update(t_id(K), Type, Env, FinalEnv) :- updte(K, Type, Env, FinalEnv).
updte(K, Type, [], [(K,Type)]).
updte(K, Type, [(K, _)|T], [(K, Type)|T]).
updte(K, Type, [H|T], [H|R]) :- H \= (K,_), updte(K, Type, T, R).


% Lookup Value in Environment
lookup(t_id(K), Type, Env) :- look_up(K, Type, Env).
look_up(_K, _Type, []) :- fail.
look_up(K, Type, [(K,Type)|_T]).
look_up(K1, Type, [(K2,_T2)|T]) :- K1 \= K2, look_up(K1, Type, T).


% Expressions
expr(t_assign(X, Y)) --> identifier(X), [=], expr_op(Y).
expr(X) --> expr_op(X).

expr_op(t_add(X, Y))-->expr_op(X), [+], term(Y).
expr_op(t_sub(X, Y))-->expr_op(X), [-], term(Y).
expr_op(X) --> term(X).

term(t_div(X, Y))-->term(X), [/], brackets(Y).
term(t_mul(X, Y)) --> term(X), [*], brackets(Y).
term(X) --> brackets(X).

brackets(X) --> ['('], expr(X), [')'].
brackets(X) --> num(X).
brackets(X) --> identifier(X).

identifier(t_id(X)) -->[X],{X \= true}, {X \= false}, {atom(X)}.
num(t_num(X)) --> [X], {number(X)}.

% Boolean Operators
boolean_operator(t_bool_op_and(and))  --> [and].
boolean_operator(t_bool_op_or(or))  --> [or].

% Boolean Operations
bool(t_bool(X,Y,Z)) --> expr(X), comparison_operator(Y), expr(Z).
bool(t_bool_operation(X,Y,Z)) --> bool(X), boolean_operator(Y), boolean(Z).
bool(X) --> boolean(X).
boolean(t_notbool(not, X)) --> [not], bool(X).
boolean(X) --> identifier(X).
boolean(true) --> [true].
boolean(false) --> [false].
boolean(X) --> brkt_bool(X).
brkt_bool(X)-->['('], bool(X), [')'].

% Need to redefine not equal to operator
comparison_operator(t_comp_op(>)) --> [>].
comparison_operator(t_comp_op(<)) --> [<].
comparison_operator(t_comp_op(==)) --> [==].
comparison_operator(t_comp_op(<=)) --> [<=].
comparison_operator(t_comp_op(>=)) --> [>=].
comparison_operator(t_comp_op(=\=)) --> ["!="].

% Ternary Operation
ternary_op(t_ternary(X, Y, Z)) --> bool(X), [?], expr(Y), [:], expr(Z).

% Declaration statements
declaration(Env, FinalEnv, t_declaration_bool_assign(X, Y)) --> [boolean], identifier(X), [=], bool(Y), {update(X, bool, Env, FinalEnv)}.
declaration(Env, FinalEnv, t_declaration_bool_assign(X)) --> [boolean], identifier(X), {update(X, bool, Env, FinalEnv)}.
declaration(Env, FinalEnv, t_declaration_str_assign(X, Y)) --> [string], identifier(X), [=], [Y], {string(Y)}, {update(X, str, Env, FinalEnv)}.
declaration(Env, FinalEnv, t_declaration_str_assign(X)) --> [string], identifier(X),{update(X, str, Env, FinalEnv)}.
declaration(Env, FinalEnv, t_declaration_num_assign(X, Y)) --> [num], identifier(X), [=], expr(Y), {update(X, num, Env, FinalEnv)}.
declaration(Env, FinalEnv, t_declaration_num_assign(X)) --> [num], identifier(X), {update(X, num, Env, FinalEnv)}.
declaration(Env, FinalEnv, t_declaration_num_assign_ternary(X, Y)) --> [num], identifier(X), [=], ternary_op(Y), {update(X, num, Env, FinalEnv)}.

% Assignment statements
assignment(Env, Env, t_assignment_num_assign(X, Y)) --> identifier(X), {lookup(X, num, Env)},[=], expr(Y).
assignment(Env, Env, t_assignment_num_assign_ternary(X, Y)) --> identifier(X), {lookup(X, num, Env)}, [=], ternary_op(Y).
assignment(Env, Env, t_assignment_bool(X, Y)) --> identifier(X), {lookup(X, bool, Env)}, [=], bool(Y).
assignment(Env, Env, t_assignment_str(X, Y)) --> identifier(X), {lookup(X, str, Env)},[=], ['"'], [Y], ['"'].


% Need to implement print statement
eprintv(Env, Env, t_print()) --> [].
eprintv(Env, FinalEnv, X) --> [,], printv(Env, FinalEnv, X).
printv(Env, FinalEnv, t_print(X, Y)) --> [X], {string(X)}, eprintv(Env, FinalEnv, Y).
% Check for bool and string
printv(Env, FinalEnv, t_print_id(X, Y)) --> identifier(X), eprintv(Env, FinalEnv, Y).

% if else statements
if_stmt(Env, t_ifstmt(X, Y, Z)) --> [if], ['('], bool(X), [')'], ['{'], command(Env, _, Y), ['}'], elif_stmt(Env, Z).

elif_stmt(Env, t_elifstmt(X, Y, Z)) --> [elif], ['('], bool(X), [')'], ['{'], command(Env, _, Y), ['}'], elif_stmt(Env, Z).
elif_stmt(Env, t_goto_else_stmt(X)) --> else_stmt(Env, X).

else_stmt(Env, t_elsestmt(X)) --> [else], ['{'], command(Env, _, X), ['}'].
else_stmt(_, t_elsestmt()) --> [].

% for loops
conventional_for(Env, t_conventional_for(A,B,C,D,E,F)) --> [for], ['('], identifier(A), [=], expr(B), [;], 
    identifier(A), comparison_operator(C), expr(D), [;], 
    identifier(A), [=], expr(E), [')'], ['{'], command(Env, _, F), ['}'].

new_for(Env, t_new_for(A,B,C,D)) --> [for], identifier(A), [in], 
    [range], ['('], expr(B), [,], expr(C), [')'], ['{'], command(Env, _, D), ['}'].

% General Statements and While loop
statement(Env, FinalEnv, t_statement_declaration(X)) --> declaration(Env, FinalEnv, X).
statement(Env, FinalEnv, t_statement_assign(X)) --> assignment(Env, FinalEnv, X).
statement(Env, FinalEnv, t_statement_print(X)) --> [print], ['('] , printv(Env, FinalEnv, X), [')'].
statement(Env, Env, t_statement_ifelse(X)) --> if_stmt(Env, X).
statement(Env, Env, t_statement_while(X, Y)) --> [while], ['('], bool(X), [')'], ['{'], command(Env, _, Y), ['}'].
statement(Env, Env, t_statement_for(X)) --> conventional_for(Env, X).
statement(Env, Env, t_statement_for(X)) --> new_for(Env, X).

% Command List and single command is called statement.
command(Env, FinalEnv, t_command(X, Y)) --> statement(Env, Env1, X), command(Env1, FinalEnv, Y).
command(Env, Env, t_command()) --> [].

% Block.
block(t_block(X))-->command([], _, X).

% Program entr point. Will take input as list of tokens and generate parse tree.
program(t_program(X))-->block(X).







% Runtime Semantics

% Update Environment
update(t_id(K), V, Type, Env, FinalEnv) :- update(K, V, Type, Env, FinalEnv).
update(K, V, Type, [], [(K, V, Type)]).
update(K, V, Type, [(K, _, _)|T], [(K, V, Type)|T]).
update(K, V, Type, [H|T], [H|R]) :- H \= (K,_,_), update(K, V, Type, T, R).



% Lookup Value in Environment
lookup(t_id(K), Env, V, Type) :- lookup(K, Env, V, Type).
lookup(K, [(K,V,Type)|_], V, Type).
lookup(K, [_|T], V, Type) :- lookup(K, T, V, Type).



% Check if an identifier is present in env.
check_present(t_id(K),Env) :- check_present(K, Env).
check_present(K,[(K,_,_)|_]).
check_present(K,[(H,_,_)|T]) :- K \= H, check_present(K,T).



% Evaluate Expression
eval_expr(t_assign(t_id(X), Y), Env, FinalEnv, Val):- check_present(X, Env),
    eval_expr(Y, Env, Env1, Val), update(X, Val, num, Env1, FinalEnv).

eval_expr(t_assign(t_id(X), _Y), Env, _FinalEnv, _Val):- \+check_present(X, Env),
    write("Variable not initialised. Please check."),nl, fail.

eval_expr(t_assign(t_id(X), _Y), Env, _FinalEnv, _Val):- lookup(X, Env, _, Type),
    Type \= num, write("This operation can only be perfomed on num type of variable. Please check."),nl, fail.

eval_expr(t_add(X, Y), Env, FinalEnv, Val):- eval_expr(X, Env, Env1, V1),
                                             eval_expr(Y, Env1, FinalEnv, V2),
                                             Val is V1 + V2.

eval_expr(t_sub(X, Y), Env, FinalEnv, Val):- eval_expr(X, Env, Env1, V1),
                                             eval_expr(Y, Env1, FinalEnv, V2),
                                             Val is V1 - V2.

eval_expr(t_div(X, Y), Env, FinalEnv, Val):- eval_expr(X, Env, Env1, V1),
                                             eval_expr(Y, Env1, FinalEnv, V2),
    							   			 Val is V1 / V2.

eval_expr(t_mul(X, Y), Env, FinalEnv, Val):- eval_expr(X, Env, Env1, V1),
                                             eval_expr(Y, Env1, FinalEnv, V2),
    							   			 Val is V1 * V2.

eval_expr(t_num(X), Env, Env, X).

eval_expr(t_id(X), Env, Env, Val):- check_present(X, Env), lookup(X, Env, Val, num).

eval_expr(t_id(X), Env, Env, _Val):- \+check_present(X, Env), write("Variable not initialised. Please check."),nl, fail.

eval_expr(t_id(X), Env, Env, Val):- lookup(X, Env, Val, Type), Type \= num,
    write("This operation can only be perfomed on num type of variable. Please check."),nl, fail.



% Evaluate Boolean Expression
not(true, false).
not(false, true).

eval_bool(false, Env, Env, false).
eval_bool(true, Env, Env, true).
eval_bool(t_id(X), Env, Env, Val) :- check_present(X, Env), lookup(X, Env, Val, bool).
eval_bool(t_id(X), Env, Env, Val):- lookup(X, Env, Val, Type), Type \= bool,
    write("This operation can only be perfomed on boolean type of variable. Please check."), nl, fail.

eval_bool(t_id(X), Env, Env, _Val):- \+check_present(X, Env), write("Variable not initialised. Please check."), 
    nl, fail.

eval_bool(t_notbool(not, X), Env, FinalEnv, Val) :- eval_bool(X, Env, FinalEnv, V1), not(V1, Val).


eval_bool(t_bool_operation(X, Y, Z), Env, FinalEnv, Val) :- eval_bool(X, Env, Env1, V1),
    eval_bool(Z, Env1, FinalEnv, V2),
    eval_bool_operator(Y, V1, V2, Val).

eval_bool(t_bool(X, Y, Z), Env, FinalEnv, Val):- eval_expr(X, Env, Env1,V1), 
    eval_expr(Z, Env1, FinalEnv,V2),
    eval_compare(Y, V1, V2, Val).

eval_compare(t_comp_op(>), V1, V2, true):- V1 > V2.
eval_compare(t_comp_op(>), V1, V2, false):- V1 =< V2.

eval_compare(t_comp_op(<), V1, V2, true):- V1 < V2.
eval_compare(t_comp_op(<), V1, V2, false):- V1 >= V2.

eval_compare(t_comp_op(==), V1, V2, true):- V1 =:= V2.
eval_compare(t_comp_op(==), V1, V2, false):- V1 =\= V2.

eval_compare(t_comp_op(<=), V1, V2, true):- V1 =< V2.
eval_compare(t_comp_op(<=), V1, V2, false):- V1 > V2.
                
eval_compare(t_comp_op(>=), V1, V2, true):- V1 >= V2.
eval_compare(t_comp_op(>=), V1, V2, false):- V1 < V2.

eval_compare(t_comp_op(=\=), V1, V2, false):- V1 =:= V2.
eval_compare(t_comp_op(=\=), V1, V2, true):- V1 =\= V2.

% And OR boolean operations
eval_bool_operator(t_bool_op_and(and),false,true,false).
eval_bool_operator(t_bool_op_and(and),false,false,false).
eval_bool_operator(t_bool_op_and(and),true,false,false).
eval_bool_operator(t_bool_op_and(and),true,true,true).
   
eval_bool_operator(t_bool_op_or(or),false,true,true).
eval_bool_operator(t_bool_op_or(or),false,false,false).
eval_bool_operator(t_bool_op_or(or),true,false,true).
eval_bool_operator(t_bool_op_or(or),true,true,true).



% Evaluate Ternary Statement
eval_ternary(t_ternary(X, Y, _), Env, FinalEnv, Val) :- eval_bool(X, Env, Env1, true),
    eval_expr(Y, Env1, FinalEnv, Val).

eval_ternary(t_ternary(X, _, Z), Env, FinalEnv, Val) :- eval_bool(X, Env, Env1, false),
    eval_expr(Z, Env1, FinalEnv, Val).



% Evaluate Declaration Statements
eval_declaration(t_declaration_bool_assign(t_id(X),Y), Env, FinalEnv) :- 
    eval_bool(Y, Env, Env1, Val), update(X, Val, bool, Env1 , FinalEnv).

eval_declaration(t_declaration_bool_assign(t_id(X)), Env, FinalEnv) :- 
    update(X, false, bool, Env , FinalEnv).

eval_declaration(t_declaration_str_assign(t_id(X),Y), Env, FinalEnv) :- 
    update(X, Y, str, Env , FinalEnv).

eval_declaration(t_declaration_str_assign(t_id(X)), Env, FinalEnv) :- 
    update(X, "", str, Env , FinalEnv).

eval_declaration(t_declaration_num_assign(t_id(X),Y), Env, FinalEnv) :- 
    eval_expr(Y, Env, Env1, Val), update(X, Val, num, Env1 , FinalEnv).

eval_declaration(t_declaration_num_assign_ternary(t_id(X), Y), Env, FinalEnv) :- 
    eval_ternary(Y, Env, Env1, Val), update(X, Val, num, Env1 , FinalEnv).

eval_declaration(t_declaration_num_assign(t_id(X)), Env, FinalEnv) :- 
    update(X, 0, num, Env, FinalEnv).



% Evaluate assign statements
eval_assignment(t_assignment_bool(t_id(X), _Y), Env, _FinalEnv) :- \+check_present(X, Env), 
    write("Variable not initialised. Please check."),nl, fail.

eval_assignment(t_assignment_bool(t_id(X), Y), Env, FinalEnv) :- lookup(X, Env, _, bool), eval_bool(Y, Env, Env1, Val),
    update(X, Val, bool, Env1, FinalEnv).

eval_assignment(t_assignment_bool(t_id(X), _Y), Env, _FinalEnv) :- lookup(X, Env, _, Type), Type \= bool,
    write("Cannot assign this value to non boolean type of variable"),nl, fail.

eval_assignment(t_assignment_str(t_id(X), _Y), Env, _FinalEnv) :- \+check_present(X, Env), 
    write("Variable not initialised. Please check."),nl, fail.

eval_assignment(t_assignment_str(t_id(X), Y), Env, FinalEnv) :- lookup(X, Env, _, str), 
    update(X, Y, str, Env, FinalEnv).

eval_assignment(t_assignment_str(t_id(X), _Y), Env, _FinalEnv) :- lookup(X, Env, _, Type), 
	Type \= str,
    write("Cannot assign this value to non string type of variable"),nl.

eval_assignment(t_assignment_num_assign(t_id(X), _Y), Env, _FinalEnv) :- 
	\+check_present(X, Env), 
    write("Variable not initialised. Please check."),nl, fail.

eval_assignment(t_assignment_num_assign_ternary(t_id(X), _Y), Env, _FinalEnv) :- 
	\+check_present(X, Env), 
    write("Variable not initialised. Please check."),nl, fail.

eval_assignment(t_assignment_num_assign(t_id(X), Y), Env, FinalEnv) :- 
	lookup(X, Env, _, num), 
    eval_expr(Y, Env, Env1, Val), 
	update(X, Val, num, Env1, FinalEnv).

eval_assignment(t_assignment_num_assign_ternary(t_id(X), Y), Env, FinalEnv) :- 
	lookup(X, Env, _, num), 
    eval_ternary(Y, Env, Env1, Val), 
	update(X, Val, num, Env1, FinalEnv).

eval_assignment(t_assignment_num_assign(t_id(X), _Y), Env, _FinalEnv) :- 
	lookup(X, Env, _, Type), 
	Type \= num,
    write("Cannot assign this value to non boolean type of variable"),nl, 
	fail.

eval_assignment(t_assignment_num_assign_ternary(t_id(X), _Y), Env, _FinalEnv) :- 
	lookup(X, Env, _, Type), 
	Type \= num,
    write("Cannot assign this value to non boolean type of variable"),nl, 
	fail.



% Print Statements
eval_print(t_print(), Env, Env).
eval_print(t_print(X, Y), Env, FinalEnv) :- write(X), eval_print(Y, Env, FinalEnv).
eval_print(t_print_id(X, Y), Env, FinalEnv) :- lookup(X,Env,Val,_), write(Val), eval_print(Y, Env, FinalEnv).
eval_print(t_print_id(X, _), Env, Env) :- \+check_present(X, Env), write("Variable not initialised. Please check.").



% If else statement
eval_ifelse_stmt(t_ifstmt(X, Y, _), Env, FinalEnv) :- eval_bool(X, Env, Env1, true), eval_command(Y, Env1, FinalEnv).
eval_ifelse_stmt(t_ifstmt(X, _, Z), Env, FinalEnv) :- eval_bool(X, Env, Env1, false), eval_ifelse_stmt(Z, Env1, FinalEnv).
eval_ifelse_stmt(t_elifstmt(X, Y, _), Env, FinalEnv) :- eval_bool(X, Env, Env1, true), eval_command(Y, Env1, FinalEnv).
eval_ifelse_stmt(t_elifstmt(X, _, Z), Env, FinalEnv) :- eval_bool(X, Env, Env1, false), eval_ifelse_stmt(Z, Env1, FinalEnv).
eval_ifelse_stmt(t_goto_else_stmt(X), Env, FinalEnv) :- eval_ifelse_stmt(X, Env, FinalEnv).
eval_ifelse_stmt(t_elsestmt(X), Env, FinalEnv) :- eval_command(X, Env, FinalEnv).
eval_ifelse_stmt(t_elsestmt(), Env, Env) :- true.



% While statement
eval_while(t_statement_while(X,Y), Env, FinalEnv):- eval_bool(X, Env, Env1, true), 
    eval_command(Y, Env1, Env2),
    eval_statement(t_statement_while(X,Y), Env2, FinalEnv).

eval_while(t_statement_while(X,_), Env, FinalEnv):- eval_bool(X, Env, FinalEnv, false).



% For loops
eval_for_loop(t_new_for(A,B,C,D), Env, FinalEnv) :- 
    eval_for_loop(t_conventional_for(A,B,t_comp_op(<),C, t_assign(A, t_add(A, t_num(1))),D), Env, FinalEnv).

eval_for_loop(t_conventional_for(A,_B,_C,_D,_E,_F), Env, _FinalEnv) :- \+check_present(A, Env), 
    write("Variable not initialised. Please check."),nl, fail.

eval_for_loop(t_conventional_for(A,B,C,D,E,F), Env, FinalEnv) :- eval_expr(B, Env, Env1, Val), 
    lookup(A, Env1, _, Type), 
	update(A, Val, Type, Env1, Env2), 
    eval_for_statement(t_conventional_for(A,B,C,D,E,F), Env2, FinalEnv).

eval_for_statement(t_conventional_for(A,B,C,D,E,F), Env, FinalEnv) :- eval_bool(t_bool(A, C, D), Env, Env1, true), 
    eval_command(F, Env1, Env2), 
	eval_expr(E, Env2, Env3, Val), 
	lookup(A, Env1, _, Type),
    update(A, Val, Type, Env3, Env4), 
	eval_for_statement(t_conventional_for(A,B,C,D,E,F), Env4, FinalEnv).

eval_for_statement(t_conventional_for(A,_,C,D,_,_), Env, Env) :- eval_bool(t_bool(A, C, D), Env, _, false).



% Evaluate Statements
eval_statement(t_statement_declaration(X), Env, FinalEnv) :- eval_declaration(X, Env, FinalEnv).
eval_statement(t_statement_assign(X), Env, FinalEnv) :- eval_assignment(X, Env, FinalEnv).
eval_statement(t_statement_print(X), Env, FinalEnv) :- eval_print(X, Env, FinalEnv).
eval_statement(t_statement_ifelse(X), Env, FinalEnv) :- eval_ifelse_stmt(X, Env, FinalEnv).	
eval_statement(t_statement_while(X, Y), Env, FinalEnv) :- eval_while(t_statement_while(X, Y), Env, FinalEnv).
eval_statement(t_statement_for(X), Env, FinalEnv) :- eval_for_loop(X, Env, FinalEnv).



% Evaluate Command
eval_command(t_command(), Env, Env).
eval_command(t_command(X, Y), Env, FinalEnv) :- eval_statement(X, Env, Env1), eval_command(Y, Env1, FinalEnv).



% Evaluate Block
eval_block(t_block(X), Env, FinalEnv):- eval_command(X, Env, FinalEnv).

program_eval(t_program(X), Env):- eval_block(X, [], Env), !.
