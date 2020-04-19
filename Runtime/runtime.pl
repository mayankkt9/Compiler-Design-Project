% Runtime Semantics



% Update Environment
update(K, V, Type, [], [(K, V, Type)]).

update(K, V, Type, [(K, _, _)|T], [(K, V, Type)|T]).

update(K, V, Type, [H|T], [H|R]) :- update(K, V, Type, T, R).


% Lookup Value in Environment
lookup(_, [], _, _) :- write("Variable not initialised. Please check."), fail.

lookup(K, [(K,V,Type)|_], V, Type).

lookup(K, [_|T], V, Type) :- lookup(K, T, V, Type).


% Evaluate Expression
eval_expr(t_assign(X, Y), Env, FinalEnv, Val):- lookup(X, Env, _, num),
    eval_expr(Y, Env, Env1, Val), update(X, Val, num, Env1, FinalEnv).

eval_expr(t_assign(X, _Y), Env, _FinalEnv, _Val):- lookup(X, Env, _, Type),
    Type \= num, write("This operation can only be perfomed on num type of variable. Please check."), fail.

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

eval_expr(X, Env, Env, X) :- number(X).

eval_expr(X, Env, Env, Val):- lookup(X, Env, Val, num).

eval_expr(X, Env, Env, Val):- lookup(X, Env, Val, Type), Type \= num,
    write("This operation can only be perfomed on num type of variable. Please check."), fail.

% Evaluate Boolean Expression
not(true, false).

not(false, true).

eval_bool(false, Env, Env, false).

eval_bool(true, Env, Env, true).

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

% Need to test
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
eval_statement(t_declaration_bool_assign(X,Y), Env, FinalEnv) :- 
    eval_bool(Y, Env, Env1, Val), update(X, Val, bool, Env1 , FinalEnv).

eval_statement(t_declaration_str_assign(X,Y), Env, FinalEnv) :- 
    update(X, Y, str, Env , FinalEnv).

eval_statement(t_declaration_num_assign(X,Y), Env, FinalEnv) :- 
    eval_expr(Y, Env, Env1, Val), update(X, Val, num, Env1 , FinalEnv).

eval_statement(t_declaration_num_assign(X,Y), Env, FinalEnv) :- 
    eval_ternary(Y, Env, Env1, Val), update(X, Val, num, Env1 , FinalEnv).

% Evaluate Statements
eval_statement(t_statement_declaration(X), Env, FinalEnv) :- eval_statement(X, Env, FinalEnv).

% May need to modiy it depending upon what we are printing
eval_statement(t_statement_print(t_print(X)), Env, Env) :- write(X).
eval_statement(t_statement_print(t_print_id(X)), Env, Env) :- lookup(X, Env, Val, _), write(Val).

eval_statement(t_statement_ifelse(_, t_elifstmt(), _), Env, Env) :- false.

eval_statement(t_statement_ifelse(_, _, t_elifstmt(X)), Env, FinalEnv) :- 
    eval_command(X, Env, FinalEnv).

eval_statement(t_statement_ifelse(_, _, t_elifstmt()), Env, Env) :- true.

eval_statement(t_statement_while(X,Y), Env, FinalEnv):- eval_bool(X, Env, Env1, true), 
    										eval_command(Y, Env1, Env2),
    										eval_statement(t_statement_while(X,Y), Env2, FinalEnv).

eval_statement(t_statement_while(X,_), Env, FinalEnv):- eval_bool(X, Env, FinalEnv, false).

eval_statement(t_statement_ifelse(t_ifstmt(X, Y), _, _), Env, FinalEnv) :-
    eval_bool(X, Env, Env1, true), eval_command(Y, Env1, FinalEnv).
    
eval_statement(t_statement_ifelse(_, t_elifstmt(X, Y, _), _), Env, FinalEnv) :-
    eval_bool(X, Env, Env1, true), eval_command(Y, Env1, FinalEnv).

eval_statement(t_statement_ifelse(_, t_elifstmt(X, _, Z), _), Env, FinalEnv) :-
    eval_bool(X, Env, Env1, false), eval_statement(Z, Env1, FinalEnv).

% Evaluate assign statements
eval_statement(t_assignment_bool(X, Y), Env, FinalEnv) :- lookup(X, Env, _, bool), eval_bool(Y, Env, Env1, Val),
    update(X, Val, bool, Env1, FinalEnv).
eval_statement(t_assignment_bool(X, _Y), Env, _FinalEnv) :- lookup(X, Env, _, Type), Type \= bool,
    write("Cannot assign this value to non boolean type of variable").
eval_statement(t_assignment_str(X, Y), Env, FinalEnv) :- lookup(X, Env, _, str), 
    update(X, Y, str, Env, FinalEnv).
eval_statement(t_assignment_str(X, _Y), Env, _FinalEnv) :- lookup(X, Env, _, Type), Type \= str,
    write("Cannot assign this value to non string type of variable").
eval_statement(t_declaration_num_assign(X, Y), Env, FinalEnv) :- lookup(X, Env, _, num), 
    eval_expr(Y, Env, Env1, Val), update(X, Val, num, Env1, FinalEnv).
eval_statement(t_declaration_num_assign(X, Y), Env, FinalEnv) :- lookup(X, Env, _, num), 
    eval_ternary(Y, Env, Env1, Val), update(X, Val, num, Env1, FinalEnv).
eval_statement(t_declaration_num_assign(X, _Y), Env, _FinalEnv) :- lookup(X, Env, _, Type), Type \= num,
    write("Cannot assign this value to non boolean type of variable").

eval_statement(t_statement_assign(X), Env, FinalEnv) :- eval_statement(X, Env, FinalEnv).

% Need to check
eval_statement(t_statement_ifelse(_, t_elifstmt(), _), Env, Env) :- false.

eval_statement(t_statement_ifelse(_, _, t_elifstmt(X)), Env, FinalEnv) :- 
    eval_command(X, Env, FinalEnv).

eval_statement(t_statement_ifelse(_, _, t_elifstmt()), Env, Env) :- true.

% Need to check for loop
eval_statement(t_new_for(A,B,C,D), Env, FinalEnv) :- 
    eval_statement(t_conventional_for(A,B,t_comp_op(<),C, t_assign(A, t_add(A, t_num(1))),D), Env, FinalEnv).
eval_statement(t_conventional_for(A,B,C,D,E,F), Env, FinalEnv) :- eval_expr(B, Env, Env1, Val), 
    lookup(A, Env1, _, Type), update(A, Val, Type, Env1, Env2), 
    eval_for_statement(t_conventional_for(A,B,C,D,E,F), Env2, FinalEnv).
eval_for_statement(t_conventional_for(A,B,C,D,E,F), Env, FinalEnv) :- eval_bool(t_bool(A, C, D), Env, Env1, true), 
    eval_command(F, Env1, Env2), eval_expr(E, Env2, Env3, Val), lookup(A, Env1, _, Type),
    update(A, Val, Type, Env3, Env4), eval_for_statement(t_conventional_for(A,B,C,D,E,F), Env4, FinalEnv).
eval_for_statement(t_conventional_for(A,_,C,D,_,_), Env, Env) :- eval_bool(t_bool(A, C, D), Env, _, false).

% Evaluate Command
eval_command(t_command(), Env, Env).

eval_command(t_command(X, Y), Env, FinalEnv) :- eval_statement(X, Env, Env1), eval_command(Y, Env1, FinalEnv).


% Evaluate Block
eval_block(t_block(X), Env, FinalEnv):- eval_command(X, Env, FinalEnv).

program_eval(t_program(X), Env):- eval_block(X, [], Env), !.
