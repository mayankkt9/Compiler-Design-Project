% Runtime Semantics



% Update Environment
update(K, V, [], [(K,V)]).

update(K, V, [(K,_)|T], [(K,V)|T]).

update(K, V, [H|T], [H|R]) :- update(K,V, T, R).


% Lookup Value in Environment
lookup(K, [(K,V)|_], V).

lookup(K, [_|T], V) :- lookup(K, T, V).


% Evaluate Expression
eval_expr(t_assign(t_Id(X), Y), Env, FinalEnv, Val):- eval_expr(Y, Env, Env1, Val),
    										update(X, Val, Env1, FinalEnv).

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

eval_expr(t_brk(X), Env, FinalEnv, Val):- eval_expr(X, Env, FinalEnv, Val).

eval_expr(t_Id(X), Env, Env, Val):- lookup(X, Env, Val).

eval_expr(t_num(X), Env, Env, X).



% Evaluate Boolean Expression
not(true, false).

not(false, true).

eval_bool(t_fbool(false), Env, Env, false).

eval_bool(t_tbool(true), Env, Env, true).

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
eval_declaration(t_declaration_bool_assign(X,Y), Env, FinalEnv) :- 
    eval_bool(Y, Env, Env1, Val), update(X, Val, Env1 , FinalEnv).

eval_declaration(t_declaration_str_assign(X,Y), Env, FinalEnv) :- 
    update(X, Y, Env , FinalEnv).

eval_declaration(t_declaration_num_assign(X,Y), Env, FinalEnv) :- 
    eval_expr(Y, Env, Env1, Val), update(X, Val, Env1 , FinalEnv).

eval_declaration(t_declaration_num_assign(X,Y), Env, FinalEnv) :- 
    eval_ternary(Y, Env, Env1, Val), update(X, Val, Env1 , FinalEnv).

% Evaluate Statements
eval_statement(t_statement_declaration(X), Env, FinalEnv) :- eval_declaration(X, Env, FinalEnv).

% May need to modiy it depending upon what we are printing
eval_statement(t_statement_print(t_print(X)), Env, Env) :- write(X).
eval_statement(t_statement_print(t_print_id(X)), Env, Env) :- lookup(X, Env, Val), write(Val).

eval_statement(t_statement_while(X,Y,Z), Env, FinalEnv):- eval_bool(X, Env, Env1, true), 
    										eval_command(Y, Env1, Env2),
    										eval_command(t_statement_while(X,Y,Z), Env2, FinalEnv).

eval_statement(t_statement_while(X,_,_), Env, FinalEnv):- eval_bool(X, Env, FinalEnv, false).

eval_statement(t_statement_iflese(t_ifstmt(X, Y), _, _), Env, FinalEnv) :-
    eval_bool(X, Env, Env1, true), eval_command(Y, Env1, FinalEnv).
    
eval_statement(t_statement_iflese(_, t_elifstmt(X, Y, _), _), Env, FinalEnv) :-
    eval_bool(X, Env, Env1, true), eval_command(Y, Env1, FinalEnv).

eval_statement(t_statement_iflese(_, t_elifstmt(X, _, Z), _), Env, FinalEnv) :-
    eval_bool(X, Env, Env1, false), eval_statement(Z, Env1, FinalEnv).

% Need to check
eval_statement(t_statement_iflese(_, t_elifstmt(), _), Env, Env) :- false.

eval_statement(t_statement_iflese(_, _, t_elifstmt(X)), Env, FinalEnv) :- 
    eval_command(X, Env, FinalEnv).

eval_statement(t_statement_iflese(_, _, t_elifstmt()), Env, Env) :- true.

% Need to implement
% eval_statement(t_statement_for(X)) --> conventional_for(X) | new_for(X).

% Evaluate Command
eval_command(t_command(), Env, Env).

eval_command(t_command(X, Y), Env, FinalEnv) :- eval_statement(X, Env, Env1), eval_command(Y, Env1, FinalEnv).


% Evaluate Block
eval_block(t_block(X), Env, FinalEnv):- eval_command(X, Env, FinalEnv).

program_eval(t_program(X), Env):- eval_block(X, [], Env).
