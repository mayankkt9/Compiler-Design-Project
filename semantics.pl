:- use_rendering(svgtree).

:- table expr_op/3, term/3, bool/3.


% Need to implement % operator and I = E too.
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

identifier(X) -->[X],{X \= true}, {X \= false}, {atom(X)}.
num(X) --> [X], {number(X)}.


% 4 data types
data_type-->[int].
data_type-->[float].
data_type-->[string].
data_type-->[boolean].


% Boolean Operators
boolean_operator(t_bool_op_and(and))  --> [and].
boolean_operator(t_bool_op_or(or))  --> [or].


% Boolena Operations
bool(false)-->[false].
bool(true)--> [true].
bool(t_notbool(not, X))--> [not], bool(X).
bool(t_bool(X,Y,Z))--> expr(X), comparison_operator(Y), expr(Z).
bool(t_bool_operation(X, Y, Z)) --> bool(X), boolean_operator(Y), bool(Z).


% Need to redefine not equal to operator
comparison_operator(t_comp_op(>)) --> [>].
comparison_operator(t_comp_op(<)) --> [<].
comparison_operator(t_comp_op(==)) --> [==].
comparison_operator(t_comp_op(<=)) --> [<=].
comparison_operator(t_comp_op(>=)) --> [>=].
comparison_operator(t_comp_op('!=')) --> ['!='].


% Ternary Operation
ternary_op(t_ternary(X, Y, Z)) --> bool(X), [?], expr(Y), [:], expr(Z).

% Declaration statements
declaration(t_declaration_bool_assign(X, Y)) --> [boolean], identifier(X), [=], bool(Y).
declaration(t_declaration_str_assign(X, Y)) --> [string], identifier(X), [=], ['"'], [Y], ['"'].
declaration(t_declaration_num_assign(X, Y)) --> [int], identifier(X), [=], expr(Y).
declaration(t_declaration_num_assign(X, Y)) --> [int], identifier(X), [=], ternary_op(Y).
declaration(t_declaration_identifier(X)) --> data_type, identifier(X).

% Assignment statements
assignment(t_assignment_bool(X, Y)) --> identifier(X), [=], bool(Y).
assignment(t_assignment_str(X, Y)) --> identifier(X), [=], ['"'], [Y], ['"'].
assignment(t_declaration_num_assign(X, Y)) --> identifier(X), [=], expr(Y).
assignment(t_declaration_num_assign(X, Y)) --> identifier(X), [=], ternary_op(Y).

% Need to implement print statement
printv(t_print_id(X)) --> [X].
printv(t_print(X)) --> ['"'], [X], ['"'].


% if else statements
if_stmt(t_ifstmt(X, Y)) --> [if], ['('], bool(X), [')'], ['{'], command(Y), ['}'].

elif_stmt(t_elifstmt(X, Y, Z)) --> [elif], ['('], bool(X), [')'], ['{'], command(Y), ['}'], elif_stmt(Z).
elif_stmt(t_elifstmt()) --> [].

else_stmt(t_elifstmt(X)) --> [else], ['{'], command(X), ['}'].
else_stmt(t_elsefstmt()) --> [].


% for loops
conventional_for(t_conventional_for(A,B,C,D,E,F)) --> [for], ['('], identifier(A), [=], expr(B), [;], 
    identifier(A), comparison_operator(C), expr(D), [;], 
    identifier(A), [=], expr(E), [')'], ['{'], command(F), ['}'].

new_for(t_new_for(A,B,C,D)) --> [for], identifier(A), [in], 
    [range], ['('], num(B), [,], num(C), [')'], ['{'], command(D), ['}'].

% General Statements and While loop
statement(t_statement_declaration(X)) --> declaration(X).
statement(t_statement_assign(X)) --> assignment(X).
statement(t_statement_print(X)) --> [print], ['('] , printv(X), [')'].
statement(t_statement_ifelse(X, Y, Z)) --> if_stmt(X), elif_stmt(Y), else_stmt(Z).
statement(t_statement_while(X, Y)) --> [while], ['('], bool(X), [')'], ['{'], command(Y), ['}'].
statement(t_statement_for(X)) --> conventional_for(X).
statement(t_statement_for(X)) --> new_for(X).


% Command List and single command is called statement.
command(t_command(X, Y)) --> statement(X), command(Y).
command(t_command()) --> [].


% Block.
block(t_block(X))-->[start],command(X),[end].


% Program entr point. Will take input as list of tokens and generate parse tree.
program(t_program(X))-->block(X).

%-----------------------------------------------------------------------------------------------------------

% Runtime Semantics



% Update Environment
update(K, V, Type, [], [(K, V, Type)]).

update(K, V, Type, [(K, _, _)|T], [(K, V, Type)|T]).

update(K, V, Type, [H|T], [H|R]) :- update(K, V, Type, T, R).


% Lookup Value in Environment
lookup(K, [], _, _) :- write("Variable not initialised. Please check."), fail.

lookup(K, [(K,V,Type)|_], V, Type).

lookup(K, [_|T], V, Type) :- lookup(K, T, V, Type).


% Evaluate Expression
eval_expr(t_assign(t_Id(X), Y), Env, FinalEnv, Val):- lookup(X, Env, _, num),
    eval_expr(Y, Env, Env1, Val), update(X, Val, num, Env1, FinalEnv).

eval_expr(t_assign(t_Id(X), _Y), Env, _FinalEnv, _Val):- lookup(X, Env, _, Type),
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

eval_statement(t_statement_assign(X), Env, FinalEnv) :- eval_declaration(X, Env, FinalEnv).

% Need to check
eval_statement(t_statement_ifelse(_, t_elifstmt(), _), Env, Env) :- false.

eval_statement(t_statement_ifelse(_, _, t_elifstmt(X)), Env, FinalEnv) :- 
    eval_command(X, Env, FinalEnv).

eval_statement(t_statement_ifelse(_, _, t_elifstmt()), Env, Env) :- true.

% Need to check for loop
eval_statement(t_new_for(A,B,C,D), Env, FinalEnv) :- 
    eval_statement(t_conventional_for(A,B,t_comp_op(<),C, t_assign(A, t_add(A, t_num(1))),D), Env, FinalEnv).
eval_statement(t_conventional_for(A,B,C,D,E,F), Env, FinalEnv) :- eval_expr(B, Env, Env1, Val), 
    update(A, Val, Env1, Env2), eval_for_statement(t_conventional_for(A,B,C,D,E,F), Env2, FinalEnv).
eval_for_statement(t_conventional_for(A,B,C,D,E,F), Env, FinalEnv) :- eval_bool(t_bool(A, C, D), Env, Env1, true), 
    eval_command(F, Env1, Env2), eval_expr(E, Env2, Env3, Val), update(A, Val, Env3, Env4),
    eval_for_statement(t_conventional_for(A,B,C,D,E,F), Env4, FinalEnv).
eval_for_statement(t_conventional_for(A,_,C,D,_,_), Env, Env) :- eval_bool(t_bool(A, C, D), Env, _, false).

% Evaluate Command
eval_command(t_command(), Env, Env).

eval_command(t_command(X, Y), Env, FinalEnv) :- eval_statement(X, Env, Env1), eval_command(Y, Env1, FinalEnv).


% Evaluate Block
eval_block(t_block(X), Env, FinalEnv):- eval_command(X, Env, FinalEnv).

program_eval(t_program(X), Env):- eval_block(X, Env, _), !.
