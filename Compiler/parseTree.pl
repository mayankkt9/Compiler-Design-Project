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

brackets(t_brk(X)) --> ['('], expr(X), [')'].
brackets(X) --> num(X).
brackets(X) --> identifier(X).

identifier(t_identifier(X)) -->[X],{X \= true}, {X \= false}, {atom(X)}.
num(t_num(X)) --> [X], {number(X)}.


% 4 data types
data_type-->[int].
data_type-->[float].
data_type-->[string].
data_type-->[boolean].


% Boolean Operators
boolean_operator(t_bool_op_and(and))  --> [and].
boolean_operator(t_bool_op_or(or))  --> [or].


% Boolena Operations
bool(t_fbool(false))-->[false].
bool(t_tbool(true))--> [true].
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
assignment(t_assignment_num(X, Y)) --> identifier(X), [=], expr(Y).
assignment(t_assignment_num(X, Y)) --> identifier(X), [=], ternary_op(Y).

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
conventional_for(t_for_conv(U, V, W, X, Y, Z)) --> [for], identifier(U), [=], expr(V), [;],
    						identifier(U), comparison_operator(W), expr(X), [;],
    						identifier(U), [=], expr(Y),
    						['{'], command(Z), ['}'].

new_for(t_for_new(W, X, Y, Z)) --> [for], identifier(W), [in], [range] ,['('], num(X),
    							[,], num(Y), ['{'], command(Z), ['}'].


% General Statements and While loop
statement(t_statement_declaration(X)) --> declaration(X).
statement(t_statement_assign(X)) --> assignment(X).
statement(t_statement_print(X)) --> [print], ['('] , printv(X), [')'].
statement(t_statement_iflese(X, Y, Z)) --> if_stmt(X), elif_stmt(Y), else_stmt(Z).
statement(t_statement_while(X, Y)) --> [while], ['('], bool(X), [')'], ['{'], command(Y), ['}'].
statement(t_statement_for(X)) --> conventional_for(X) | new_for(X).


% Command List and single command is called statement.
command(t_command(X, Y)) --> statement(X), command(Y).
command(t_command()) --> [].


% Block.
block(t_block(X))-->[start],command(X),[end].


% Program entr point. Will take input as list of tokens and generate parse tree.
program(t_program(X))-->block(X).