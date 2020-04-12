% Parse Tree Generator

:- use_rendering(svgtree).

% Three Data type till now
data_type(t_int_data_type(int)) --> [int].
data_type(t_float_data_type(float)) --> [float].
data_type(t_string_data_type(string)) --> [string].

% Check for valid string need to add under score in grammmer
% Also need to redefine grammer for string num string
identifier(t_identifier(X)) -->[X], {atom(X)}.

declaration(t_declaration_identifier(X, Y)) --> data_type(X), identifier(Y).

printv(t_print(X)) --> [X].

statement(t_statement_declaration(X)) --> declaration(X).
statement(t_statement_print(X)) --> [print], ['('] , printv(X), [')'].

command(t_command(X, Y)) --> statement(X), command(Y).
command(t_command()) --> [].

block(t_block(X))-->[start],command(X),[end].

program(t_program(X))-->block(X), [.].