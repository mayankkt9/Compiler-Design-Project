#!/usr/bin/env python3
from nltk.tokenize import sent_tokenize, word_tokenize
from pyswip import Prolog
from functools import reduce


def lexer_job():
    str2 = open('inputsourcecode.rch', 'r').read()
    str1 = word_tokenize(str2)
    lex = ""
    lex += '['
    for i in range(0, len(str1)):
        ascii = reduce(lambda x, y: str(x)+str(y), map(ord, str1[i]))
        # print(str1[i]+ " "+str(ascii))
        if i > 1 and str1[i] == '=' and str1[i-1] == ':':
            continue
        if str1[i] == ':' and str1[i+1] == '=':
            lex += str1[i]+str1[i+1]
            i += 2
        elif str1[i] == '(':
            lex += "'('"
        elif str1[i] == ')':
            lex += "')'"
        elif ascii == "9696":
            lex += "'\"'"
        elif ascii == "3939":
            lex += "'\"'"
        elif str1[i] == '}':
            lex += "'}'"
        elif str1[i] == '{':
            lex += "'{'"
        else:
            lex += str1[i]
        lex += ','
    lex = lex[:-1]
    lex += ']'
    print("LEXER")
    print(lex)
    return lex


def create_parse_tree(lex):

    prolog.consult("semantics.pl")

    query = "program(P,{},[])."

    parsetree = prolog.query(query.format(lex))

    return next(parsetree)["P"]


def give_semantics(parse_tree):

    query = "program_eval({},Z)"

    Env = prolog.query(query.format(parse_tree))

    print(next(Env)["Z"])


def set_up_env():
    import nltk
    nltk.download('punkt')


# set_up_env()
prolog = Prolog()
lex = lexer_job()
parse_tree = create_parse_tree(lex)
give_semantics(parse_tree)

