#!/usr/bin/env python3
from nltk.tokenize import sent_tokenize, word_tokenize 
from pyswip import Prolog
import sys
import operator
from functools import reduce
from pprint import pprint

def lexer_job():
	str2 = open('inputsourcecode.rch', 'r').read()
	str1 = word_tokenize(str2)
	lex=""
	lex+='['
	for i in range(0,len(str1)):
		ascii = reduce(lambda x, y: str(x)+str(y), map(ord,str1[i]))
		# print(str1[i]+ " "+str(ascii))
		if i>1 and str1[i]=='=' and str1[i-1]==':':
			continue;
		if str1[i]==':' and str1[i+1]=='=':
			lex+=str1[i]+str1[i+1]
			i+=2
		elif str1[i]=='(':
			lex+="'('"
		elif str1[i]==')':
			lex+="')'"
		elif ascii == "9696":
			lex+="'\"'"
		elif ascii == "3939":
			lex+="'\"'"
		elif str1[i]=='}':
			lex+="'}'"
		elif str1[i]=='{':
			lex+="'{'"
		else:
			lex+=str1[i]
		lex+=','
	lex = lex[:-1]
	lex+=']'
	print("LEXER")
	print(lex)
	return lex

def create_parse_tree(lex):
	prolog.consult("semantics.pl")
	query1 = "program(P,"+lex+",[])."
	parsetree = prolog.query(query1)
	print("PARSE TREE")
	for sol in parsetree:
		x=sol["P"]
		print(x)
	
	return parsetree

def give_semantics(parsetree):
	x=""
	for sol in parsetree:
		x=sol["P"]
	query2 = "program_eval("+x+",Z)"
	ans = prolog.query(query2)
	z=""
	# for sol in ans:
	# 	z=sol["Z"]
	# 	print(sol["Z"])

prolog = Prolog()
lex = lexer_job()
parse_tree = create_parse_tree(lex)
give_semantics(parse_tree)

#program(P, [begin, var, z, ; , var, x, ;, z, :=, x, end, .], []), write(P), program_eval(P, 2, 3, Z).

