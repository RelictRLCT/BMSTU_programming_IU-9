#!/bin/bash

flex lexer.l
bison -d parser.y
gcc -o formatter *.c
rm -f lex.yy.c parser.tab.?
./formatter input.txt
