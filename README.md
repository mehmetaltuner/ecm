# ecm
Yet another, very humble lisp-like programming language

To compile:
  gcc -std=c99 ecmlang.c mpc.c -o ecm -lm -ledit
 
To run:
  ./ecm <file_name>
  ./ecm (to access repl)
  
 To load libraries:
  load "<libname>"
  
 You can see the syntax and implementations of some essential functions in std.ecm library
