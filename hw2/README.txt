                    CSCI 5535 - Homework #2 (Code Portion)

-1. This README is just like the README for Homework #1, so you don't need
to read it again if you remember how that worked. 

0. There is a written component to the homework as well. Don't forget!

1. This is version 1.2 of the Homework #2 code pack. Before submitting your
work or reporting a bug check to make sure that a more recent version has
not been released. 

2. This assignment will require you to write in OCaml, a popular and
efficient ML variant. ML variants are favored by PL researchers because
they are particularly good for writing programs that manipulate other
programs. 

Manual:         http://caml.inria.fr/pub/docs/manual-ocaml/index.html
Tutorials:      http://caml.inria.fr/pub/docs/manual-ocaml/manual003.html
                http://www.ocaml-tutorial.org/
Book:           http://caml.inria.fr/pub/docs/oreilly-book/
Download It:    http://caml.inria.fr/ocaml/release.en.html

In this assignment, we'll be manipulating (interpreting) IMP programs. All
of the "undergrad" work (e.g., the lexer, the parser) has been done
for you. You need only flesh out the semantics-based interpreter. 

If you have used Standard ML before, here's a quick side-by-side
comparison of the syntactic differences:

  http://www.mpi-sws.org/~rossberg/sml-vs-ocaml.html

3. Manifest: 

README.txt              this file
imp.ml                  Abstract syntax of Winskel's IMP as an abstract
                        data type (AST)
lex.mll                 A "lex" file for our IMP concrete syntax
parse.mly               A "yacc" file for our IMP concrete syntax
main.ml                 A main() driver that reads an IMP command from
                        stdin and evaluates it
hw2.ml                  *** The file you must edit so that it contains
                        an interpreter for IMP with exceptions ***
hw2.mli                 Your hw2.ml must meet this contract (although it
                        can do other things)
hello.ml                "Hello, World" in OCaml
Makefile                "make hello", "make all", "make clean" and "make test"

This distribution ships with parse.ml and lex.ml pre-generated (although
you can rebuild them if you like) so that you can still get going even if
you, for some reason, can't get ocamllex and ocamlyacc to work. 

4. Get ocaml up and running on your system. Make sure that "make hello"
works. Part of comparing and evaluating languages involves being able to 
run and try out new languages and run-time systems. 

5. Run "make all" to build the skeletal IMP interpreter. 

6. Run the resulting imp executable and type in "skip ." as input. You
should see something like this: 

  omoide:~$ ./imp.exe 
  Enter an IMP command (use . to end your command):
  skip .
  skip

The harness accepts an IMP command (terminated with a "."), pretty-prints
it back out, and then interprets it. Interpreting skip isn't that exciting,
however. Our concrete IMP syntax is more or less what you would expect. It
also supports ()'s, {}'s, and comments. Example concrete IMP command:

  x := 5 ;      /* comment */
  { if x <= 9 then { x := x - (5 - 3) } else print x } ; 
  x := 6 
  .

5. Inspect imp.ml and get a feeling for how we have translated IMP into ML.
The translation is quite direct. 

6. Inspect hw2.ml to see the skeletal interpreter. I have provided a
complete interpreter for the Aexp sub-language. You must complete the
skeletal interpreters for Bexp and Com. Use the big-step operational
semantics rules as guides. 

7. Keep at it until you pass all of the tests in "make test". Note that you
should implement the "print" command using something like:

  Printf.printf "%d " the_value

... because the grading script is not expecting you to add newlines. 

8. Write some tests of your own. Put your best test case in the file
"example.imp". 

9. Rename "hw2.ml" and "example.imp" and submit them as per the
directions in the homework handout. 

---------------------------------------------------------------------------
Revision History

02/05/2010	1.1
  Changed the state type to be a functional map.
  Updated the lexer to accept negative integer constants.

09/25/2013      1.2
  Modified project to do both big-step and small-step semantics with
  exceptions.

10/03/2013      1.3
  Fixed lexer/parser to include let.
