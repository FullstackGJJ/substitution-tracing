## Substitution Model Tracing tool for Chicken Scheme REPL

This implementation is based off of (https://github.com/petermichaux/bootstrap-scheme/blob/v0.21/scheme.c) and (https://homes.cs.aau.dk/~normark/prog3-03/html/notes/languages-note-scheme-in-scheme.html)

The purpose of this tool is that it allows the user to load up a function and then see its execution step by step through substition model. This is similar to how lambda calculus reduction steps look. This makes it easier to see the workflow of a possibly complicated procedure through reduction of its S-expressions.

A sample use would look like this 

Declare method
`(define (sample-handler x y) (+ (+ 1 y) x))`

Then wrap it with eval-with-trace
`(eval-with-trace (sample-handler 1 2) '() 2)`
