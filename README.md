# Assignment 8, CS3250
This repository contains a implementation of CatML, an ML variant used at UVM for CS3250.

## Building this repository with `dune`

This repository can be built with [`dune`](https://github.com/ocaml/dune), a build system for OCaml projects. It helps manage linking when your project is made up of multiple `.ml` files.

### Install `dune`

Check if dune is already installed via the following terminal command:

```
$ dune --version
```

If the command is not found, you can install it via `opam` [(detailed instructions)](https://dune.readthedocs.io/en/latest/howto/install-dune.html).

```
$ opam install dune
$ eval $(opam config env)
$ dune --version
```

### Build the project

With `dune` installed, you can build the project by running `dune build` from the command line.

```
$ dune build
```

You can also access the project modules by running `dune utop`, which will build the project and then open the `utop` toplevel. Different files (such as `src/lang.ml`) correspond to OCaml modules, and data structures (such as the `Var` constructor in the CatML AST) can be accessed via `CatML.Lang.Bool`, where `CatML` is the overall name of the project.

### Structure of the repository

```
- <root>
  - dune-project
  - src
    - dune
    - lang.ml
    - lexer.mll
    - parser.mly
    - main.ml
  - examples
    - example.catml
    - factorial.catml
```

The `dune-project` plaintext file at the top level tells dune how to build your project. The file `src/dune` tells dune the name of the library (`CatML`) and the fact that we have a ocamllex file `lexer.mll` and a menhir parser specification file `parser.mly` in the `src` subdirectory.

```
(library (name CatML))
(menhir (modules parser))
(ocamllex lexer)
```

The files in the `src` directory include:

- `lang.ml`: The main definition of the CatML language, including syntax, evaluation, and type checking
- `lexer.mll`: The specification of the lexer used by ocamllex
- `parser.mly`: The specification of the parser used by menhir
- `main.ml`: Holds the run function

In the `examples` directory I have included two sample CatML programs. You should write your own additional CatML programs to test your work.

## Other Resources

1. Parsing and lexing generators in OCaml, [OCaml Programming, Correct + Efficient + Beautiful](https://cs3110.github.io/textbook/chapters/interp/parsing.html)
2. Parsing with OCamllex and Menhir, [Real World OCaml](https://dev.realworldocaml.org/parsing-with-ocamllex-and-menhir.html)
3. [Dune build system reference](https://github.com/ocaml/dune)

## How to interact with a CatML program
Write a CatML program in a file (like examples/factorial.catml), from the root of the repository run the following:

```
dune utop src
```
```
CatML.Main.run "examples/factorial.catml";;
```

CatML.Main.run lexes and parses your passed in file, then type checks, evaluates, and outputs the solution if the program is not failing. If it does fail, it will output one of the following messages:

- `ERROR (file): .... error text ....` if it cannot read your file
- `ERROR (lexer): unrecognized char.` if it runs into a invalid character
- `ERROR (parser): invalid syntax.` 
- `ERROR (type): the program is not well typed.`

To write a program use these following rules:
- Functions need type annotations as described in the design decisions below
- The types are as listed: `Int`, `Bool`, `T * T`, `T -> T`
- Subtraction is represented as addition of negative numbers
- Variable names cannot contain special characters or numbers (letters only), something like x1 would lex as an id x followed by an int 1.

# Reflection Questions

1. If you worked as a group, list all the group members.

N/A, I worked alone.

2. Did you use generative AI to assist you in implementing this project? If so, briefly explain how you used it and for which components.

Yes, I used Claude Code (Opus & Fable) in the following situations:

- Prompted Claude to install the required tools/packages into the project and verify that they are working correctly. 
- Caught a few bugs (parens instead of braces, FIX: instead of FIX;, e1 = expr instead of e1 = app in application rules, spelling errors) inside of menhir file. It explained the issues, I made all of the changes.
- Asked for short overarching explanations of ocamllex & menhir to help me implement them cleaner. 
- Prompted Claude to clone the project repo to a private GitHub repo, and set origin. 
- Asked Claude to explain to me how I can run a function from dune utop.
- Asked Claude how to implement a command-line executable (didn't implement).
- Asked Claude how to debug "utop # CatML.Main "examples/factorial.catml";;
Error: Unbound constructor CatML.Main"
- Asked Claude why my output isn't changing after fixing my code.
- Prompted Claude to "Grade my project" to give feedback to me. (approved prompt in person).

3. Did you make any design decisions that we should know about?

a) I added type annotations on functions (fix and fun), instead of implementing type inference. examples/factorial.catml can be seen updated to the new form. 
```
  Fun x -> e changed to Fun (x : Int) -> e
  Fix f. x -> e changed to Fix (f : Int -> Int). x -> e
```

b) Application binds tighter than the ops, all of Not, First, and Second take a singular atom, in consequence ```Not Not True``` for example must be written as ```Not (Not True)```.

c) Fix is treated like a value, same as Fun, without it the Let expression cant reduce:
```
  Let fact = (Fix ...) fact 5 (examples/factorial.catml)
  -> (Fix ...) 5
  -> ... -> 120
```

4. Did you implement any of the extra credit features? Briefly summarize your work.

No, I didn't implement any extra credit features. 

5. Any feedback on the structure of this assignment?

I liked the structure of this assignment a lot. Overall, it was very helpful to my learning, and it felt more fun to complete than a normal assignment. 