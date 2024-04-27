# LATSI

LATSI is an academic project in the in the grammar and lexical analysis module that implements an interpreter for a simple language using <b>The OCaml programming language with ocamllex and menhir.</b>

## Building

To build the project just lunch this command in the terminal:

```bash
dune build
```

## Running

You can run the program with a specific file as an input:

```bash
_build/default/src/latsi.exe < tests/file.lt
```

Or you can use it to read from standard input, like so.
