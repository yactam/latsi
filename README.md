# LATSI

LATSI is an academic project in the in the grammar and lexical analysis module that implements an interpreter for a simple language using <b>The OCaml programming language with ocamllex and menhir.</b>

## Building

To build the project just launch this command in the terminal:

```bash
dune build
```

## Testing

To test the project with the provided tests you can use this command to run any provided program prog.lt:

```bash
_build/default/src/latsi.exe tests/prog.lt
```
