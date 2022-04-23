# Blank

Blank is an interpreter for the
[Whitespace](https://en.wikipedia.org/wiki/Whitespace_(programming_language))
programming language.

## Getting Started

Blank is written in [OCaml](https://ocaml.org/), instructions on how to install
OCaml for your operating system can be found in the [offical
documentation](https://ocaml.org/docs/install.html).

Blank uses the [Dune](https://github.com/ocaml/dune) build
system. Instructions on installing Dune can be found in the [project
documentation](https://github.com/ocaml/dune#installation).

Once you have installed OCaml and Dune, the project can be built by running

```shell
dune build
```

Once built, the `blank.exe` build artifact can be found in the
`_build/default` directory.

## Example Usage

The fizz-buzz.ws example program in the `examples` directory can be
run using the following command:

```shell
./_build/default/blank.exe examples/fizz-buzz.ws
```

