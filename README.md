# `mfetch`, a tool to fetch and lock dependencies of OCaml projects

`mfetch` is a simple programme that allows you to download the source code for
the dependencies listed in an `_mfetch` file, based on the version available
under the current OPAM switch. You can ‘lock’ dependencies and ensure the
reproducibility of your programme using [`orb`][orb]. `mfetch` is a lighter
version of [`opam-monorepo`][opam-monorepo] and leaves dependency resolution
(which also involves version constraints) to [OPAM][opam].

This project forms part of the new workflow for developing unikernels and
enables the _vendoring_ of dependencies containing C files that must be
compiled with the Solo5 toolchain.

## Example

Let's take the [`immuable`][immuable] project as an example, a simple HTTP
server implemented as a unikernel.

```shell
$ git clone https://github.com/dinosaure/immuable
$ cd immuable
$ unic . > _mfetch
$ mfetch
$ mfetch lock
$ orb build
```

[opam-monorepo]: https://github.com/tarides/opam-monorepo
[orb]: https://github.com/robur-coop/orb
[opam]: https://opam.ocaml.org/
[immuable]: https://github.com/dinosaure/immuable
