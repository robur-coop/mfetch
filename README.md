# `mfetch`, a tool to fetch and lock dependencies of OCaml projects

`mfetch` is a simple tool which, using a `_mfetch` configuration file, allows
you to download the source code for OCaml projects. The tool does not perform
resolution in the strict sense; it simply retrieves the source code for
projects based on what [`opam`][opam] tells it, or as specified by the user.

![mfetch](./assets/mfetch.gif)

`mfetch` then creates (by default) a `vendors` folder. It can also append to a
`dune` file in the current directory from which it is run (`--with-dune-file`),
adding the directive `(vendored_dirs vendors)`. It then maintains a track in
this folder to indicate whether there have been any changes between the
downloaded version and the one currently in your directory.

Finally, it is possible to generate a reproducible file detailing what `mfetch`
does, in order to verify the reproducibility of a project that has been
compiled using these downloaded sources. This is notably what [`orb`][orb] uses
to verify the reproducibility of an OCaml project.

## A simple usage

Here is a simple example of how to use `mfetch`. Here, we create a
configuration file [`bcfg`][bcfg] called `_mfetch`, then download the source
code for [`digestif`][digestif] and build a simple executable using `dune`. In
this case, `dune` compiles your executable using the version of `digestif`
available in the `vendors/` folder (rather than the one available in your OPAM
switch). This is notably what we use for our unikernels in order to
cross-compile these projects with [`ocaml-solo5`][ocaml-solo5] and `dune`.

```sh
$ cat >_mfetch<<EOF
digestif
EOF
$ mfetch -q --with-dune-file
$ cat >main.ml<<EOF
let empty = Digestif.SHA1.digest_string ""
let () = Fmt.pr "%a\n%!" Digestif.SHA1.pp empty
EOF
$ cat >>dune<<EOF
(executable
 (name main)
 (libraries fmt digestif.c))
EOF
$ cat >dune-project<<EOF
(lang dune 3.0)
EOF
$ dune exec ./main.exe
da39a3ee5e6b4b0d3255bfef95601890afd80709
```

### Specify the sources

By default, `mfetch` refers to OPAM and the versions installed by OPAM: in
other words, `mfetch` **does not** resolve versions (unlike, for example,
[`opam-monorepo`][opam-monorepo]). This choice is driven by the inherent
complexity of determining _the correct versions_ of the packages we wish to
download within a constantly evolving ecosystem. We therefore delegate this
task to OPAM (as this is, first and foremost, its job).

However, it is possible to specify sources that do not necessarily come from
OPAM or [`opam-repository`][opam-repository]:

```scfg
# A simple OPAM package, we take it from what your switch says
digestif
# Projects from a Git repository
git@github.com:robur-coop/httpcats.git # through ssh
git+https://github.com/robur-coop/bcfg.git # through https
# Project from your disk
git+file:///home/dinosaure/dev/cattery # as a Git repository
file:///home/dinosaure/dev/miou # as a simple directory
# A tarball
https://github.com/mirage/Zarith/archive/refs/tags/1.14+dune+mirage1.tar.gz as zarith {
  sha256 915bd53ebb608729eb76b4e5bc0580090d8428fe919c9bf9a5dcd39f739b49f7
}
```

It should be noted that sources from your hard drive **cannot** be used to
_lock_ these sources.

### Lock sources

As these sources can contribute to the build of your project using `dune`, they
must be included as artefacts required to produce your executable (or your
unikernel) and help verify the reproducibility of your executable (or your
unikernel). In this case, our cooperative has developed the [`orb`][orb] tool
to verify a project’s reproducibility, and `mfetch` can contribute to this
verification using `mfetch lock`:

```sh
$ cat >_mfetch<<EOF
digestif
httpcats
EOF
$ mfetch -q
$ mfetch lock
2 sources locked into _mfetch.locked
```

The generated file is an OPAM file containing various details, such as the
location of the sources, their hashes (SHA-256 and SHA-512) and their versions
(as determined by OPAM). 

### The developer's workflow

As these source files are used to build your project, you may need to modify
them. `mfetch` provides a way of checking whether the source files have been
modified and, if so, notifies you so that you can _patch-back_ your changes to
_upstream_.

```sh
$ cat >_mfetch<<EOF
digestif
git@github.com:robur-coop/httpcats.git
EOF
$ mfetch -q
$ touch vendors/httpcats/foo
$ mfetch status
digestif                         ok
httpcats                         modified
$ git -C vendors/httpcats status -s
?? foo
```

This example demonstrates how to use `mfetch status`, specifically with a Git
repository. When `mfetch` downloads a project from a Git repository, it acts
like a `git clone` and creates a Git repository that can then track changes to
the project.

## License

It is licensed under the AGPL-3.0 or later. If you want to pay for a more
permissive license, reach out to team@robur.coop.

[opam-monorepo]: https://github.com/tarides/opam-monorepo
[orb]: https://github.com/robur-coop/orb
[opam]: https://opam.ocaml.org/
[immuable]: https://github.com/dinosaure/immuable
[ocaml-solo5]: https://github.com/mirage/ocaml-solo5
[opam-repository]: https://github.com/ocaml/opam-repository
[bcfg]: https://github.com/robur-coop/bcfg
[digestif]: https://github.com/mirage/digestif
