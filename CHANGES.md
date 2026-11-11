# v0.1.2 (2026-11-10)

- Be able to use `mfetch` into a Git repository of a project that we would also
  would like to fetch (@dinosaure, [!15][15])
- Rename `checksum` by `checksums` because it brings multiple checksums
  (@dinosaure, [!16][16])
- Be able to use a mirror instead of direct artifacts (like GitHub artifacts)
  (@hannesm, @dinosaure, [!14][14])
- Fix our `--mirror` option (the default behavior is to not use direct artifact)
  and be able to retrieve archives from a mirror (defaults to
  https://opam.ocaml.org/) (@dinosaure, @hannesm, [!18][18])

[14]: https://git.robur.coop/robur/mfetch/pulls/14
[15]: https://git.robur.coop/robur/mfetch/pulls/15
[16]: https://git.robur.coop/robur/mfetch/pulls/16
[18]: https://git.robur.coop/robur/mfetch/pulls/18

# v0.1.1 (2026-09-09)

- Handle smart version 1 protocol (@dinosaure, fix [#11][11], [!12][12])

[12]: https://git.robur.coop/robur/mfetch/pulls/12
[11]: https://git.robur.coop/robur/mfetch/issues/11

# v0.1.0 (2026-08-27)

- First release
