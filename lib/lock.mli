(** The lock file: what every vendored directory must be filled with, named in a
    way which does not depend on the machine that produced it.

    Where [_mfetch] says "the package [x509]" - whose answer depends on the opam
    root it is resolved against, and therefore on the day - the lock file says
    "this archive, with these checksums" or "this Git repository, at this
    commit".

    It is written in the opam file syntax, in three fields:

    {v
      x-mfetch-target:        "vendors"
      x-mfetch-vendored-dirs: [ [ <url> <dir> [ <checksum> ... ] ] ... ]
      x-mfetch-endpoints:     [ [ <dir> <version> [ <spec> ... ] ] ... ]
    v}

    [x-mfetch-vendored-dirs] is the contract: it has the very same shape as
    opam-monorepo's [x-opam-monorepo-duniverse-dirs], so a tool which already
    knows how to pull those - {{:https://github.com/robur-coop/orb} orb}, to
    check that a build is reproducible - needs no new code to pull these.
    [x-mfetch-endpoints] is provenance only: which specifications of [_mfetch]
    landed into which directory, and at which version. *)

type entry = {
  target : string;  (** Sub-directory of the target directory. *)
  url : string;
      (** An opam URL: the URL of an archive, or [git+<uri>#<commit>]. *)
  checksum : Opam.hash list;  (** Empty for Git sources. *)
  version : string option;  (** The opam version, when it comes from one. *)
  edns : Edn.t list;  (** Provenance, possibly empty. *)
}

type t = { target : string; entries : entry list }

type ls_remote =
  ?branch:string ->
  [ `HTTP of string | `SSH of Git.ssh | `Local of Fpath.t ] ->
  (Carton.Uid.t, [ `Msg of string ]) result

val of_jobs :
  target:string ->
  ls_remote:ls_remote ->
  Resolve.entry list ->
  (t, [ `Msg of string ]) result

val to_jobs : t -> (Resolve.entry list, [ `Msg of string ]) result
val save : Fpath.t -> t -> (unit, [ `Msg of string ]) result
val load : Fpath.t -> (t, [ `Msg of string ]) result
