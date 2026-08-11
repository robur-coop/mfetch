type action =
  | Download of {
      uri : string;
      archive : Edn.archive;
      checksum : Opam.hash list;
      version : string option;
      source : string;
    }
  | Clone of {
      remote : [ `HTTP of string | `SSH of Git.ssh | `Local of Fpath.t ];
      branch : string option;
      origin : string;
    }
  | Copy of { dirpath : Fpath.t }

type job = {
  target : string;
  edns : Edn.t list;
  action : action;
  name : string option;
}

type entry =
  | Job of job
  | Unresolved of { target : string; msg : string; name : string option }

val action_of_edn : root:Fpath.t -> Edn.t -> (action, [ `Msg of string ]) result

val action_of_url :
  checksum:Opam.hash list ->
  version:string option ->
  string ->
  (action * Edn.t, [ `Msg of string ]) result

val coalesce : root:Fpath.t -> Edn.t list -> entry list
