type reporter = {
  report : int -> unit;
  total : int option -> unit;
  finally : unit -> unit;
}

type t = { add : string -> reporter; finally : unit -> unit }

let ignore_reporter = { report = ignore; total = ignore; finally = ignore }
let ignore_progress = { add = (fun _ -> ignore_reporter); finally = ignore }

let frames () =
  if Fmt.utf_8 Fmt.stderr
  then [ "⠋"; "⠙"; "⠹"; "⠸"; "⠼"; "⠴"; "⠦"; "⠧"; "⠇"; "⠏" ]
  else [ "|"; "/"; "-"; "\\" ]

let to_size = Progress.Printer.to_to_string Progress.Units.Bytes.of_int
let bytes_width = Progress.Printer.print_width Progress.Units.Bytes.of_int
let rate_width = bytes_width + 3
let max_bar_width = 20
let min_bar_width = 5
let large_width = 26 + 1 + bytes_width + 4 + bytes_width + rate_width
let small_width = 18 + 1 + bytes_width
let is_small columns = columns < large_width + min_bar_width

let bar_width ~columns =
  let available =
    columns - if is_small columns then small_width else large_width in
  Int.max min_bar_width (Int.min max_bar_width available)

let line_width ~columns =
  (if is_small columns then small_width else large_width) + bar_width ~columns

let truncate len str =
  if String.length str <= len then str else String.sub str 0 len

let name_line ~columns name =
  let open Progress.Line in
  if is_small columns
  then constf " %-16s " (truncate 16 name)
  else constf " %-24s " (truncate 24 name)

let spinner_line ~columns name =
  let open Progress.Line in
  let rate = if is_small columns then noop () else const " " ++ bytes_per_sec in
  rpad (line_width ~columns)
    (name_line ~columns name
    ++ spinner ~frames:(frames ()) ()
    ++ const " "
    ++ bytes
    ++ rate)

let bar_line ~columns ~total name =
  let open Progress.Line in
  let style = if Fmt.utf_8 Fmt.stderr then `UTF8 else `ASCII in
  let rate =
    if is_small columns
    then noop ()
    else constf " / %s " (to_size total) ++ bytes_per_sec in
  rpad (line_width ~columns)
    (name_line ~columns name
    ++ bar ~style ~width:(`Fixed (bar_width ~columns)) total
    ++ const " "
    ++ bytes
    ++ rate)

let done_line ~columns name =
  let open Progress.Line in
  let mark = if Fmt.utf_8 Fmt.stderr then "✔" else "*" in
  rpad (line_width ~columns)
    (name_line ~columns name ++ const mark ++ const " " ++ bytes)

type entry = {
  name : string;
  mutable line : int Progress.Reporter.t;
  mutable sofar : int;
  mutable switched : bool;
  mutable done' : bool;
}

let set_interject mutex =
  let run fn = Mutex.protect mutex @@ fun () -> Progress.interject_with fn in
  Mfetch_cli.interject := { Mfetch_cli.run }

let make ~config ~columns = function
  | false -> ignore_progress
  | true ->
      let display = Progress.Display.start ~config Progress.Multi.blank in
      let mutex = Mutex.create () in
      let entries = ref [] in
      let below entry =
        let rec after = function
          | [] -> []
          | e :: rest -> if e == entry then rest else after rest in
        1 + List.length (after !entries) in
      let replace entry line =
        let above = below entry in
        Progress.Display.remove_line display entry.line ;
        let line = Progress.Display.add_line ~above display line in
        Progress.Reporter.report line entry.sofar ;
        entry.line <- line in
      let previous = !Mfetch_cli.interject in
      set_interject mutex ;
      let add name =
        let entry =
          Mutex.protect mutex @@ fun () ->
          let line =
            Progress.Display.add_line ~above:1 display
              (spinner_line ~columns name) in
          let entry =
            { name; line; sofar = 0; switched = false; done' = false } in
          entries := !entries @ [ entry ] ;
          entry in
        let report len =
          Mutex.protect mutex @@ fun () ->
          if not entry.done'
          then begin
            entry.sofar <- entry.sofar + len ;
            Progress.Reporter.report entry.line len
          end in
        let total = function
          | None -> ()
          | Some total ->
              Mutex.protect mutex @@ fun () ->
              if (not entry.done') && (not entry.switched) && total > 0
              then begin
                entry.switched <- true ;
                replace entry (bar_line ~columns ~total entry.name)
              end in
        let finally () =
          Mutex.protect mutex @@ fun () ->
          if not entry.done'
          then begin
            entry.done' <- true ;
            if not entry.switched
            then replace entry (done_line ~columns entry.name) ;
            Progress.Reporter.finalise entry.line
          end in
        { report; total; finally } in
      let finalised = ref false in
      let finally () =
        Mutex.protect mutex @@ fun () ->
        if not !finalised
        then begin
          finalised := true ;
          Progress.Display.finalise display ;
          Mfetch_cli.interject := previous
        end in
      { add; finally }
