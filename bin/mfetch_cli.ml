type interject = { run : 'a. (unit -> 'a) -> 'a }

let interject = ref { run = (fun fn -> fn ()) }
