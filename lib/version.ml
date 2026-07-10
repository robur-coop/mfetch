let is_digit = function '0' .. '9' -> true | _ -> false

let order = function
  | '~' -> -1
  | ('a' .. 'z' | 'A' .. 'Z') as chr -> Char.code chr
  | chr -> Char.code chr + 0x100

let compare a b =
  let la = String.length a and lb = String.length b in
  let rec go i j = if i >= la && j >= lb then 0 else nondigit i j
  and nondigit i j =
    let ca = if i < la && not (is_digit a.[i]) then Some a.[i] else None
    and cb = if i < lb && not (is_digit b.[j]) then Some b.[j] else None in
    match (ca, cb) with
    | None, None -> digits i j
    | _ ->
        let va = Option.map order ca |> Option.value ~default:0
        and vb = Option.map order cb |> Option.value ~default:0 in
        if va <> vb then Int.compare va vb else nondigit (i + 1) (j + 1)
  and digits i j =
    let rec skip str len k =
      if k < len && str.[k] = '0' then skip str len (k + 1) else k in
    let rec until str len k =
      if k < len && is_digit str.[k] then until str len (k + 1) else k in
    let i' = skip a la i and j' = skip b lb j in
    let ea = until a la i' and eb = until b lb j' in
    if ea - i' <> eb - j
    then Int.compare (ea - i') (eb - j')
    else
      let v = String.(compare (sub a i' (ea - i')) (sub b j' (eb - j'))) in
      if v <> 0 then v else go ea eb in
  go 0 0
