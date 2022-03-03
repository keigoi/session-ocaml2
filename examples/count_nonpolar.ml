open Session_ocaml.NonPolar

let server ch =
  let open Server in
  let ch, cnt = receive ch in
  let rec loop ch i =
    if i = 0 then close (select (fun o -> `right o) ch)
    else loop (send i (select (fun o -> `left o) ch)) (i - 1)
  in
  loop ch cnt

let client ch =
  let ch = send 10 ch in
  let rec loop ch =
    match offer ch with
    | `left ch ->
        let ch, v = receive ch in
        Printf.printf "%d\n" v;
        loop ch
    | `right ch -> close ch
  in
  loop ch

let () =
  let cch = fork server in
  client cch
