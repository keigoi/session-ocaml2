open Session_ocaml

let () =
  let cch, sch = new_session () in
  let _t =
    Thread.create
      (fun ch ->
        let ch, cnt = receive ch in
        let rec loop ch i =
          if i = 0 then close (select (fun o -> o#right) ch)
          else loop (send i (select (fun o -> o#left) ch)) (i - 1)
        in
        loop ch cnt)
      sch
  in
  let client ch =
    let ch = send 10 ch in
    let rec loop ch =
      offer ch
        (object
           method left ch =
             let ch, v = receive ch in
             Printf.printf "%d\n" v;
             loop ch

           method right ch = close ch
        end)
    in
    loop ch
  in
  client cch
