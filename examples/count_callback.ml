open Session_ocaml.CallbackStyle

let () =
  let ch =
    fork (fun ch ->
        let cnt, ch = receive ch in
        let rec loop ch i =
          if i = 0 then close (select (fun o -> o#right) ch)
          else loop (send i (select (fun o -> o#left) ch)) (i - 1)
        in
        loop ch cnt)
  in
  let ch = send 10 ch in
  let rec loop ch =
    offer ch
      (object
         method left ch =
           let v, ch = receive ch in
           Printf.printf "%d\n" v;
           loop ch

         method right ch = close ch
      end)
  in
  loop ch
