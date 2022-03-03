open Session_ocaml.MonadicStyle
open Syntax

let srv () =
  let* cnt = receive in
  let rec loop i =
    if i = 0 then
      let* () = select (fun o -> o#right) in
      close
    else
      let* () = select (fun o -> o#left) in
      let* () = send i in
      loop (i - 1)
  in
  loop cnt

let cli () =
  let* () = send 10 in
  let rec loop () =
    offer
      (object
         method left =
           let* v = receive in
           Printf.printf "%d\n" v;
           loop ()

         method right = close
      end)
  in
  loop ()

let () =
  run_monad (fun () ->
      let* () = fork srv in
      cli ())
