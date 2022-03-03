open! Session_ocaml.Monadic
open! Syntax

let main () =
  let* () =
    start_server (fun () ->
        let* x, y = receive in
        let* () = send (x <> y) in
        close)
  in
  let* () = send (false, true) in
  let* b = receive in
  Printf.printf "%b\n" b;
  close

let () = run_monad main
