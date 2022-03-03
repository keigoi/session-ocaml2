open! Session_ocaml.MonadicStyle
open! Syntax

let main () =
  let* () =
    fork (fun () ->
        let* x, y = receive in
        let* () = send (x <> y) in
        close)
  in
  let* () = send (false, true) in
  let* b = receive in
  Printf.printf "%b\n" b;
  close

let () = run_monad main
