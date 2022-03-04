open Domainslib

type req = Req
and resp = Resp

type cli = req * resp
type srv = resp * req

type 'p data =
  | Msg : ('v * 'p data Domainslib.Chan.t) -> [ `msg of 'r * 'v * 'p ] data
  | Branch : 'br -> [ `branch of 'r * 'br ] data

(* | Chan : (('pp, 'rr) sess * 'p data Domainslib.Chan.t) -> [`deleg of 'r * ('pp, 'rr) sess * 'p] data *)
and ('p, 'q) sess = 'p data Domainslib.Chan.t * 'q

let send v (ch, q) =
  let ch' = Chan.make_unbounded () in
  Chan.send ch (Msg (v, ch'));
  (ch', q)

let receive (ch, q) =
  let (Msg (v, ch')) = Chan.recv ch in
  ((ch', q), v)

let select f (ch, (r1, r2)) =
  let ch' = Chan.make_unbounded () in
  Chan.send ch (Branch (f (ch', (r2, r1))));
  (ch', (r1, r2))

let offer (ch, _) =
  let (Branch var) = Chan.recv ch in
  var

let close _ = ()

module Channel = struct
  let new_session () =
    let ch = Chan.make_unbounded () in
    ((ch, (Req, Resp)), (ch, (Resp, Req)))
end

let fork f =
  let cli, srv = Channel.new_session () in
  let (_ : Thread.t) = Thread.create f srv in
  cli

module CallbackStyle = struct
  type 'p data =
    | Msg : ('v * 'p data Domainslib.Chan.t) -> [ `msg of 'r * 'v * 'p ] data
    | Branch :
        ('br -> 'p data Domainslib.Chan.t -> unit) * 'p data Domainslib.Chan.t
        -> [ `branch of 'r * 'br ] data

  (* | Chan : (('pp, 'rr) sess * 'p data Domainslib.Chan.t) -> [`deleg of 'r * ('pp, 'rr) sess * 'p] data *)
  and ('p, 'q) sess = 'p data Domainslib.Chan.t * 'q

  let send v (ch, q) =
    let ch' = Chan.make_unbounded () in
    Chan.send ch (Msg (v, ch'));
    (ch', q)

  let receive (ch, q) =
    let (Msg (v, ch')) = Chan.recv ch in
    ((ch', q), v)

  let select f (ch, (r1, r2)) =
    let ch' = Chan.make_unbounded () in
    Chan.send ch (Branch ((fun obj p -> f obj (p, (r2, r1))), ch'));
    (ch', (r1, r2))

  let offer (ch, _) obj : unit =
    let (Branch (f, ch')) = Chan.recv ch in
    f obj ch'

  let close _ = ()

  module Channel = struct
    let new_session () =
      let ch = Chan.make_unbounded () in
      ((ch, (Req, Resp)), (ch, (Resp, Req)))
  end

  let fork f =
    let cli, srv = Channel.new_session () in
    let (_ : Thread.t) = Thread.create f srv in
    cli
end

module MonadicStyle = struct
  type ('p, 'q, 'a) monad = 'p -> 'q * 'a

  let bind m f p =
    let q, a = m p in
    f a q

  let return x p = (p, x)

  let run_monad f =
    let (), a = f () () in
    a

  type 'p data =
    | Msg : ('v * 'p data Domainslib.Chan.t) -> [ `msg of 'r * 'v * 'p ] data
    | Branch :
        (('br -> ('p data Domainslib.Chan.t, unit, unit) monad)
        * 'p data Domainslib.Chan.t)
        -> [ `branch of 'r * 'br ] data

  and ('p, 'q) sess = 'p data Domainslib.Chan.t * 'q

  let send v (ch, q) =
    let ch' = Chan.make_unbounded () in
    ((ch', q), Chan.send ch (Msg (v, ch')))

  let receive (ch, q) =
    let (Msg (v, ch')) = Chan.recv ch in
    ((ch', q), v)

  let select f (ch, (r1, r2)) =
    let ch' = Chan.make_unbounded () in
    Chan.send ch (Branch ((fun obj p -> f obj (p, (r2, r1))), ch'));
    ((ch', (r1, r2)), ())

  let offer obj (ch, _) =
    let (Branch (f, ch')) = Chan.recv ch in
    f obj ch'

  let close _ = ((), ())

  let fork f () =
    let cli, srv = CallbackStyle.Channel.new_session () in
    ignore (Thread.create (fun srv -> snd (f () srv)) srv);
    (cli, ())

  module Syntax = struct
    let ( let* ) = bind
  end
end

module NonPolar = struct
  type 'p data =
    | Send : ('v * 'p data Domainslib.Chan.t) -> [ `send of 'v * 'p ] data
    | Select : 'br -> [ `select of 'br ] data
    | Recv : ('v * 'p data Domainslib.Chan.t) -> [ `recv of 'v * 'p ] data
    | Branch : 'br -> [ `branch of 'br ] data

  and 'p cli = 'p data Domainslib.Chan.t
  and 'p srv = 'p data Domainslib.Chan.t

  let send v ch =
    let ch' = Chan.make_unbounded () in
    Chan.send ch (Send (v, ch'));
    ch'

  let receive ch =
    let (Recv (v, ch')) = Chan.recv ch in
    (ch', v)

  let select f ch =
    let ch' = Chan.make_unbounded () in
    Chan.send ch (Select (f ch'));
    ch'

  let offer ch =
    let (Branch var) = Chan.recv ch in
    var

  let close _ = ()

  module Channel = struct
    let new_session () =
      let ch = Chan.make_unbounded () in
      (ch, ch)
  end

  let fork f =
    let cli, srv = Channel.new_session () in
    let (_ : Thread.t) = Thread.create f srv in
    cli

  module Server = struct
    let send v ch =
      let ch' = Chan.make_unbounded () in
      Chan.send ch (Recv (v, ch'));
      ch'

    let receive ch =
      let (Send (v, ch')) = Chan.recv ch in
      (ch', v)

    let select f ch =
      let ch' = Chan.make_unbounded () in
      Chan.send ch (Branch (f ch'));
      ch'

    let offer ch =
      let (Select var) = Chan.recv ch in
      var

    let close _ = ()
  end
end