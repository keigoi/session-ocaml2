open Domainslib

type 'p data =
    Msg : ('v * 'p data Domainslib.Chan.t) -> [`msg of 'r * 'v * 'p] data
  | Branch : ('br -> 'p data Domainslib.Chan.t  -> unit) * 'p data Domainslib.Chan.t  -> [`branch of 'r * 'br] data
  (* | Chan : (('pp, 'rr) sess * 'p data Domainslib.Chan.t) -> [`deleg of 'r * ('pp, 'rr) sess * 'p] data *)
and ('p, 'q) sess = 'p data Domainslib.Chan.t * 'q

type req = Req and resp = Resp
type cli = req * resp
type srv = resp * req

let send v (ch,q) =
  let ch' = Chan.make_unbounded () in
  Chan.send ch (Msg(v,ch'));
  (ch',q)

let receive (ch,q) =
  let Msg(v,ch') = Chan.recv ch in
  (ch',q), v

let select f (ch,(r1,r2)) =
  let ch' = Chan.make_unbounded () in
  Chan.send ch (Branch ((fun obj p -> f obj (p,(r2,r1))), ch'));
  (ch',(r1,r2))

let offer (ch,_) obj : unit =
  let Branch (f,ch') = Chan.recv ch in
  f obj ch'

let close _ = ()

let new_session () =
  let ch = Chan.make_unbounded () in
  (ch,(Req,Resp)), (ch,(Resp,Req))

module Monadic = struct
  
  type ('p,'q,'a) monad = 'p -> 'q * 'a

  let bind m f p = 
    let q, a = m p in 
    f a q

  let return x p = p, x

  let run_monad f = 
    let (), a = f () ()
    in a

  type 'p data =
      Msg : ('v * 'p data Domainslib.Chan.t) -> [`msg of 'r * 'v * 'p] data
    | Branch : (('br -> ('p data Domainslib.Chan.t, unit, unit) monad) * 'p data Domainslib.Chan.t)  -> [`branch of 'r * 'br] data
  and ('p, 'q) sess = 'p data Domainslib.Chan.t * 'q

  let send v (ch,q) =
    let ch' = Chan.make_unbounded () in
    (ch',q), Chan.send ch (Msg(v,ch'))
  
  let receive (ch,q) =
    let Msg(v,ch') = Chan.recv ch in
    (ch',q), v

  let select f (ch,(r1,r2)) =
    let ch' = Chan.make_unbounded () in
    Chan.send ch (Branch((fun obj p -> f obj (p,(r2,r1))), ch'));
    (ch',(r1,r2)), ()

  let offer obj (ch,_) =
    let Branch(f, ch') = Chan.recv ch in
    f obj ch'

  let close _ = (), ()

  let start_server f () =
    let cli, srv = new_session () in
    ignore (Thread.create (fun srv -> snd (f () srv)) srv);
    cli, ()

  module Syntax = struct
    let (let*) = bind
  end
end