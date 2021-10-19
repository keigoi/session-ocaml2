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
