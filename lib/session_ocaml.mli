type ('p, 'q) sess

type req
and resp

type cli = req * resp
type srv = resp * req

val send :
  'v -> ([ `msg of 'r1 * 'v * 'p ], 'r1 * 'r2) sess -> ('p, 'r1 * 'r2) sess

val receive :
  ([ `msg of 'r2 * 'v * 'p ], 'r1 * 'r2) sess -> ('p, 'r1 * 'r2) sess * 'v

val select :
  ('obj -> ('p, 'r2 * 'r1) sess -> unit) ->
  ([ `branch of 'r1 * 'obj ], 'r1 * 'r2) sess ->
  ('p, 'r1 * 'r2) sess

val offer : ([ `branch of 'r2 * 'obj ], 'r1 * 'r2) sess -> 'obj -> unit
val close : (unit, 'r1 * 'r2) sess -> unit
val new_session : unit -> ('p, cli) sess * ('p, srv) sess

module Monadic : sig
  type ('p, 'q, 'a) monad

  val bind :
    ('p, 'q, 'a) monad -> ('a -> ('q, 'r, 'b) monad) -> ('p, 'r, 'b) monad

  val return : 'a -> ('p, 'p, 'a) monad
  val run_monad : (unit -> (unit, unit, 'a) monad) -> 'a

  type ('p, 'q) sess

  val send :
    'v ->
    ( ([ `msg of 'r1 * 'v * 'p ], 'r1 * 'r2) sess,
      ('p, 'r1 * 'r2) sess,
      unit )
    monad

  val receive :
    ( ([ `msg of 'r2 * 'v * 'p ], 'r1 * 'r2) sess,
      ('p, 'r1 * 'r2) sess,
      'v )
    monad

  val select :
    ('obj -> (('p, 'r2 * 'r1) sess, unit, unit) monad) ->
    ( ([ `branch of 'r1 * 'obj ], 'r1 * 'r2) sess,
      ('p, 'r1 * 'r2) sess,
      unit )
    monad

  val offer :
    'obj -> (([ `branch of 'r2 * 'obj ], 'r1 * 'r2) sess, unit, unit) monad

  val close : ((unit, 'r1 * 'r2) sess, unit, unit) monad

  val start_server :
    (unit -> (('p, srv) sess, unit, unit) monad) ->
    (unit, ('p, cli) sess, unit) monad

  module Syntax : sig
    val ( let* ) :
      ('p, 'q, 'a) monad -> ('a -> ('q, 'r, 'b) monad) -> ('p, 'r, 'b) monad
  end
end
