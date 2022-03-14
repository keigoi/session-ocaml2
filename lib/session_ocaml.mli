type req
and resp

type cli = req * resp
type srv = resp * req
type ('p, 'q) sess

val send :
  'v -> ([ `msg of 'r1 * 'v * 'p ], 'r1 * 'r2) sess -> ('p, 'r1 * 'r2) sess

val receive :
  ([ `msg of 'r2 * 'v * 'p ], 'r1 * 'r2) sess -> 'v * ('p, 'r1 * 'r2) sess

val select :
  (('p, 'r2 * 'r1) sess -> 'var) ->
  ([ `branch of 'r1 * 'var ], 'r1 * 'r2) sess ->
  ('p, 'r1 * 'r2) sess

val offer : ([ `branch of 'r2 * 'var ], 'r1 * 'r2) sess -> 'var
val close : (unit, 'r1 * 'r2) sess -> unit
val fork : (('p, srv) sess -> unit) -> ('p, cli) sess

module Channel : sig
  val new_session : unit -> ('p, cli) sess * ('p, srv) sess
end

module CallbackStyle : sig
  type ('p, 'q) sess

  val send :
    'v -> ([ `msg of 'r1 * 'v * 'p ], 'r1 * 'r2) sess -> ('p, 'r1 * 'r2) sess

  val receive :
    ([ `msg of 'r2 * 'v * 'p ], 'r1 * 'r2) sess -> 'v * ('p, 'r1 * 'r2) sess

  val select :
    ('obj -> ('p, 'r2 * 'r1) sess -> unit) ->
    ([ `branch of 'r1 * 'obj ], 'r1 * 'r2) sess ->
    ('p, 'r1 * 'r2) sess

  val offer : ([ `branch of 'r2 * 'obj ], 'r1 * 'r2) sess -> 'obj -> unit
  val close : (unit, 'r1 * 'r2) sess -> unit
  val fork : (('p, srv) sess -> unit) -> ('p, cli) sess

  module Channel : sig
    val new_session : unit -> ('p, cli) sess * ('p, srv) sess
  end
end

module MonadicStyle : sig
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

  val fork :
    (unit -> (('p, srv) sess, unit, unit) monad) ->
    (unit, ('p, cli) sess, unit) monad

  module Syntax : sig
    val ( let* ) :
      ('p, 'q, 'a) monad -> ('a -> ('q, 'r, 'b) monad) -> ('p, 'r, 'b) monad
  end
end

module NonPolar : sig
  type 'p cli
  type 'p srv

  val send : 'v -> [ `send of 'v * 'p ] cli -> 'p cli
  val send_and_forward : 'v -> 'p srv -> [ `send of 'v * 'p ] cli -> unit
  val receive : [ `recv of 'v * 'p ] cli -> 'v * 'p cli
  val select : ('p srv -> 'var) -> [ `select of 'var ] cli -> 'p cli

  val select_and_forward :
    ('p srv -> 'var) -> 'p srv -> [ `select of 'var ] cli -> unit

  val offer : [ `branch of 'var ] cli -> 'var
  val close : unit cli -> unit
  val fork : ('p srv -> unit) -> 'p cli

  module Server : sig
    val send : 'v -> [ `recv of 'v * 'p ] srv -> 'p srv
    val send_and_forward : 'v -> 'p cli -> [ `recv of 'v * 'p ] srv -> unit
    val receive : [ `send of 'v * 'p ] srv -> 'v * 'p srv
    val select : ('p cli -> 'var) -> [ `branch of 'var ] srv -> 'p srv

    val select_and_forward :
      ('p cli -> 'var) -> 'p cli -> [ `branch of 'var ] srv -> unit

    val offer : [ `select of 'var ] srv -> 'var
    val close : unit srv -> unit
  end
end
