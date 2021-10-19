type ('p, 'q) sess

type req and resp

type cli = req * resp
type srv = resp * req

val send : 'v -> ([`msg of 'r1 * 'v * 'p], 'r1*'r2) sess -> ('p,'r1*'r2) sess
val receive : ([`msg of 'r2 * 'v * 'p], 'r1*'r2) sess -> ('p,'r1*'r2) sess * 'v
val select : ('obj -> ('p,'r2*'r1) sess -> unit) -> ([`branch of 'r1 * 'obj], 'r1*'r2) sess -> ('p, 'r1*'r2) sess
val offer : ([`branch of 'r2 * 'obj], 'r1*'r2) sess -> 'obj -> unit
val close : (unit, 'r1*'r2) sess -> unit
val new_session : unit -> ('p,cli) sess * ('p,srv) sess
