type handler = string list -> (string * string) list -> unit
type routes

val import: (string * handler) list -> routes
val handle: routes -> string -> bool
