type t = float list

val empty : t

val is_empty : t -> bool

val push : float -> t -> t

val pop : t -> float * t

val current_point : Picture.point ref

val current_transform : Transform.transform ref

val starting_point : Picture.point ref

val current_picture : Picture.picture ref

val get_current_point : unit -> Picture.point

val set_current_point : Picture.point -> unit

val get_current_transform : unit -> Transform.transform

val set_current_transform : Transform.transform -> unit

val get_starting_point : unit -> Picture.point

val set_starting_point : Picture.point -> unit

val get_current_picture : unit -> Picture.picture

val set_current_picture : Picture.picture -> unit

val moveto : t -> t

val lineto : t -> t

val closepath : unit -> unit

val add : t -> t

val sub : t -> t

val mul : t -> t

val div : t -> t

val translate : t -> t

val rotate : t -> t

val add_line_to_picture : unit -> unit