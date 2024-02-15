(** This module provides operations for manipulating a stack of floats
    and performing transformations on pictures. *)

    module Stack : sig
        (** The signature of the Stack module *)
      
        type t = float list
        (** The type of the stack *)
      
        val empty : t
        (** An empty stack *)
      
        val is_empty : t -> bool
        (** Check if the stack is empty *)
      
        val push : float -> t -> t
        (** Push a float onto the stack *)
      
        val pop : t -> float * t
        (** Pop a float from the stack, raising an exception if the stack is empty *)
      
        val moveto : t -> Transform.transform -> Picture.point * t
        (** Perform a move-to operation on the stack, updating the current point *)
      
        val lineto : t -> Picture.point -> Transform.transform -> Picture.picture ->
                     Picture.point * t * Picture.picture
        (** Perform a line-to operation on the stack, updating the current point and picture *)
      
        val closepath : Picture.point -> Picture.point -> Picture.picture -> Picture.picture
        (** Perform a close-path operation on the stack, updating the current picture *)
      
        val add : t -> t
        (** Perform an addition operation on the top two elements of the stack *)
      
        val sub : t -> t
        (** Perform a subtraction operation on the top two elements of the stack *)
      
        val mul : t -> t
        (** Perform a multiplication operation on the top two elements of the stack *)
      
        val div : t -> t
        (** Perform a division operation on the top two elements of the stack *)
      
        val translate : t -> Transform.transform -> Transform.transform * t
        (** Perform a translation operation on the stack, updating the current transform *)
      
        val rotate : t -> Transform.transform -> Transform.transform * t
        (** Perform a rotation operation on the stack, updating the current transform *)
      
        val add_line_to_picture : Picture.picture -> Picture.point -> Picture.point -> Picture.picture
        (** Add a line to the current picture, connecting two points *)
      end
      