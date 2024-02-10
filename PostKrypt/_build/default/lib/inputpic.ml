(* inputpic.ml *)

open Picture
open Stack
open Scanf

(* Token type for integers and operation names *)
type token =
  | Int of int
  | Op of string

(* Read a token from the input channel *)
let input_token input_channel =
  try
    let line = input_line input_channel in
    sscanf line "%s" (fun token_str ->
      try
        Int (int_of_string token_str)
      with
      | Failure _ -> Op token_str
    )
  with
  | End_of_file -> Op "EOF"

(* Read a series of integers and operation names from the input channel,
   perform the relevant operations using a new stack, and return
   the resulting picture. *)
let readPic input_channel =
  let rec process_tokens stack tokens =
    match tokens with
    | [] -> Stack.get_current_picture
    | token :: rest ->
      let new_stack = match token with
        | Int n -> Stack.push (float_of_int n) stack;
        | Op op ->
          (match op with
          | "moveto" -> Stack.moveto stack
          | "lineto" -> Stack.lineto stack
          | "closepath" -> Stack.closepath (); stack
          | "add" -> Stack.add stack
          | "sub" -> Stack.sub stack
          | "mul" -> Stack.mul stack
          | "div" -> Stack.div stack
          | "translate" -> Stack.translate stack
          | "rotate" -> Stack.rotate stack
          | _ -> failwith "Invalid operation");
      in
      process_tokens new_stack rest
  in
  let tokens = ref [] in
  let rec tokenize_input () =
    match input_token input_channel with
    | Op "EOF" -> process_tokens Stack.empty !tokens
    | token ->
      tokens := token :: !tokens;
      tokenize_input ()
  in
  tokenize_input ()
