(* Open the Picture and Transform modules *)
open Picture
open Transform
open Stack

(* Stack module
module Stack = struct
  type 'a t = 'a list

  let empty = []

  let is_empty stack = stack = []

  let push x stack = x :: stack

  let pop stack =
    match stack with
    | [] -> failwith "Empty stack"
    | x :: xs -> (x, xs)

  (* Global variable representing the current point *)
  let current_point : Picture.point ref = ref (0.0, 0.0)

  (* Variable representing the current sum of translations *)
  let current_transformation : Transform.transform ref = ref Transform.id

  (* Variable representing the starting point of a path of lines *)
  let starting_point : Picture.point ref = ref (0.0, 0.0)

  (* Variable representing the current picture *)
  let current_picture : Picture.picture ref = ref []

  (* Function to get the current point *)
  let get_current_point () = !current_point

  (* Function to set the current point *)
  let set_current_point p = current_point := p

  (* Function to get the current sum of translations *)
  let get_current_transformation () = !current_transformation

  (* Function to set the current sum of translations *)
  let set_current_transformation t = current_transformation := t

  (* Function to get the starting point of a path *)
  let get_starting_point () = !starting_point

  (* Function to set the starting point of a path *)
  let set_starting_point p = starting_point := p

  (* Function to get the current picture *)
  let get_current_picture () = !current_picture

  (* Function to set the current picture *)
  let set_current_picture pic = current_picture := pic

  (* MoveTo operation: Set the current point and starting point to the top two values on the stack *)
  let moveto stack =
    match pop stack with
    | (x, rest) ->
      match pop rest with
      | (y, new_stack) ->
        let target_point : Picture.point = (float_of_int x, float_of_int y) in
        let transformed_point = Transform.trpoint (get_current_transformation ()) target_point in
        set_current_point transformed_point;
        set_starting_point transformed_point;
        new_stack

  (* LineTo operation: Set the current point to the top two values on the stack and add a line to the picture *)
  let lineto stack =
    match pop stack with
    | (x, rest) ->
      match pop rest with
      | (y, new_stack) ->
        let target_point : Picture.point = (float_of_int x, float_of_int y) in
        let transformed_point = Transform.trpoint (get_current_transformation ()) target_point in
        let previous_point = get_current_point () in
        set_current_point transformed_point;
        let current_pic = get_current_picture () in
        set_current_picture (current_pic +++ Picture.line previous_point transformed_point);
        new_stack

  (* ClosePath operation: Add a line from current_point to starting_point to the picture *)
  let closepath () =
    let current = get_current_point () in
    let start = get_starting_point () in
    if current <> start then
      let current_pic = get_current_picture () in
      set_current_picture (current_pic +++ Picture.line current start)
    else
      ()

  (* Addition operation: Add the top two values on the stack *)
  let add stack =
    match pop stack with
    | (x, rest) ->
      match pop rest with
      | (y, new_stack) -> push (x + y) new_stack

  (* Subtraction operation: Subtract the top value from the second top value on the stack *)
  let sub stack =
    match pop stack with
    | (x, rest) ->
      match pop rest with
      | (y, new_stack) -> push (y - x) new_stack

  (* Multiplication operation: Multiply the top two values on the stack *)
  let mul stack =
    match pop stack with
    | (x, rest) ->
      match pop rest with
      | (y, new_stack) -> push (x * y) new_stack

  (* Division operation: Divide the second top value by the top value on the stack *)
  let div stack =
    match pop stack with
    | (x, rest) ->
      match pop rest with
      | (y, new_stack) ->
        if y = 0 then failwith "Division by zero"
        else push (y / x) new_stack

  (* Translate operation: Translate all further points by the vector represented by the top two values on the stack *)
  let translate stack =
    match pop stack with
    | (x, rest) ->
      match pop rest with
      | (y, new_stack) ->
        let translation_vector : Picture.vec = (float_of_int x, float_of_int y) in
        let current_transform = get_current_transformation () in
        let updated_transform = Transform.sum current_transform (Transform.translate translation_vector) in
        set_current_transformation updated_transform;
        new_stack

  (* Rotate operation: Rotate all further points by the angle represented by the top value on the stack (fraction of a full circle) *)
  let rotate stack =
    match pop stack with
    | (angle_fraction, new_stack) ->
      let full_circle = Transform.fullCircle in
      let angle = full_circle *. angle_fraction in
      let current_transform = get_current_transformation () in
      let updated_transform = Transform.sum current_transform (Transform.rotate angle) in
      set_current_transformation updated_transform;
      new_stack

  (* Function to add a Line to the current picture from the starting point to the current point *)
  let add_line_to_picture () =
    let line = Picture.line (get_starting_point ()) (get_current_point ()) in
    let current_pic = get_current_picture () in
    set_current_picture (current_pic +++ line)
end *)

(* Main function to read and perform operations *)
let rec process_input stack =
  match read_line () with
  | exception End_of_file -> stack
  | input ->
    let input_tokens = String.split_on_char ' ' input in
    match input_tokens with
    | op :: _ when op = "moveto" -> process_input (Stack.moveto stack)
    | op :: _ when op = "add" -> process_input (Stack.add stack)
    | op :: _ when op = "sub" -> process_input (Stack.sub stack)
    | op :: _ when op = "mul" -> process_input (Stack.mul stack)
    | op :: _ when op = "div" -> process_input (Stack.div stack)
    | _ -> process_input stack

    let () =
    let initial_stack = Stack.empty in
    let final_stack = process_input initial_stack in
    Printf.printf "Final Stack: %s\n"
      (if Stack.is_empty final_stack then "Empty" else Picture.string_of_point (Stack.get_current_point ()))
  