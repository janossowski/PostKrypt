open Picture

type t = float list

let empty = []

let is_empty stack = stack = []

let push x stack = x :: stack

let pop stack =
  match stack with
  | [] -> failwith "Empty stack"
  | x :: xs -> (x, xs)

let current_point : Picture.point ref = ref (0.0, 0.0)
let current_transform : Transform.transform ref = ref Transform.id
let starting_point : Picture.point ref = ref (0.0, 0.0)
let current_picture : Picture.picture ref = ref []

let get_current_point () = !current_point
let set_current_point p = current_point := p

let get_current_transform () = !current_transform
let set_current_transform t = current_transform := t

let get_starting_point () = !starting_point
let set_starting_point p = starting_point := p

let get_current_picture () = !current_picture
let set_current_picture pic = current_picture := pic

let moveto stack =
  match pop stack with
  | (x, rest) ->
    match pop rest with
    | (y, new_stack) ->
      let target_point : Picture.point = (x, y) in
      let transformed_point = Transform.trpoint (get_current_transform ()) target_point in
      set_current_point transformed_point;
      set_starting_point transformed_point;
      new_stack

let lineto stack =
  match pop stack with
  | (x, rest) ->
    match pop rest with
    | (y, new_stack) ->
      let target_point : Picture.point = (x, y) in
      let transformed_point = Transform.trpoint (get_current_transform ()) target_point in
      let previous_point = get_current_point () in
      set_current_point transformed_point;
      let current_pic = get_current_picture () in
      set_current_picture (current_pic +++ Picture.line previous_point transformed_point);
      new_stack

let closepath () =
  let current = get_current_point () in
  let start = get_starting_point () in
  if current <> start then
    let current_pic = get_current_picture () in
    set_current_picture (current_pic +++ Picture.line current start)
  else
    ()

let add stack =
  match pop stack with
  | (x, rest) ->
    match pop rest with
    | (y, new_stack) -> push (x +. y) new_stack

let sub stack =
  match pop stack with
  | (x, rest) ->
    match pop rest with
    | (y, new_stack) -> push (y -. x) new_stack

let mul stack =
  match pop stack with
  | (x, rest) ->
    match pop rest with
    | (y, new_stack) -> push (x *. y) new_stack

let div stack =
  match pop stack with
  | (x, rest) ->
    match pop rest with
    | (y, new_stack) ->
      if y = 0.0 then failwith "Division by zero"
      else push (y /. x) new_stack

let translate stack =
  match pop stack with
  | (x, rest) ->
    match pop rest with
    | (y, new_stack) ->
      let translation_vector : Picture.vec = (x, y) in
      let current_transform = get_current_transform () in
      let updated_transform = Transform.sum current_transform (Transform.translate translation_vector) in
      set_current_transform updated_transform;
      new_stack

let rotate stack =
  match pop stack with
  | (angle_fraction, new_stack) ->
    let full_circle = Transform.fullCircle in
    let angle = full_circle *. angle_fraction in
    let current_transform = get_current_transform () in
    let updated_transform = Transform.sum current_transform (Transform.rotate angle) in
    set_current_transform updated_transform;
    new_stack

let add_line_to_picture () =
  let line = Picture.line (get_starting_point ()) (get_current_point ()) in
  let current_pic = get_current_picture () in
  set_current_picture (current_pic +++ line)