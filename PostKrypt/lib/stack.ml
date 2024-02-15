open Picture

module Stack = struct
  type t = float list

  let empty = []

  let is_empty stack = stack = []

  let push x stack = x :: stack

  let pop stack =
    match stack with
    | [] -> failwith "Empty stack"
    | x :: xs -> (x, xs)

  let moveto stack current_transform =
    match pop stack with
    | (x, rest) ->
      match pop rest with
      | (y, new_stack) ->
        let target_point : Picture.point = (x, y) in
        let transformed_point = Transform.trpoint current_transform target_point in
        transformed_point, new_stack

  let lineto stack current_point current_transform current_picture =
    match pop stack with
    | (x, rest) ->
      match pop rest with
      | (y, new_stack) ->
        let target_point : Picture.point = (x, y) in
        let transformed_point = Transform.trpoint current_transform target_point in
        let previous_point = current_point in
        let updated_picture = current_picture +++ Picture.line previous_point transformed_point in
        transformed_point, new_stack, updated_picture

  let closepath current_point starting_point current_picture =
    let current = current_point in
    let start = starting_point in
    if current <> start then
      let updated_picture = current_picture +++ Picture.line current start in
      updated_picture
    else
      current_picture

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
        else push (x /. y) new_stack

  let translate stack current_transform =
    match pop stack with
    | (x, rest) ->
      match pop rest with
      | (y, new_stack) ->
        let translation_vector : Picture.vec = (x, y) in
        let updated_transform = Transform.sum current_transform (Transform.translate translation_vector) in
        updated_transform, new_stack

  let rotate stack current_transform =
    match pop stack with
    | (angle_fraction, new_stack) ->
      let full_circle = Transform.fullCircle in
      let angle = full_circle *. angle_fraction in
      let updated_transform = Transform.sum current_transform (Transform.rotate angle) in
      updated_transform, new_stack

  let add_line_to_picture current_picture starting_point current_point =
    let line = Picture.line starting_point current_point in
    let updated_picture = current_picture +++ line in
    updated_picture
end