open Stack

let readPic channel =
  let current_transform = ref Transform.id in
  let current_picture = ref [] in
  let current_stack = ref Stack.empty in
  let path_in_progress = ref false in
  let start_point = ref (0., 0.) in
  let current_point = ref (0., 0.) in
  
  let process_tokens tokens =
    List.iter (fun token ->
      match token with
      | "moveto" ->
        let (transformed_point, new_stack) = Stack.moveto !current_stack !current_transform in
        path_in_progress := true;
        current_point := transformed_point;
        current_stack := new_stack
      | "lineto" ->
        if !path_in_progress = false
          then failwith "Path is empty!"
        else
          let (transformed_point, new_stack, updated_picture) =
            Stack.lineto !current_stack !current_point !current_transform !current_picture in
          current_point := transformed_point;
          current_stack := new_stack;
          current_picture := updated_picture
      | "closepath" ->
        current_picture := Stack.closepath !current_point !start_point !current_picture;
        path_in_progress := false
      | "translate" ->
        let (updated_transform, new_stack) = Stack.translate !current_stack !current_transform in
        current_transform := updated_transform;
        current_stack := new_stack
      | "rotate" ->
        let (updated_transform, new_stack) = Stack.rotate !current_stack !current_transform in
        current_transform := updated_transform;
        current_stack := new_stack
      | "add" ->
        let (new_stack) = Stack.add !current_stack in
        current_stack := new_stack
      | "sub" ->
        let (new_stack) = Stack.sub !current_stack in
        current_stack := new_stack
      | "mul" ->
        let (new_stack) = Stack.mul !current_stack in
        current_stack := new_stack
      | "div" ->
        let (new_stack) = Stack.div !current_stack in
        current_stack := new_stack
      | num ->
        try
          current_stack := Stack.push (float_of_string num) !current_stack
        with
        | Failure _ -> failwith "Bad token!"  (* Ignore invalid tokens *)
    ) tokens
  in
  
  try
    let rec read_lines () =
      try
        let line = input_line channel in
        let tokens = String.split_on_char ' ' line in
        process_tokens tokens;
        read_lines ()
      with End_of_file -> ()
    in
    read_lines ();
    !current_picture
  with End_of_file -> !current_picture
