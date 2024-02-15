open Picture
open Inputpic

let parse_arguments () =
  let scale = ref 1 in
  let input_file = ref stdin in
  let use_graphics = ref false in
  let usage_msg = "Usage: graf [-n n] [-d] [plik.in]" in
  let speclist = [
    ("-n", Arg.Set_int scale, "Set the scale of the graphic (default: 1)");
    ("-d", Arg.Set use_graphics, "Use graphics output");
  ] in
  Arg.parse speclist (fun filename -> input_file := open_in filename) usage_msg;
  !scale, !input_file, !use_graphics

let draw_line ((x1, y1), (x2, y2)) =
  Graphics.moveto (int_of_float x1) (int_of_float y1);
  Graphics.lineto (int_of_float x2) (int_of_float y2)

let draw_rendering rendering =
  List.iter draw_line rendering

let main () =
  let use_graphics = ref false in
  try
    let scale, input_channel, use_graphics_val = parse_arguments () in
    use_graphics := use_graphics_val;
    let picture = readPic input_channel in
    let rendering = renderScaled scale picture in
    if !use_graphics then (
      let width = 800 in
      let height = 600 in
      Graphics.open_graph "";
      Graphics.resize_window width height;
      Graphics.clear_graph ();
      let rendering = List.map (fun ((x1, y1), (x2, y2)) -> ((float_of_int (x1 + 400), float_of_int (y1 + 300)), (float_of_int (x2 + 400), float_of_int (y2 + 300)))) rendering in
      draw_rendering rendering;
      ignore (Graphics.read_key ());
      Graphics.close_graph ();
      ""
    ) else (
      let translated_rendering = List.map (fun ((x1, y1), (x2, y2)) -> Printf.sprintf "%d %d moveto %d %d lineto" y1 x1 y2 x2) rendering in
      let translated_rendering_string = String.concat "\n" translated_rendering in
      let output_string = Printf.sprintf "300 400 translate\n%s\nstroke showpage" translated_rendering_string in
      output_string
    )
  with
  | _ ->
    if !use_graphics then (
      Graphics.close_graph ();
      prerr_endline "An error occurred. Graphic window cannot be displayed.";
      ""
    ) else (
      "300 400 translate\n/Courier findfont 24 scalefont setfont 0 0 moveto (Error) show\nstroke showpage"
    )

let () = print_endline (main ())