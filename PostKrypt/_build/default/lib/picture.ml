type r = float
type r2 = r * r
type vec = r2
type point = r2

let ( +| ) ((x1, y1) : r2) ((x2, y2) : r2) : r2 = (x1 +. x2, y1 +. y2)
let ( *| ) (c : r) ((x, y) : r2) : r2 = (c *. x, c *. y)

type pic =
  | Line of point * point

type picture = pic list

let rec string_of_pic : pic -> string = function
  | Line (p1, p2) -> "Line from " ^ string_of_point p1 ^ " to " ^ string_of_point p2
  
and string_of_point : point -> string = function
  | (x, y) -> "(" ^ string_of_float x ^ ", " ^ string_of_float y ^ ")"

let string_of_picture : picture -> string =
  fun pic_list -> String.concat "; " (List.map string_of_pic pic_list)  

let line : point -> point -> picture =
  fun p1 p2 -> [Line (p1, p2)]

let rectangle : r -> r -> picture =
  fun width height ->
    let p1 = (0.0, 0.0) in
    let p2 = (width, 0.0) in
    let p3 = (width, height) in
    let p4 = (0.0, height) in
    [ Line (p1, p2); Line (p2, p3); Line (p3, p4); Line (p4, p1) ]

let ( +++ ) : picture -> picture -> picture =
  fun pic1 pic2 -> pic1 @ pic2

let baloon : picture =
  rectangle 100.0 100.0 +++ line (-100.0, -100.0) (0.0, 0.0)

type intLine = (int * int) * (int * int)
type intRendering = intLine list

let round x =
  let floor_x = floor x in
  let ceil_x = ceil x in
  if x -. floor_x < ceil_x -. x then
    int_of_float floor_x
  else
    int_of_float ceil_x


let renderScaled : int -> picture -> intRendering =
  fun scale pic ->
    let scale_line (Line ((x1, y1), (x2, y2))) =
      ((round (x1 *. float_of_int scale), round (y1 *. float_of_int scale)),
       (round (x2 *. float_of_int scale), round (y2 *. float_of_int scale)))
    in
    List.map scale_line pic