open Picture

type transform =
  | Id
  | Sum of transform * transform
  | Translate of vec
  | Rotate of r

let id : transform =
  Id

let sum : transform -> transform -> transform =
  fun t1 t2 ->
    Sum (t1, t2)

let translate : vec -> transform =
  fun v ->
    Translate v

let rotate : r -> transform =
  fun angle ->
    Rotate angle

let fullCircle : r =
  1.0  (* Assuming 1.0 represents a full circle *)

let rec trpoint : transform -> point -> point =
  fun transformation point ->
    match transformation with
    | Id -> point
    | Sum (t1, t2) -> trpoint t2 (trpoint t1 point)
    | Translate v -> point +| v
    | Rotate angle ->
        let (x, y) = point in
        let cos_a = cos (angle *. 2.0 *. Float.pi) in
        let sin_a = sin (angle *. 2.0 *. Float.pi) in
        ((cos_a *. x) -. (sin_a *. y), (sin_a *. x) +. (cos_a *. y))

let rec trvec : transform -> vec -> vec =
  fun transformation vector ->
    match transformation with
    | Id -> vector
    | Sum (t1, t2) -> trvec t2 (trvec t1 vector)
    | Translate v -> v +| vector
    | Rotate _ -> vector  (* Rotation does not affect vectors *)

let rec transform : transform -> picture -> picture =
  fun transformation pic ->
    List.map (fun graphic -> transform_graphic transformation graphic) pic

and transform_graphic : transform -> pic -> pic =
  fun transformation graphic ->
    match graphic with
    | Line (p1, p2) -> Line (trpoint transformation p1, trpoint transformation p2)