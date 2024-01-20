(* Reprezentacja punktów i obrazów *)
type r = float
type r2 = r * r

val ( +| ) : r2 -> r2 -> r2
val ( *| ) : r -> r2 -> r2

(* wektor 2D *)
type vec = r2
(* punkt 2D *)
type point = r2

type pic =
  | Line of point * point

type picture = pic list

val string_of_pic : pic -> string
val string_of_picture : picture -> string

(* odcinek pomiędzy punktami o podanych współrzędnych *)
val line : point -> point -> picture
(* prostokąt o podanej szerokości i wysokości zaczepiony w (0,0) *)
val rectangle : r -> r -> picture
(* suma (nałożenie) dwóch rysunków *)
val (+++) : picture -> picture -> picture

(* przykładowy obrazek 
  ▢ 
 ╱   
  Prostokąt o boku 100 (zaczepiony w 0,0) i linia od (-100,-100) do (0,0)
*)
val baloon : picture


(* Obrazowanie *)
type intLine = (int * int) * (int * int)
type intRendering = intLine list

(* Obrazowanie przy danym współczynniku powiększenia
   z zaokrągleniem do najbliższych wartości całkowitych *)
val renderScaled : int -> picture -> intRendering