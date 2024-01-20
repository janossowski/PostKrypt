open Picture

(* typ transformacji *)
type transform

(* identyczność *)
val id : transform
(* suma transformacji *)
val sum : transform -> transform -> transform

(* przesunięcie o wektor *)
val translate : vec -> transform

(* obrót wokół punktu (0,0) przeciwnie do ruchu wskazówek zegara
   jednostki mozna sobie wybrać *)
val rotate : r -> transform
val fullCircle : r  (* wartość odpowiadająca 1 pełnemu obrotowi (360 stopni) *)

(* przetrasformuj punkt, wektor, obrazek *)
val trpoint : transform -> point -> point
val trvec : transform -> vec -> vec
val transform : transform -> picture -> picture