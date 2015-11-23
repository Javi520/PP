#load "st_tree.cmo";;
open St_tree;;

let is_single arbol =
    try let _ = ramas arbol in false
    with Ramas -> true;;

let izq arbol = match ramas arbol with
	(i,d) -> i;;

let izq arbol=
  let (a,b) =
    ramas arbol
  in a;;

let dech arbol = match ramas arbol with
	(i,d) -> d;;

let rec size arbol =
	if is_single arbol then 1
	else
		 match ramas arbol with
			(i,d) -> 1 + (size i) + (size d);;

let crearSuperArbol d =
	let elarbol = comp 1 (single 1,single 1) in
  	let rec auxiliar n arbol=
  		if n=0 then arbol
  		else
  		auxiliar (n-1) (comp 1 (arbol,arbol))
		in auxiliar d elarbol;;

let rec height arbol =
	if is_single arbol then 1
	else
		match ramas arbol with
		(i,d) -> 1 + max(height i)(height d);;

(*intento de version terminal de altura
sincerandome**
no se ni como funciona, no me acuerdo, solo se que va mejor,
eso si, la hice yo, en un alarde de euforia, creatividad, impulsividad y lucidez*)
let height2 arbol =
  if is_single arbol then 1
    else
    match ramas arbol with
      (i,d) -> let rec auxiliar n arbol =
                  if is_single arbol then n
                  else match ramas arbol with
                    (a,b) -> auxiliar (n+1) a; auxiliar (n+1) b
               in 2 + max(auxiliar 0 i)(auxiliar 0 d);;

let leafs arbol =
    let lista = [] in
      let rec getHojas arbolito =
        if (is_single arbolito) then (raiz arbolito) :: []
        else
          match ramas arbolito with
                (i,d) -> (getHojas i) @ (getHojas d)
      in lista @ getHojas arbol;;

izq (comp 5 ((single 1), (single 2)));;

let mirror arbol =
  let rec auxiliar arbol =
    if is_single arbol then single(raiz arbol)
    else match ramas arbol with
      (i,d)-> comp (raiz arbol) ((auxiliar d), (auxiliar i))
  in auxiliar arbol;;

let mirror2 arbol =
  (*funciona peor, muy pocos milisegundos menmos pero peor,
  el gallo pone huevos y la gallina
  tambien pero aunque losp onga no pueden salir pollos de ahi,iwnbmocokop
  isto quere dicir que ounha cousa non quita a outra*)
  let rec auxiliar arbol =
  let rice = raiz arbol in
    if is_single arbol then single(rice)
    else match ramas arbol with
      (i,d)-> comp (rice) ((auxiliar d), (auxiliar i))
  in auxiliar arbol;;

let is_mirrored arbol1 arbol2 =
    let rec auxiliar subarbol1 subarbol2 =
      if ( (is_single subarbol1 && (not (is_single subarbol2)))
      ||(is_single subarbol2 && (not (is_single subarbol1))) ) then false
      else
        if (is_single subarbol1 && is_single subarbol2) then
          subarbol1 = subarbol2
        else
          match ramas subarbol1,ramas subarbol2 with
            ((i1,d1),(i2,d2))->
              ((auxiliar i1 d2)&&(auxiliar i2 d1))
    in auxiliar arbol1 arbol2;;

let treemap arbol funy a b=
  if is_single arbol then raise(Failure "caca")
  else
  let auxiliar2 a b =
    (funy a b) in
    let rec auxiliar arbol =
      match ramas arbol with
        (i,d) -> auxiliar2 a b; auxiliar i; auxiliar d
    in auxiliar arbol;;

let funcionita a b =
    a+b;;
let crono f x =
   let t = Sys.time () in
      let _ = f x in
         Sys.time () -. t;;

let mayorMenosmenor a b =
  (max a b) -. (min a b );;

crono mirror (crearSuperArbol 20) /. crono mirror2 (crearSuperArbol 20);;
let x = 22;;
let hola = crearSuperArbol x;;
crono height hola;;
crono height2 hola;;
crono height2 (crearSuperArbol 25) /. crono height (crearSuperArbol 25);;
c

let probando d =
  let rec auxiliar d =
    if(d!=1000) then (shower d;auxiliar (d+1))
    else d
  in auxiliar 0;;
