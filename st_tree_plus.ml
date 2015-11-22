#load "st_tree.cmo";;
open St_tree;;

let is_single arbol =
    try let _ = ramas arbol in false
    with Ramas -> true;;

let izq arbol = match ramas arbol with
	(i,d) -> i;;

let izq =
  let (a,b) =
    ramas
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

let leafs = arbol
    let lista = [[]] in
      let rec getHojas arbolito =
        if (is_single arbolito) then arbolito
        else
          match ramas arbolito with
                (i,d) -> lista::[[raiz (getHojas i)]]::[[raiz (getHojas d)]]
      in getHojas arbol;;

izq (comp 5 ((single 1), (single 2)));;

let crono f x =
   let t = Sys.time () in
      let _ = f x in
         Sys.time () -. t;;


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
