(* 01 - Tail of a list *)
let rec last (list:'a list): 'a option =
    match list with
    | [] -> None
    | h :: [] -> Some h
    | _ :: t -> last t
;;

(* 02 - Last Two elements of a list *)
let rec last_two (list: 'a list): ('a * 'a) option =
    match list with
    | [] -> None
    | [ x ] -> None
    | [x; y] -> Some (x , y)
    | _ :: x -> last_two x
;;

(* 04 - Length of a list *)
let length list =
    let rec aux k = function
        | [] -> k
        | _ :: t -> aux (k + 1) t
    in
    aux 0 list
;;

(* 05 - Reverse a list *)
let rev list =
    let rec aux acc = function
        | [] -> acc
        | hd :: tl -> aux (hd :: acc) tl
    in
    aux [] list
;;
