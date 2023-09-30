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

List.nth
(* 03 - Nth element on list *)
let rec nth list n =
    match list with
    | [] -> raise (Failure "Empty")
    | hd :: tl -> if n = 0 then hd else nth tl (n - 1)
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

(* 06 - Palindrome *)
let is_palindrome list =
    list = rev list
;;

(* 07 - Flatten a list *)
type 'a node = | One of 'a | Many of 'a node list


let rec flatten lst =
    match lst with
    | [] -> []
    | One x :: rest -> x :: flatten rest
    | Many lst :: rest -> flatten lst @ flatten rest
;;

