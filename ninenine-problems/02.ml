let rec last_two (list: 'a list): ('a * 'a) option =
    match list with
    | [] -> None
    | [ x ] -> None
    | [x; y] -> Some (x , y)
    | _ :: x -> last_two x
