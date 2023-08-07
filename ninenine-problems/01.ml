let rec last (list:'a list): 'a option =
    match list with
    | [] -> None
    | h :: [] -> Some h
    | _ :: t -> last t
