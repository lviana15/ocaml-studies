let length list =
    let rec aux k = function
        | [] -> k
        | _ :: t -> aux (k + 1) t
    in
    aux 0 list
;;
