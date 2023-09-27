let listen_port =
    try int_of_string (Sys.getenv "listen_port") with Not_found -> 8125

let forward_host = try Sys.getenv "forward_host" with Not_found -> "127.0.0.1"

let forward_port =
    try int_of_string (Sys.getenv "forward_port") with Not_found -> 8126

let blocklist =
    try
        "blocklist"
        |> Sys.getenv
        |> String.split_on_char ','
        |> List.map Str.regexp_string
    with
        Not_found -> []
