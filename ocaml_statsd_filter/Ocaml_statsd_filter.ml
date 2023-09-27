open Unix

let bufsize = 8192
let buf = Bytes.create bufsize


let forward_addr = ADDR_INET (inet_addr_of_string Cfg.forward_host, Cfg.forward_port)
let forward_sock = socket PF_INET SOCK_DGRAM 0

let allow data = Cfg.blocklist
    |> List.exists (fun regexp -> Str.string_match regexp data 0)
    |> not

let process inc _ =
    let in_descr = descr_of_in_channel inc in
    let read_len, _ = recvfrom in_descr buf 0 bufsize [] in
    let buf_str = Bytes.to_string buf in
    if allow buf_str then begin
        ignore (send forward_sock buf 0 read_len []);
        print_string ("Sent: " ^ buf_str)
    end else
        print_string ("Did not send: " ^ buf_str)

let () =
    connect forward_sock forward_addr;
    establish_server process(ADDR_INET (inet_addr_any, Cfg.listen_port))
