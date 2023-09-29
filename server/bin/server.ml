open Opium

let hello _req = Lwt.return (Response.of_plain_text "Hello")
let greet req =
    let name = Router.param req "name" in
    Printf.sprintf "Hello %s" name
    |> Response.of_plain_text
    |> Lwt.return

let app = App.empty
let () =
    App.port 3000 app
    |> App.get "/" hello
    |> App.get "/greet/:name" greet
    |> App.run_command
