module Cmd = struct
    type t = { wait: int; repeat: int; msg: string }

    let usage = "cmdarg [-w <time>] [-r <repeat>] <msg>

    Prints <msg> out to standard output.
    "
    let parse () =
        let wait = ref 0 in
        let repeat = ref 1 in
        let msg = ref None in

        let specs = [
            "-w", Arg.Set_int wait, "Time in seconds to wait before printing the message [default 0]";
            "-r", Set_int repeat, "How many times to print hte message [default 1]";
        ]
    in
    let anon str = msg := Some str in
    Arg.parse specs anon usage;
    {
        wait = !wait;
        repeat = !repeat;
        msg = match !msg with
            | Some m ->
                m
            | None ->
                Arg.usage specs usage;
                invalid_arg "<msg> is required";
    }
end

let () =
    let { Cmd.wait; repeat; msg } = Cmd.parse () in
    for _ = 1 to repeat do
        Unix.sleep wait;
        print_endline msg
    done
