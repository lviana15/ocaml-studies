include Lib
include MyTree

let prog =
  Lib.CompoundStm
    ( AssignStm ("a", OpExp (NumExp 5, Plus, NumExp 3)),
      CompoundStm
        ( AssignStm
            ( "b",
              EseqExp
                ( PrintStm [ IdExp "a"; OpExp (IdExp "a", Minus, NumExp 1) ],
                  OpExp (NumExp 10, Times, IdExp "a") ) ),
          PrintStm [ IdExp "b" ] ) )

let () =
  let _ = Lib.interp_stm prog [] in
  Printf.printf "\n"

let empty = Leaf
let tree2 = insert "a" empty
let () = Printf.printf "%s" (string_of_tree tree2)
