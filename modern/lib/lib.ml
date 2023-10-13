module Lib = struct
  type id = string
  type binop = Plus | Minus | Times

  type stm =
    | CompoundStm of stm * stm
    | AssignStm of id * exp
    | PrintStm of exp list

  and exp =
    | IdExp of id
    | NumExp of int
    | EseqExp of stm * exp
    | OpExp of exp * binop * exp

  let rec interp_exp exp env =
    match exp with
    | IdExp id -> (List.assoc id env, env)
    | NumExp num -> (num, env)
    | EseqExp (stm, exp) ->
        let env_after_stm = interp_stm stm env in
        interp_exp exp env_after_stm
    | OpExp (a, op, b) ->
        let a_val, _ = interp_exp a env in
        let b_val, _ = interp_exp b env in
        let value =
          match op with
          | Plus -> a_val + b_val
          | Minus -> a_val - b_val
          | Times -> a_val * b_val
        in
        (value, env)

  and interp_stm stm env : (id * int) list =
    match stm with
    | CompoundStm (stm1, stm2) ->
        let env_after_stm1 = interp_stm stm1 env in
        interp_stm stm2 env_after_stm1
    | AssignStm (id, exp) ->
        let value, _ = interp_exp exp env in
        (id, value) :: env
    | PrintStm expList ->
        let rec eval_and_print exp_list env_acc =
          match exp_list with
          | [] -> env_acc
          | exp :: rest ->
              let value, updated_env = interp_exp exp env in
              Printf.printf "%d " value;
              eval_and_print rest updated_env
        in
        let _ = eval_and_print expList env in
        env
end

module MyTree = struct
  type key = string
  type tree =
      | Leaf
      | Tree of tree * key * tree

  let rec insert key = function
    | Leaf -> Tree (Leaf, key, Leaf)
    | Tree (l, k, r) ->
        if key < k then Tree (insert key l, k, r)
        else if key > k then Tree (l, k, insert key r)
        else Tree (l, key, r)

  let rec member key = function
    | Leaf -> false
    | Tree (l, k, r) ->
        if key < k then member key l else if key > k then member key r else true

  let rec string_of_tree tree : string =
    match tree with
    | Leaf -> "Leaf"
    | Tree (left, key, right) ->
        "Tree (" ^ string_of_tree left ^ ", " ^ key ^ ", "
        ^ string_of_tree right ^ ")"
end
