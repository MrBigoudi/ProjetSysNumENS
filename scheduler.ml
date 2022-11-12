open Netlist_ast
open Graph

exception Combinational_cycle


let read_exp eq =
  let getVar arg = 
    match arg with
    | Avar id -> [id]
    | _ -> []
  in
  let (_, exp) = eq in
  match exp with
    | Earg arg -> (getVar arg)
    | Ereg ident -> []
    | Enot arg -> (getVar arg)
    | Ebinop (_, arg1, arg2) -> (getVar arg1)@(getVar arg2)
    | Emux (arg0, arg1, arg2) -> (getVar arg0)@(getVar arg1)@(getVar arg2)
    | Erom (_,_,arg) -> (getVar arg)
    | Eram (_,_,arg0,_,_,_) -> (getVar arg0)
    | Econcat (arg1, arg2) -> (getVar arg1)@(getVar arg2)
    | Eslice (_,_,arg) -> (getVar arg)
    | Eselect (_,arg) -> (getVar arg)
;;


let getKeys vars = 
  let (keys,_) = (List.split (Env.bindings vars)) in keys
;;

let initGraph p = 
  let g  = Graph.mk_graph() in
      let keys = (getKeys p.p_vars) in 
  let rec aux keys = match keys with
    | [] -> g
    | h::t -> begin 
                (Graph.add_node g h);
                (aux t);
              end
  in (aux keys)
;;

let addEdges g inputs output =
  let rec aux inputs = match inputs with
  | [] -> g
  | h::t -> begin
              (Graph.add_edge g h output);
              (aux t);
            end
  in (aux inputs)
;;

let program_to_graph p =
  (* create the graph *)
  let rec aux (eqs: equation list) g = match eqs with
    | [] -> g
    | (i,exp)::t -> 
          let idents = (read_exp (i,exp)) in
            (aux t (addEdges g idents i))
  in (aux p.p_eqs (initGraph p))
;; 


let getEq ident eqList = 
  let rec aux eqList =
    match eqList with
    | [] -> failwith "Ident not in eqList"
    | h::t -> 
      let (i,exp) = h in
        begin
          (* print_string ("eq ident: " ^ i ^ "\n"); *)
          if (i = ident) then (i,exp)
          else (aux t)
        end
  in (aux eqList)
;;


let identList_to_equationList identL p =
  let rec aux res identL = 
    match identL with
    | [] -> res
    | h::t -> 
      begin
        (* print_string ("\nident: " ^ h ^ "\n"); *)
        (* if h is an input we don't check it *)
        if List.mem h p.p_inputs 
          then 
            begin
              (* print_string "INPUT\n"; *)
              (aux res t)
            end
        else 
          (* otherwise rebuild its equation *)
          begin
            (* print_string "VAR\n"; *)
            let eq = getEq h p.p_eqs
              in (aux (res@[eq]) t);
          end
      end
  in (aux [] identL)
;;


let schedule p = 
  let g = (program_to_graph p) in
  try 
    begin
      let lTopo = (Graph.topological g) in
      (* Graph.print_list_string lTopo; *)
      let eqList = (identList_to_equationList lTopo p) in
        {p_eqs = eqList; p_inputs = p.p_inputs; p_outputs = p.p_outputs; p_vars = p.p_vars}
    end
  with Cycle -> raise Combinational_cycle;
;;
