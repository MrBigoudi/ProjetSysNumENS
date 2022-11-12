open Netlist_ast;;
open Format;;

let print_only = ref false
let number_steps = ref (-1)
let fStdout = (formatter_of_out_channel stdout)
let fStderr = (formatter_of_out_channel stderr)

exception LogicalError of string
exception SystemError of string

(* init l'environnement *)
let initEnv p = 
  (* Netlist.print_program stdout p; *)
  (* on cree une map vide *)
  let env = Env.empty in
  (* on recupere la liste des cles de l'environnement de depart *)
  let (keys,_) = (List.split (Env.bindings p.p_vars))
  in
    (* fprintf fStdout "%a@.\n" Netlist_printer.print_idents keys; *)
    (* on ajoute ces cles dans l'env *)
    let rec aux keys env = 
      match keys with 
      | [] -> env
      | k::keys -> 
        (* on initialise la valeure a false *)
        let env = (Env.add k (VBit(false)) env)
          in (aux keys env)
      in (aux keys env)
;;

(* return the Value of an argument *)
let calculArg arg env = 
  match arg with 
    | Avar ident -> Env.find ident env
    | Aconst value -> value
;;

(* return the Value of a Register *)
let calculReg ident env =
  Env.find ident env
;;

(* return the Value after a Not operation *)
let calculNot arg env =
  (* logical not *)
  let notOp e = not e in
  match (calculArg arg env) with 
  | VBit b -> VBit(not b)
  | VBitArray arr -> VBitArray(Array.map notOp arr)
;;

(* applies a binary operation *)
let binopAux arg1 arg2 env op msg = 
  match (calculArg arg1 env) with 
  | VBit b1 -> 
      begin
        match (calculArg arg2 env) with 
        | VBit b2 -> VBit(op b1 b2)
        | VBitArray arr -> raise (LogicalError ("Can't do an \'"^msg^"\' operation between a bit and a bit array!\n"))
      end
  | VBitArray arr1 -> 
      begin
        match (calculArg arg2 env) with
        | VBit b2 -> raise (LogicalError ("Can't do \'"^msg^"\' operation between a bit and a bit array!\n"))
        | VBitArray arr2  -> 
          try VBitArray(Array.map2 op arr1 arr2) with _ -> raise (LogicalError ("Can't do \'"^msg^"\' operation between bit arrays of different size!\n"))
      end
;;

(* return the Value after an Or operation between VBits*)
let orOp e1 e2 = e1 || e2;;
(* return the Value after an Xor operation between VBits*)
let xorOp e1 e2 = e1 <> e2;;
(* return the Value after an And operation between VBits*)
let andOp e1 e2 = e1 && e2;;
(* return the Value after an And operation between VBits*)
let nandOp e1 e2 = not (e1 && e2);;

(* return the Value after a binary operation *)
let calculBinop binop arg1 arg2 env =
  match binop with
    | Or   -> (binopAux arg1 arg2 env orOp "Or")
    | Xor  -> (binopAux arg1 arg2 env xorOp "Xor")
    | And  -> (binopAux arg1 arg2 env andOp "And")
    | Nand -> (binopAux arg1 arg2 env nandOp "Nand")
;;

(* return the Value after a multiplexor operation *)
let calculMux bit arg1 arg2 env =
  match (calculArg bit env) with 
    | VBit false -> (calculArg arg1 env)
    | VBit true  -> (calculArg arg2 env)
    (* test if the first argument is corect *)
    | _ -> raise (LogicalError "Can't have a VBitArray as the multiplexor selector input!\n")


(* return the Value after a concatenation *)
let calculConcat arg1 arg2 env = 
  match (calculArg arg1 env) with
  | VBit _ -> raise (LogicalError "Can't concatenate a bit and a bit array!\n")
  | VBitArray arr1 ->
    match (calculArg arg2 env) with
    | VBit _ -> raise (LogicalError "Can't concatenate a bit and a bit array!\n")
    | VBitArray arr2 -> 
      try 
        VBitArray((Array.append arr1 arr2))
      with _ -> raise (SystemError "Bit arrays are too big to be concatenate!\n")
;; 


(* return the Value after a slice *)
let calculSlice i1 i2 arg env =
  match (calculArg arg env) with
  | VBit _ -> raise (LogicalError "Can't slice a bit!\n")
  | VBitArray arr -> 
    try
      VBitArray((Array.sub arr i1 i2))
    with _ -> raise (LogicalError "Invalid indices for slicing the array!\n")
;;


(* return the Value after a bit selection *)
let calculSelect i arg env = 
  match (calculArg arg env) with
  | VBit _ -> raise (LogicalError "Can't select from a bit!\n")
  | VBitArray arr -> 
    try
      VBit((Array.get arr i))
    with _ -> raise (LogicalError "Invalid index for selecting in the array!\n")
;;


(* simlue le calcul d'une expression *)
let calculExp exp env prevEnv =
  (* on reconnait l'expression *)
  match exp with
    | Earg arg                 -> (calculArg arg env)
    | Ereg ident               -> (calculReg ident prevEnv) (* prevEnv is env but delayed by one cycle *)
    | Enot arg                 -> (calculNot arg env)
    | Ebinop (binop,arg1,arg2) -> (calculBinop binop arg1 arg2 env)
    | Emux (bit, arg0, arg1)   -> (calculMux bit arg0 arg1 env)
    | Econcat (arg1, arg2)     -> (calculConcat arg1 arg2 env)
    | Eslice (i1, i2, arg)     -> (calculSlice i1 i2 arg env)
    | Eselect (i, arg)         -> (calculSelect i arg env)
    | _ -> raise (SystemError "Unknown expression!\n")
    
    (*
    | Erom of int (*addr size*) * int (*word size*) * arg (*read_addr*)
      (* ROM addr_size word_size read_addr *)
    | Eram of int (*addr size*) * int (*word size*)
      * arg (*read_addr*) * arg (*write_enable*)
      * arg (*write_addr*) * arg (*data*)
      (* RAM addr_size word_size read_addr write_enable write_addr data *)
    *)
;;

(* simule le calcul d'une equation *)
let doEq eq env prevEnv =
  (* on decompose l'equation *)
  let (ident, exp) = eq in
    (* on calcul la valeure de l'expression *)
    let value = (calculExp exp env prevEnv) in
      (* on rend les environnements mis a jour, prevEnv reste inchange car en retard *)
      let newEnv = Env.add ident value env in (newEnv, prevEnv)
;;

(* print the results *)
let showResults program env =
  let outputs = program.p_outputs in
  begin
    let rec aux outs =
      match outs with
      | [] -> ()
      | out::outs ->
        begin
          let value = Env.find out env in
              fprintf fStdout "=> %a = %a@." Netlist_printer.print_idents [out] Netlist_printer.print_value value;
          aux outs
        end
    in (aux outputs)
  end
;;


(* check input correctness *)
let checkInput input =
  if (String.length input = 0) then false else
  let charCorrect c = (c=='0' || c=='1') in
    let len = String.length input in
      let rec aux len = 
        match len with 
        | 0 -> true
        | i -> if (not (charCorrect input.[i-1])) then false
                else (aux (len-1))
      in (aux len)
;;

(* cast a char to a boolean *)
let charToBool c =
  match c with
  | '0' -> false
  | '1' -> true
  | _ -> raise (SystemError "Unexpecter character!\n")
;;

(* cast a string to a boolean array *)
let stringToArray s len =
  let arr = Array.make len false in
  let rec aux len =
    match len with 
    | 0 -> arr 
    | i -> 
      begin
        arr.(i-1) <- (charToBool s.[i-1]);
        (aux (len-1))
      end
  in (aux len)
;;

(* cast a string to a Value *)
let stringToValue s = 
  let len = (String.length s) in
    match len with
    | 1 -> let b = charToBool s.[0] in VBit(b)
    | l when (l > 1) -> let arr = (stringToArray s l) in VBitArray(arr)
    | _ -> raise (SystemError "Invalid user input!\n")
;;


(* read a user input *)
let askInput ident env =
  (* get the user input *)
  let rec ask () =
      fprintf fStdout "%s = ?@." ident;
      flush stdout;
      let userInput = read_line() in
        (* fprintf fStdout "your input : %s\n@." userInput; *)
        (* check if correct *)
        if(checkInput userInput)
          (* if correct then update env *)
          then
            begin 
              (* fprintf fStdout "your input : %s\n@." userInput; *)
              let value = stringToValue userInput in
                (* return the updated env *)
                Env.add ident value env
            end
          (* else ask for the input again *)
          else
            begin
              fprintf fStdout "Wrong input@.";
              ask()
            end
    in ask()
;;


(* read the user's value for the inputs *)
let askForInputs inputIdents env =
  (* ask for each of the program's inputs *)
  let rec aux inputIdents env =
    match inputIdents with 
    | [] -> env
    | ident::inputIdents ->
      let env = askInput ident env in (aux inputIdents env)
  in (aux inputIdents env)
;;


(* exception handler *)
let catchException exc = 
  let manageException exc excName msg =
    begin
      fprintf fStderr "\n##### ERROR #####\n\n%s error: %s\n@." excName msg;
      raise exc
    end
  in
  match exc with
    | LogicalError msg -> (manageException (LogicalError "") "Logical" msg)
    | SystemError  msg -> (manageException (SystemError  "") "System"  msg)
    | _ -> raise exc
;;


let simulator program number_steps = 
  (* Netlist.print_program stdout program; *)
  (* creation environnement *)
  let env = (initEnv program) in
  (* fprintf fStdout "nbsteps = %d\n@." number_steps; *)
  if (number_steps < (-1)) then catchException (LogicalError "Number of steps can't be a negative value!\n")
  else
    (* for i in range nbSteps *)
    let rec forNbStep numSteps env = 
      if (numSteps == 0) 
        then 
          begin
            fprintf fStdout "\n\nDone, final output:\n";
            (showResults program env)
          end
      else
        begin
          fprintf fStdout "Step %d:\n" (number_steps - (numSteps-1));
          (* demande d'inputs *)
          let env = askForInputs program.p_inputs env in
            (* for eq in equations *)
            let eqs = program.p_eqs in
              let rec forEqs eqs env prevEnv =
                match eqs with
                | [] -> env
                | eq::eqs ->
                  (* traite une equation *)
                  try
                    let (env, prevEnv) = (doEq eq env prevEnv) in (forEqs eqs env prevEnv)
                  with exc -> (catchException exc)
              in
                let env = (forEqs eqs env env) 
            in 
              begin 
                (showResults program env);
                (forNbStep (numSteps-1) env)
              end
          end
    in
    (forNbStep number_steps env)
;;


let compile filename =
  try
    let p = Netlist.read_file filename in
    begin try
        let p = Scheduler.schedule p in
        (* print only option *)
        if (!print_only)
          then Netlist.print_program stdout p
          else
            simulator p !number_steps
      with
        | Scheduler.Combinational_cycle ->
            Format.eprintf "The netlist has a combinatory cycle.@.";
    end;
  with
    | Netlist.Parse_error s -> Format.eprintf "An error accurred: %s@." s; exit 2

let main () =
  Arg.parse
    [
      "-n", Arg.Set_int number_steps, "Number of steps to simulate";
      "--print", Arg.Set print_only, "Only prints the program"
    ]
    compile
    ""
;;

main ()
