(*
 * Yuebai Gao, Kayla Sell, and Jarrod Young
 * CSC 254 Assignment 7
 *)

type tok = string * string;;
(*         category * name *)

class token (type_ : string) (val_ : string) =
  object(self)
    val ttype = type_
    val v = val_
    method value = v
    method tok_type = ttype
  end;;

let is_alpha alpha = match alpha with 
  | 'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' -> true
  | 'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z' -> true 
  | _ -> false;;

let is_digit digit = match digit with
  |'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9' -> true
  | _ -> false;; 

let explode (s:string) : char list =
  let rec exp i l = if i < 0 then l else exp (i-1) (s.[i] :: l) in
  exp (String.length s - 1) [];;

let implode (l:char list) : string =
  let res = Bytes.create (List.length l) in
  let rec imp i l =
    match l with
    | [] -> Bytes.to_string res
    | c :: l -> Bytes.set res i c; imp (i - 1) l in
  imp ((List.length l)-1) l;;

let prefix_char s c = String.make 1 c ^ s

let tokenize (program:string) : token list =
  let get_id prog =
    let rec gi tok p =
        match p with
        | c :: rest when (is_digit c)|| (is_alpha c) || (c = '_')
            -> gi (c :: tok) rest
        | _ -> (implode (tok), p) in
    gi [] prog in

  let get_int prog =
    let rec gi tok p =
        match p with
        | c :: rest when (is_digit c)
            -> gi (c :: tok) rest
        | _ -> (implode (tok), p) in
    gi [] prog in

  let get_num prog =
    let (tok, rest) = get_int prog in
    match rest with
    | '.' :: c :: r when (is_digit c)
        -> let (tok2, rest2) = get_int (c :: r) in
           ((tok ^ "." ^ tok2), rest2)
    | _ -> (tok, rest) in

  let rec get_error tok prog =
    match prog with
    | [] -> (implode (tok), prog)
    | c :: rest ->
        match c with
        | ':' | '+' | '-' | '*' | '/' | '(' | ')' | '_'
        | '<' | '>' | '=' | 'a'..'z' | 'A'..'Z' | '0'..'9'
            -> (implode ( tok), prog)
        | _ -> get_error (c :: tok) rest in

  let rec skip_space prog =
    match prog with
    | [] -> []
    | c :: rest -> if (c = ' ' || c = '\n' || c = '\r' || c = '\t')
                      then skip_space rest else prog in 
  
  let rec get_comment prog =
    let rec gi tok p =
      match p with
      | c :: rest ->
         match c with
         | '/' -> gi ('/'::tok) rest 
         | '\n' -> (implode tok, p) 
         | _ -> gi (c :: tok) rest in
    gi [] prog in 

  let get_equal prog =
    let rec gi tok p =
        match p with
        | c :: rest when (c = '=')
            -> gi (c :: tok) rest
        | _ -> (implode (tok), p) in
    gi [] prog in

  let get_meta prog =
    let rec gi tok p =
      match p with
      | c :: rest ->
         match c with
         | '#' -> gi ('#'::tok) rest 
         | '\n' -> (implode tok, p) 
         | _ -> gi (c :: tok) rest in
    gi [] prog in  

  let get_string prog =
    let rec gi tok p =
        match p with
        | c :: rest ->
          match c with
          | '"' -> (prefix_char (implode ('"'::tok)) '"', rest)
          | _ -> gi (c::tok) rest in
    gi [] prog in

  let get_and prog =
    let rec gi tok p =
        match p with
        | c :: rest when (c = '&' || c='|')
            -> gi (c :: tok) rest
        | _ -> (implode (tok), p) in
    gi [] prog in 

  let rec tkize toks prog =
    match prog with
    | []                 -> ((toks), [])
    | '\n' :: rest
    | '\r' :: rest
    | '\t' :: rest
    | ' ' :: rest        -> tkize toks (skip_space prog)
    | '/' :: '/' :: rest -> let (t, rest) = get_comment prog in
                         tkize (t :: toks) rest
    | '-' :: rest        -> tkize ("-"  :: toks) rest
    | '+' :: rest        -> tkize ("+"  :: toks) rest
    | '/' :: rest        -> tkize ("/"  :: toks) rest
    | '*' :: rest        -> tkize ("*"  :: toks) rest
    | '(' :: rest        -> tkize ("("  :: toks) rest
    | ')' :: rest        -> tkize (")"  :: toks) rest
    | '{' :: rest        -> tkize ("{"  :: toks) rest
    | '}' :: rest        -> tkize ("}"  :: toks) rest
    | '[' :: rest        -> tkize ("["  :: toks) rest
    | ']' :: rest        -> tkize ("]"  :: toks) rest
    | ',' :: rest        -> tkize (","  :: toks) rest
    | ';' :: rest        -> tkize (";"  :: toks) rest

    | '<' :: '=' :: rest -> tkize ("<=" :: toks) rest
    | '<' :: rest        -> tkize ("<"  :: toks) rest
    | '>' :: '=' :: rest -> tkize (">=" :: toks) rest
    | '>' :: rest        -> tkize (">"  :: toks) rest
    | '!' :: '=' :: rest -> tkize ("!=" :: toks) rest

    | h :: t -> match h with
           | '0'..'9' -> let (t, rest) = get_num prog in
                         tkize (t :: toks) rest
           | 'a'..'z'
           | 'A'..'Z'
           | '_'      -> let (t, rest) = get_id prog in
                         tkize (t :: toks) rest
           | '#'     -> let (t, rest) = get_meta prog in
                         tkize (t :: toks) rest
           | '='    ->  let (t, rest) =get_equal prog in
                         tkize (t :: toks) rest
           | '&'    ->  let (t, rest) =get_and prog in
                         tkize (t :: toks) rest
           | '|'    ->  let (t, rest) =get_and prog in
                         tkize (t :: toks) rest

           | '"'    -> let (t, rest) = get_string t in
                         tkize (t :: toks) rest

           | c        -> let (t, rest) = get_error [c] t in
                         tkize (t :: toks) rest in
  let (toks, _) = (tkize [] (explode program)) in
  let categorize tok =
    match tok with
    | "int" | "void" | "if" | "while"
    | "return" | "continue" | "break" | "scanf" | "main" | "printf"  | "else"
     -> (new token "reserved" tok)
    | "=" | "+"  | "-"  | "*"  | "/"  | "("  | ")" | "{" |"}"|"["|"]"|","|";"|"!="
    | "<"  | "<=" | ">"  | ">=" | "==" | "&&"|"&"|"||"|"|" 
     -> (new token "symbol" tok)   
    | _ -> match tok.[0] with
           | '0'..'9' -> (new token "number" tok)
           | 'a'..'z'
           | 'A'..'Z' | '_' -> (new token "ident" tok)
           | '#' -> (new token "comment" tok)
           | '/' -> (new token "comment" tok)
           | '"' -> (new token "string" tok)
           | _ -> (new token "error" tok) in
  List.map categorize (toks);;

let read_file file =
  let ic = open_in file in
  let buf = Buffer.create (in_channel_length ic) in
  try
    while true do
      let line = input_line ic in
      Buffer.add_string buf line;
      Buffer.add_char buf '\n';
    done; assert false
  with End_of_file ->
    Buffer.contents buf
;;


let rec print_list lst= match lst with
  | [] -> ()
  | t::l -> Printf.printf "%s " t#tok_type; Printf.printf "%s\n" t#value; print_list l ;;  

let program=read_file Sys.argv.(1);;
let result=tokenize program ;;
let rresult=List.rev result;;
(*print_list rresult;;*)

let toks=ref (rresult @ [new token "end" ""]);;

let next()=
  match !toks with
    | t::n -> toks:=n; t;;


let peek() = 
  match !toks with
    | t::n-> t ;;



