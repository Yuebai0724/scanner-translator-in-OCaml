(*
 * Yuebai Gao, Kayla Sell, and Jarrod Young
 * CSC 254 Assignment 7
 *)

open Scanner;;

open String;;
open Hashtbl;;


let loop_begin_label = ref "";;
let loop_exit_label = ref "";;
let global_sym_tab = Hashtbl.create 50;;
let functions =  Hashtbl.create 50;;


let idx = ref (-1);;
let num_global_vars = ref 0;;
let prog_code = ref "";;

let loop_begin_label = ref "";;
let loop_exit_label = ref "";;


class function_ =
        object(self)
                val mutable fncode : string = ""
                val mutable symtab = Hashtbl.create 50
                val mutable num_vars = 0
                val mutable num_labels = 0
                method append_code str =
                        fncode <- (concat "" [fncode; str])
                method new_var () =
                        num_vars <- num_vars + 1;
                        (concat "" ["mem["; (string_of_int (num_vars - 1)); "]"])
                method new_label () =
                        num_labels <- num_labels + 1;
                        (concat "" ["L"; (string_of_int num_labels)])
                method add_to_symtab (key : string) (value : string) =
                        Hashtbl.add symtab key value
                method sym_tab =
                        symtab
                method code () =
                        fncode
                method add_param name =
                        Hashtbl.add symtab name (self#new_var ());
                        self#append_code (concat "" [Hashtbl.find symtab name; " = "; name; ";\n"])
                method get_num_vars () =
                        num_vars
        end;;


type symbols =
        | Sym of string
        | SymList of string list
;;

exception MatchError of string;;
exception ParseError of string;;



let rec skip_meta_statements () =
        let t = peek () in
        if t#tok_type = "comment" then (
                let nxt = next () in
                print_endline nxt#value;
                skip_meta_statements ()
        )
;;

let match_tok tok =
        let t = peek () in 
        if tok#value = t#value then (
                ignore (next ());
                if t#tok_type <> "end" then (
                skip_meta_statements ())
        )
        else (
                raise (MatchError (concat " " ["Expcted"; tok#value; "but seeing"; t#value]))
        )
;;

let comma () =
        match_tok (new token "symbol" ",")
;;

let semicolon () =
        match_tok (new token "symbol" ";")
;;



let type_name () =
        let peek = peek () in
        let tok = (
        match peek#value with
        | "int" -> "int"
        | "void" -> "void"
        | _ -> raise (ParseError (concat " " ["Expected type_name, seeing"; peek#value])))
        in
        match_tok (new token "reserved" tok);
        tok
;;

let rec parameter_list fn = 
        let rec parameter_list_tail fn =
                let pk = peek () in
                if pk#value = "," then (
                        comma ();
                        prog_code :=  concat "" [!prog_code; ","];
                        match_tok (new token "reserved" "int");
                        prog_code := concat "" [!prog_code; "int "];
                        let nxt = next () in
                        let name = nxt#value in
                        fn#add_param name;
                        prog_code := concat "" [!prog_code; name];
                        parameter_list_tail fn )
        in
        let peek = peek () in
        match peek#value with
        | "void" -> (match_tok (new token "reserved" "void");
                     prog_code := concat "" [!prog_code; "void"])
        | "int" -> (match_tok (new token "reserved" "int");
                    prog_code := concat "" [!prog_code; "int "];
                    let next = next () in
                    let name = next#value in
                    (fn#add_param name);
                    prog_code := concat "" [!prog_code; name];
                    (parameter_list_tail fn)
                    )
        | ")" -> ()
        | _ -> raise (ParseError (concat " " ["Expected parameter_list, seeing"; peek#value]))
;;

let func_decl () = 
        let t = type_name () in 
        prog_code := (concat "" [!prog_code; t; " "]);
        let peek = peek () in
        if peek#tok_type <> "ident" && peek#value <> "main" then (
                raise (ParseError (concat "" ["Expected ident, seeing "; peek#tok_type]))
        )
        else (
        let nxt = next () in
        let name = nxt#value in
        prog_code := (concat "" [!prog_code; name]);
        let (fn : function_)  = (try Hashtbl.find functions name
        with Not_found -> Hashtbl.add functions name (new function_); Hashtbl.find functions name) in
        (Hashtbl.add global_sym_tab name name);
        
        let nxt2 = next () in
        prog_code := concat "" [!prog_code; nxt2#value];
        (parameter_list fn);
        let nxt3 = next () in
        prog_code := concat "" [!prog_code; nxt3#value];
        (Hashtbl.find functions name)
        )
;;



let new_global_var () =
        num_global_vars := !num_global_vars + 1;
        (concat "" ["global["; (string_of_int (!num_global_vars - 1)); "]"])
;;

let rec global_id_list () =
        let rec global_id_list_tail () =
                let peek = peek () in
                if peek#value = "," then (
                        ignore (comma ());
                        ignore (global_id_list ())
                )
        in
        let next = next () in
        Hashtbl.add global_sym_tab next#value (new_global_var ());
        (global_id_list_tail ())
;;


let rec id_list (fn : function_) =
        let id_list_tail fn =
                let tok = peek () in
                match tok#value with
                | "," -> comma (); id_list fn
                | _ -> ()
        in
        let nxt = next () in
        fn#add_to_symtab nxt#value (fn#new_var ());
        (id_list_tail fn)
;;

let rec data_decls (fn : function_) = 
        let peek = peek () in
        if peek#value = "int" then (
                match_tok (new token "reserved" "int");
                (id_list fn);
                (semicolon ());
                (data_decls fn)
        )
;;


let rec func_tail fn =
        let peek = peek () in
        match peek#value with
        | ";" -> (semicolon (); prog_code := concat "" [!prog_code; ";\n";])
        | "{" -> (
                 match_tok (new token "symbol" "{");
                 prog_code := concat "" [!prog_code; "{\n"];
                 (data_decls fn);
                 (statements fn);
                 match_tok (new token "symbol" "}");
                 if (fn#get_num_vars ()) > 0 then (
                 prog_code := concat "" [!prog_code; "int mem["; (string_of_int (fn#get_num_vars ())); "];\n"]
                 );
                 prog_code := concat "" [!prog_code; (fn#code ())];
                 prog_code := concat "" [!prog_code; "}\n"]
        )
        | _ -> raise (ParseError (concat "" ["Expected ; or {, found "; peek#value]))

and addop () = 
        let plus = new token "symbol" "+" in
        let minus = new token "symbol" "-" in
        let tok = peek () in
        match tok#value with
        | "+" -> match_tok plus
        | "-" -> match_tok minus
        | _ -> raise (MatchError (concat " " ["Expected mulop, seeing a"; tok#value]))


and func () =
        let fn = func_decl () in
        func_tail fn


and func_list () =
        let peek = peek () in
        if peek#value = "int" || peek#value = "void" then (
                func ();
                func_list ()
        )


and  mulop () = 
        let times = new token "symbol" "*" in
        let divide = new token "symbol" "/" in
        let t = peek () in
        match t#value with
        | "*" -> match_tok times
        | "/" -> match_tok divide
        | _ -> raise (MatchError (concat " " ["Expected mulop, seeing a"; t#value]))


and in_first_of_expression () = 
        let t = peek () in
        match t#tok_type with
        | "ident" -> true
        | "number" -> true
        | "symbol" -> (t#value = "(") || (t#value = "-")
        | _ -> false


and break_statement fn =
        match_tok (new token "reserved" "break");
        semicolon ();
        (fn#append_code (concat "" ["goto "; !loop_exit_label; ";\n"]));
        ""
        


and continue_statement fn =
        match_tok (new token "reserved" "continue");
        semicolon ();
        (fn#append_code (concat "" ["goto "; !loop_begin_label; ";\n"]));
        ""
        


and expr_list fn =
        let rec expr_list_tail fn = (
                let peek = peek () in
                if peek#value = "," then (
                        let nxt = next () in
                        (concat " " [nxt#value; (expression fn); (expr_list_tail fn)])
                )
                else "")
        in
        if in_first_of_expression () then (
                let e = (expression fn) in
                let elt = (expr_list_tail fn) in
                (concat " " [e; elt;]))
        else ( "" )

and factor (fn : function_) =
        let rec factor_tail fn = 
                let t = peek () in
                match t#value with
                | "(" -> match_tok (new token "symbol" "(");
                        let ft_place = expr_list fn in
                        match_tok (new token "symbol" ")");
                        concat "" ["("; ft_place; ")"]
                | _ -> ""
                in
        let peek = peek () in
        match peek#tok_type with
        | "number" -> (let f_place = fn#new_var () in
                      let nxt = next () in
                      fn#append_code (concat " " [f_place; "="; nxt#value; ";\n"]);
                      f_place)
        | "symbol" -> (match peek#value with
                       | "(" -> (let f_place = fn#new_var () in
                                 match_tok (new token "symbol" "(");
                                 fn#append_code (concat " " [f_place; "="; (expression fn); ";\n"]);
                                 match_tok (new token "symbol" ")");
                                 f_place)
                       | "-" -> (let f_place = fn#new_var () in
                                 let nxt = next () in
                                 let nxt2 = next () in
                                 fn#append_code (concat " " [f_place; "="; nxt#value; nxt2#value; ";\n"]);
                                 f_place) 
                       | _ -> raise (ParseError (concat " " ["Unexpected token"; peek#value])))
        | "ident" -> (let nxt = next () in
                      let name = nxt#value in
                      let place = try (Hashtbl.find fn#sym_tab name)
                      with Not_found -> (try Hashtbl.find global_sym_tab name with Not_found -> "")
                      in
                      (concat "" [place; (factor_tail fn)]))
        | _ -> raise (ParseError (concat " " ["Unexpected token"; peek#value]))



and term fn =
        let rec term_tail (fn : function_) t1_left =
                let peek = peek () in
                if peek#value = "*" || peek#value = "/" then (
                        let t1_place = fn#new_var () in
                        let nxt = next () in
                        fn#append_code (concat " " [t1_place; "="; t1_left; nxt#value; (term fn); ";\n"]);
                        term_tail fn t1_place
                )
                else
                        t1_left
        in

        term_tail fn (factor fn)


and expression fn =
        let rec expression_tail fn e1_left =
                let peek = peek () in
                if peek#value = "+" || peek#value = "-" then (
                        let e1_place = fn#new_var () in
                        let nxt = next () in
                        fn#append_code (concat " " [e1_place; "="; e1_left; nxt#value; (term fn); ";\n"]);
                        expression_tail fn e1_place
                )
                else e1_left
        in
        expression_tail fn (term fn)


and program () =
        let peek = peek () in
        match peek#value with
        | "void" -> (
                    let nxt = next () in
                    prog_code := concat "" [!prog_code; nxt#value; " "];
                    let nxt2 = next () in
                    let fn_name = nxt2#value in
                    let fn = try (Hashtbl.find functions fn_name) with
                         Not_found -> (Hashtbl.add functions fn_name (new function_); Hashtbl.find functions fn_name) in
                    Hashtbl.add global_sym_tab fn_name fn_name;
                    prog_code := concat "" [!prog_code; fn_name];
                    let nxt3 = next () in
                    prog_code := concat "" [!prog_code; nxt3#value];
                    parameter_list fn;
                    let nxt4 = next () in
                    prog_code := concat "" [!prog_code; nxt4#value];
                    func_tail fn;
                    (func_list ())
        )
        | "int" -> (
                   ignore (next ());
                   let nxt = next () in
                   let name = nxt#value in
                   (program_tail name)
        )
        | _ -> ()
and program_tail name =
        let peek = peek () in
        if peek#value = "(" then (
                prog_code := concat "" [!prog_code; "int "; name];
                Hashtbl.add global_sym_tab name name;
                let fn = try (Hashtbl.find functions name)
                         with Not_found -> (Hashtbl.add functions name (new function_); Hashtbl.find functions name) in
                let nxt = next () in
                prog_code := concat "" [!prog_code; nxt#value];
                parameter_list fn;
                let nxt2 = next () in
                prog_code := concat "" [!prog_code; nxt2#value];
                func_tail fn;
                func_list ()
        )
        else (program_decl_tail name)
and program_decl_tail name =
        Hashtbl.add global_sym_tab name (new_global_var ());
        let peek = peek () in
        match peek#value with
        | "," -> (
                 comma ();
                 global_id_list ();
                 semicolon ();
                 program ()
        )
        | ";" -> (
                 semicolon ();
                 program ()
        )
        | _ -> raise (ParseError (concat "" ["Expected , or ; but found "; peek#value]))



and return_statement fn =
        let return_statement_tail fn =
                let pk = peek () in
                if pk#value = ";" then(
                        next (); pk#value
                )else(
                        let fn_place = expression fn
                        
                        and nxt = peek () in
                        match nxt#value with
                        | ";" -> match_tok(new token "symbol" ";");
                                 (concat " " [fn_place; nxt#value])
                        | _ -> raise(ParseError (concat " " ["Unexpected token"; nxt#value]))
                )
        and pk = peek () in
        match pk#value with
        | "return" -> match_tok (new token "reserved" "return");
                      let fn_place = return_statement_tail fn in
                      fn#append_code (concat " " ["return"; fn_place]);
                      (concat " " ["return"; fn_place])
        | _ -> raise(ParseError (concat " " ["Unexpected token"; pk#value]))


and printf_func_call fn =
        let printf_func_call_tail fn =
                let pk = peek () in
                match pk#value with
                | "," -> match_tok (new token "symbol" ",");
                        let fn_place = expression fn in
                        match_tok (new token "symbol" ")");
                        match_tok (new token "symbol" ";");
                        (concat " " [","; fn_place; ");"])
                | ")" -> match_tok (new token "symbol" ")");
                        match_tok (new token "symbol" ";");
                        ");"
                | _ -> raise(ParseError (concat " " ["Unexpected token"; pk#value]))
         in 
         match_tok (new token "reserved" "printf");
         match_tok (new token "symbol" "(");
         let pk = peek () in
         let string_place = pk#value in
         match_tok (new token "string" string_place);
         let curr_string = (concat " " ["printf("; string_place])
         and fn_place = printf_func_call_tail fn in
         fn#append_code (concat " " [curr_string; fn_place]);
         (concat " " [curr_string; fn_place])


and scanf_func_call fn =
        match_tok ( new token "reserved" "scanf");
        match_tok(new token "symbol" "(");

        let pk = peek () in
        let string_place = pk#value in
        match_tok (new token "string" string_place);
        match_tok (new token "symbol" ",");
        match_tok (new token "symbol" "&");
        let fn_place = expression fn in
        match_tok (new token "symbol" ")");
        match_tok (new token "symbol" ";");
        fn#append_code (concat "" ["scanf("; string_place; ", &"; fn_place; ");"]);
        (concat "" ["scanf("; string_place; ", &"; fn_place; ");"])


and general_function_call fn id =
        match_tok (new token "symbol" "(");
        let fn_place = expr_list fn in
        match_tok (new token "symbol" ")");
        match_tok (new token "symbol" ";");
        fn#append_code (concat " " [id; "("; fn_place; ");"]);
        (concat " " [id; "("; fn_place; ");"])


and assignment fn id = 
        match_tok (new token "symbol" "=");
        let fn_place = expression fn in
        match_tok(new token "symbol" ";");
        fn#append_code (concat " " [id; "="; fn_place; ";"]);
        (concat " " [id; "="; fn_place; ";"])


and condition fn =
        let comparision_op fn = 
                let pk = peek () in
                if List.mem pk#value ["=="; "!=";">";">=";"<";"<="] then(
                        match_tok (new token "symbol" pk#value);
                        pk#value)
                else(
                        raise(ParseError (concat " " ["Unexpected token"; pk#value]))
                )
        in
        let fn_place = expression fn 
        and fn2_place = comparision_op fn
        and fn3_place = expression fn in
        (concat " " [fn_place; fn2_place; fn3_place])


and condition_expression fn num_label =
        let condition_expression_tail fn labelNumber =
                let pk = peek () in
                match pk#value with
                | "||" -> match_tok (new token "symbol" "||");
                          let fn_place = condition fn in
                         (concat "" ["if("; fn_place; ") goto TRUE"; num_label; ";\n"])
                | "&&" -> match_tok (new token "symbol" "&&");
                           condition fn|> ignore;
                           ""
                | ")" -> ""
                | _ -> raise(ParseError (concat " " ["Unexpected token"; pk#value]))
        
        and fn_place = condition fn in
        let fn2_place = (concat "" ["if("; fn_place; ") goto TRUE"; num_label; ";\n"]) 
        and fn3_place = condition_expression_tail fn num_label
        in
        (concat " " [fn2_place; fn3_place])

and statement fn =
        let pk = peek () in
                match pk#value with
                | "return" -> return_statement fn
                | "break" -> break_statement fn
                | "continue" -> continue_statement fn
                | "printf" -> printf_func_call fn
                | "scanf" -> scanf_func_call fn
                | "if" ->  if_statement fn
                | "while" -> while_statement fn
                |  "}" -> ""
                | _ -> if pk#tok_type = "ident" then(
                        match_tok(new token "ident" pk#value);
                        let id = (try (Hashtbl.find fn#sym_tab pk#value) with Not_found -> try (Hashtbl.find global_sym_tab pk#value) with Not_found -> pk#value)
                        and nxt = peek () in
                        match nxt#value with
                        | "=" ->  (assignment fn id)
                        | "(" ->  (general_function_call fn id)
                        | _ -> raise(ParseError (concat " " ["Unexpected token"; nxt#value]))
                )else(
                        raise(ParseError (concat " " ["Unexpected token"; pk#value]))
                )


and block_statements fn  =
        match_tok (new token "symbol" "{");

        let statements_place = statements fn in
        match_tok (new token "symbol" "}");
        
        statements_place
        

and statements fn = 
        let pk = peek () in
                match pk#tok_type with
                | "reserved" -> let fn_place = statement fn
                                and fn2_place = statements fn in
                                (concat " " [fn_place; fn2_place])
                | "ident" -> let fn_place = statement fn
                                  and fn2_place = statements fn in
                                  (concat " " [fn_place; fn2_place])
                | "symbol" -> ""
                | _ -> raise (ParseError (concat " " ["Unexpected token"; pk#tok_type]))

and if_statement (fn : function_) =
         let else_statement (fn : function_) =
                let pk = peek () in
                match pk#value with
                | "else" ->
                        match_tok (new token "reserved" "else");
                        block_statements fn
                | _ -> ""
        in
        match_tok(new token "reserved" "if");
        match_tok(new token "symbol" "(");
        let num_label = fn#new_label () in
        let fn_place = condition_expression fn num_label in
        match_tok(new token "symbol" ")");
        fn#append_code (concat "" [fn_place; "\n goto FALSE"; num_label; ";\n TRUE"; num_label; ":"]);
        let trueCondition = block_statements fn in
        fn#append_code (concat "" ["\n goto OUT"; num_label; ";\n FALSE"; num_label; ":"]);
        let falseCondition = else_statement fn in
        fn#append_code  (concat "" ["\n OUT"; num_label; ":;\n"]);
        (concat "" [fn_place; "\n goto FALSE"; num_label; ";\n TRUE"; num_label;":"; trueCondition; "\n goto OUT"; num_label; ";\n FALSE"; num_label; ":"; falseCondition; "\n OUT"; num_label; ":;\n"])


and while_statement fn =
        match_tok (new token "reserved" "while");
        match_tok (new token "symbol" "(");
        let num_label = fn#new_label () in
        let fn_place = condition_expression fn num_label in
        loop_begin_label := (concat "" ["CONTINUE"; num_label]);
        loop_exit_label := (concat "" ["FALSE"; num_label]);
        match_tok(new token "symbol" ")");
        fn#append_code (concat "" ["CONTINUE"; num_label; ":\n"; fn_place; "\n goto FALSE"; num_label;  ";\n TRUE"; num_label; ":"]);
        let whileTrue = block_statements fn in
        fn#append_code (concat "" [ "\ngoto CONTINUE"; num_label; ";\nFALSE"; num_label; ":;"]);
        (concat "" ["CONTINUE"; num_label; ":\n"; fn_place; "\n goto FALSE"; num_label; ";\n TRUE"; num_label; ":"; whileTrue; "\ngoto CONTINUE"; num_label; ";\nFALSE"; num_label; ":;"])
;;

let parse () =
        skip_meta_statements ();
        program ();
        match_tok (new token "end" "");
        if !num_global_vars > 0 then (
        print_endline (concat "" ["int global ["; (string_of_int !num_global_vars); "]"; ";"; "\n"])
        );
        print_endline !prog_code;
;;

parse ();;
