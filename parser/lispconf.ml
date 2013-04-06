open List;;
open Char;;

module type CONF_PARSER = sig
  type token
  
  type conf_value
  
  type conf
  
  val tokenize : string -> token list
end

  (* module LispParser : CONF_PARSER = struct *)
  
type token = 
  Integer of string
| Float of string
| String of string
| Plus
| Minus
| Mul
| Div
| OpenParen 
| CloseParen
    
type state =
  Open
| Closed
| Comment

type value = 
  IntVal of int
| FloatVal of float
| StrVal of string
| Id of string

type expr =
  Val of value
| Sum of expr list
| Diff of expr list
| Prod of expr list
| Quot of expr list
| List of expr list
    
type conf = string * expr
  
exception TokenizeError of string
    
let tokenize ( filename : string ) = 
  let ic : in_channel = open_in filename 
  and merge ( d : char ) ( t : token ) =
    match t with
    | Integer(s) -> (
      match d with
        '0' .. '9' -> Integer( s ^ escaped d )
      | '.' -> Float( s ^ escaped d )
      | _ -> String( s ^ escaped d )
    )
    | Float(s) -> (
      match d with
        '0' .. '9' -> Float( s ^ escaped d )
      | _ -> String( s ^ escaped d )
    )
    | String(s) -> String( s ^ escaped d )
    | _ -> raise (TokenizeError "failed to tokenize") 
  in let rec tokenize_iter ( accu : token list ) ( st : state ) =
       try let ch = input_char ic
           in match st with
             Comment -> 
               if '\n' == ch 
               then tokenize_iter accu Closed 
               else tokenize_iter accu Comment
           | Closed -> (
             match ch with
               ' ' | '\n' -> tokenize_iter accu Closed
             | ';' -> tokenize_iter accu Comment
             | '(' -> tokenize_iter (OpenParen::accu) Closed
             | ')' -> tokenize_iter (CloseParen::accu) Closed
             | '+' -> tokenize_iter (Plus::accu) Closed
             | '-' -> tokenize_iter (Minus::accu) Closed
             | '*' -> tokenize_iter (Mul::accu) Closed
             | '/' -> tokenize_iter (Div::accu) Closed
             | '0' .. '9' -> tokenize_iter (Integer( escaped ch )::accu) Open
             | '.' -> tokenize_iter (Float( "0." )::accu) Open
             | _ -> tokenize_iter (String( escaped ch )::accu) Open
           )
           | Open -> (
             match ch with 
               ' ' | '\n' -> tokenize_iter accu Closed
             | '(' -> tokenize_iter (OpenParen::accu) Closed
             | ')' -> tokenize_iter (CloseParen::accu) Closed
             | _ -> tokenize_iter ((merge ch (hd accu))::(tl accu)) Open
           )
       with End_of_file -> rev accu
     in tokenize_iter [] Closed;;

let parse_token_list ( tklst : token list ) =
  let rec parse_iter ( accu : conf list ) ( lst : token list ) ( depth : int ) 
      ( env : string list ) ( var : string ) ( collect : expr list ) =
    match lst with
      tk::rest -> (
        match tk with
          OpenParen -> (
            match rest with
              name::remainder -> 
                if depth = 0 
                then parse_iter accu remainder 1 env name []
                else match name with
                  
          
      
  
    
    
(* end *)


