open List;;
open Char;;

module LispParser : CONF_PARSER = struct
    
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
  | Nil

  type expr =
    Val of value
  | Sum of expr list
  | Diff of expr list
  | Prod of expr list
  | Quot of expr list
  | List of expr list
      
  type conf = string * expr
    
  exception TokenizeError of string

  exception ParseError of string


  let print_token = function
    | Integer(s) -> print_string s
    | Float(s) -> print_string s
    | String(s) -> print_string ( "\"" ^ s ^ "\"" )
    | Plus -> print_char '+'
    | Minus -> print_char '-'
    | Mul -> print_char '*'
    | Div -> print_char '/'
    | OpenParen -> print_char '('
    | CloseParen -> print_char ')';;

  let print_token_list tklst =
    iter ( fun t -> print_token t; print_char ' ' ) tklst;
    print_char '\n';;

  

  
  
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
         with End_of_file -> close_in ic; rev accu
       in tokenize_iter [] Closed;;

  let parse_token_list ( tklst : token list ) =
    let rec next_expr ( lst : token list ) ( env : string list ) =
      match lst with
        OpenParen::name::l -> (
          let body, rest = complete_list l env
          in match name with
            Plus -> (Sum(body)), rest
          | Minus -> (Diff(body)), rest
          | Mul -> (Prod(body)), rest
          | Div -> (Quot(body)), rest
          | _ -> raise (ParseError "unrecognized operator")
        )
      | (Integer(s))::l -> (Val(IntVal( int_of_string s ))), l
      | (Float(s))::l -> (Val(FloatVal( float_of_string s ))), l
      | (String(s))::l -> if mem s env then (Val(Id( s ))), l else (Val(StrVal( s ))), l
      | something::l -> raise (ParseError "invalid syntax")
      | _ -> (Val( Nil )), [] (* empty lst case *)
    and complete_list ( lst : token list ) ( env : string list ) =
      match hd lst with
        CloseParen -> [], (tl lst)
      | _ -> let e, rest = next_expr lst env
             in let relst, remainder = complete_list rest env
                in (e::relst), remainder
    in let rec parse_iter ( accu : conf list ) ( lst : token list ) ( env : string list ) =
         match lst with
           [] -> rev accu
         | OpenParen::(String(var))::remainder -> (
           let body, rest = complete_list remainder env
           in match body with
             e::[] -> parse_iter ((var, e)::accu) rest (var::env)
           | e::r -> parse_iter ((var, List(body))::accu) rest (var::env)
           | _ -> raise (ParseError "invalid assignment value")
         )
         | _ -> raise (ParseError "invalid assignment form")
       in parse_iter [] tklst [];;

  let parse ( filename : string ) = parse_token_list (tokenize filename);;

end


