open List;;

module type CONF_PARSER = sig
  type token
  type conf_item
end

module LispParser : CONF_PARSER = struct
    
  type token = 
    Identifier of string
  | Integer of string
  | Float of string
  | String of string
  | LeftParen 
  | RightParen
      
  type state =
    Open
  | Closed

  exception TokenizeError of string
      
  let tokenize ( filename : string ) =
    let ic : in_channel = open_in filename 
    and merge 
    in
    let rec tokenize_iter ( accu : token list ) ( st : state ) =
      try match input_char ic with
        '(' -> tokenize_iter LeftParen::accu Closed
      | ')' -> tokenize_iter RightParen::accu Closed
      | '0' .. '9' as d -> (
        match state with
          Closed -> tokenize_iter Integer(Char.escaped d)::accu Open
        | Open -> (
          let 
          match hd accu with
            Identifier (s) -> 
          
        
        
      
end
