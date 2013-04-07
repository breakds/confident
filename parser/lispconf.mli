open List;;
open Char;;
module type CONF_PARSER = sig
  type token
  type state
  type value
  type expr
  type conf 
  exception TokenizeError of string
  exception ParseError of string
  val print_token : token -> unit
  val print_token_list : token list -> unit
  val tokenize : string -> token list
  val parse_token_list : token list -> conf list
  val parse : string -> conf list
end

    
    
  
