type token
type state
type value
type expr
type conf 
exception TokenizeError of string
exception ParseError of string
val print_token : token -> unit
val print_token_list : token list -> unit
val output_expr_list : out_channel -> expr list -> unit
val output_expr : out_channel -> expr -> unit
val output_conf : out_channel -> conf -> unit
val output_conf_list : out_channel -> conf list -> unit
val save_conf_list : string -> conf list -> unit
val update_variable : conf list -> conf -> conf list
val update_variables : conf list -> conf list -> conf list
val tokenize : string -> token list
val parse_token_list : token list -> conf list
val parse : string -> conf list


    
    
  
