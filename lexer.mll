{
    open Lexing
    open Parser

    exception SyntaxError of string

    let next_line lexbuf =
        let pos = lexbuf.lex_curr_p in
        lexbuf.lex_curr_p <- { pos with pos_bol = lexbuf.lex_curr_p; pos_lnum = pos.pos_lnum + 1 }
}

let digit = ['0'-'9']
let sign = ['+' '-']
let int = sign? digit+
let float = sign? digit+ '.' digit*

let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

rule read = parse
    | whitespace { read lexbuf }
    | newline { next_line lexbuf; read lexbuf }
    | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
    | float { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
    | "Sph" { SPH }
    | "Surf" { SURF }
    | "Point" { POINT }
    | "Sun" { SUN }
    | '{' { LEFT_BRACE }
    | '}' { RIGHT_BRACE }
    | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
    | eof { EOF }