module BasicUtils

open System.IO
open BasicLexer
open BasicParser

let parseString text = 
    lineNumber <- ref 1
    let lexbuf = Lexing.LexBuffer<char>.FromString text
    try 
        start token lexbuf 
    with e ->  
        failwithf "Error in line %d: %s" !lineNumber e.Message
        
let parseFile filename =
    let text = File.ReadAllText(filename)
    parseString text
    
let writeFile filename content =
    File.WriteAllText(filename, content)