(*
 * BASIC Compiler
 * Marco Chávez 
 *)
open BasicUtils
open BasicSemantic
open BasicCodeGenerator

[<EntryPoint>]
let main (args : string[]) =      
    printfn "BASIC Compiler, version 1.0"
    printfn "Copyright (C) 2012 by Marco Chávez & Ravel Domínguez, ITESM CEM."
    try
        if args.Length <> 2 then
            failwith "Please specify input and output file names." 
        let parseTree = parseFile args.[0]
        //printfn "Parse Tree: %A" parseTree
        printfn "Syntax OK"
        let symbolTable = semanticAnalysis parseTree
        printfn "Semantics OK"
        (*
        let scalarT, vectorT, matrixT, fnT = symbolTable
        printfn "Scalar Vars: %A" scalarT
        printfn "Vector Vars: %A" vectorT
        printfn "Matrix Vars: %A" matrixT
        printfn "Functions: %A" fnT
        *)
        let code = generateCode symbolTable parseTree
        writeFile args.[1] code
        printfn "CIL code generated"
        0
    with
        | Failure(msg) ->
            printfn "%s" msg
            1
        | e -> 
            printfn "%s" e.Message
            1
