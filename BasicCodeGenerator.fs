(*
 * BASIC Compiler
 * Marco Chávez
 *)
module BasicCodeGenerator

open BasicAST

let mapNextPosForData = ref Map.empty
let mapFunctions = ref Map.empty

let getLineNumber numLine =
    match numLine with
    | Position (lineF,lineL) -> (int(lineL))

let getLineNumberP numLine =
    match numLine with
    | Position (lineF,lineL) -> (int(lineF))
    
let createFunctions stmtList = 
    stmtList 
    |> List.iter (fun stmt ->
        match stmt with
        | Defn (numLine, fnName, varName, expr) -> 
            mapFunctions := Map.add fnName (varName, expr) !mapFunctions
        | _ -> ())
    
let genScalars scalarList =
    List.map (fun scalar -> 
            sprintf "\t\t.locals init (float64 '%s')\n" scalar)
            scalarList
            
let genVectors vectorMap =
    vectorMap
        |> Map.toList
        |> List.map (fun (key, value) ->
            (sprintf "\t\t.locals init (float64[] 'v%s')\n" key) +
            (sprintf "\t\tldc.i4 %A\n" value) +
            (sprintf "\t\tnewarr [mscorlib]System.Double\n") +
            (sprintf "\t\tstloc 'v%s'\n" key) )
        
let genMatrices matrixMap =
    matrixMap
        |> Map.toList
        |> List.map (fun (key, (row, col)) ->
            (sprintf "\t\t.locals init (float64[,] 'm%s')\n" key) +
            (sprintf "\t\tldc.i4 %A\n" row) +
            (sprintf "\t\tldc.i4 %A\n" col) +
            (sprintf "\t\tnewobj instance void float64[,]::'.ctor'(int32, int32)\n") +
            (sprintf "\t\tstloc 'm%s'\n" key))
        
let genVars symbolTable = 
    let scalarList, vectorMap, matrixMap, funcList = symbolTable
    String.concat "" (genScalars scalarList 
                    @ genVectors vectorMap
                    @ genMatrices matrixMap)

let genDouble numVal =
    sprintf "\t\tldc.r8 %A\n" numVal
    
let genInt numVal =
    if numVal <= 8 then
        sprintf "\t\tldc.i4.%i\n" numVal
    elif numVal <= 127 then
        sprintf "\t\tldc.i4.s %i\n" numVal
    else
        sprintf "\t\tldc.i4 %i\n" numVal 
        
    
let rec genBinOp op expr1 expr2 =
    sprintf
        "%s%s\t\t%s\n"
        (genExpr expr1)
        (genExpr expr2)
        op                
        
and genExpr expr =
    match expr with
    
    | Escalar (_, varName) ->
        sprintf "\t\tldloc '%s'\n" varName
        
    | Vector (_,varName, index) ->
        (sprintf "\t\tldloc 'v%s'\n" varName) +
        (sprintf "%s\t\tconv.i4\n" (genExpr index)) +
        (sprintf "\t\tldelem.r8\n")
        
    | Matrix (_,varName, index1, index2) ->
        (sprintf "\t\tldloc 'm%s'\n" varName) +
        (sprintf "%s\t\tconv.i4\n" (genExpr index1)) +
        (sprintf "%s\t\tconv.i4\n" (genExpr index2)) +
        (sprintf "\t\tcall instance float64 float64[,]::Get(int32, int32)\n")
    
    | Number (numVal) -> genDouble numVal
    
    | Neg (_, expr1) ->
        sprintf "\t\tldc.i4.0\n%s\t\tsub.ovf\n" (genExpr expr1)
        
    | And (lineNum, expr1, expr2) ->
        genBinOp "and" expr1 expr2

    | Equal (lineNum, expr1, expr2) ->
       genBinOp "ceq" expr1 expr2
          
    | Less (lineNum, expr1, expr2) ->
        genBinOp "clt" expr1 expr2
    
    | Grtr (lineNum, expr1, expr2) ->
        genBinOp "cgt" expr1 expr2

    | Geq (lineNum, expr1, expr2)->
        (genBinOp "clt" expr1 expr2) +
        (sprintf "\t\tldc.i4.0\n\t\tceq\n")        

    | Leq (lineNum, expr1, expr2)->
        (genBinOp "cgt" expr1 expr2) +
        (sprintf "\t\tldc.i4.0\n\t\tceq\n")
        
    | Dif (lineNum, expr1, expr2)->
        (genBinOp "ceq" expr1 expr2)
        + (sprintf "\t\tldc.i4.0\n\t\tceq\n")

    | Plus (lineNum, expr1, expr2) ->
        genBinOp "add" expr1 expr2

    | Minus (lineNum, expr1, expr2) ->
        genBinOp "sub" expr1 expr2

    | Times (lineNum, expr1, expr2) ->
        genBinOp "mul" expr1 expr2
    
    | Divided (lineNum, expr1, expr2) ->
        genBinOp "div" expr1 expr2
        
    | Power (lineNum, expr1, expr2)->
        (genExpr expr1)
        + (sprintf "\t\tconv.r8\n")
        + (genExpr expr2)
        + (sprintf "\t\tconv.r8\n")
        + (sprintf "\t\tcall float64 class [mscorlib]System.Math::Pow(float64, float64)\n")
    
    | Label (_,label) ->
        sprintf "\t\tldstr %s\n" label
        
    | Function (_, fnName, argExpr) ->
        try
            let varName, fnExpr = Map.find fnName !mapFunctions
            (genLet varName argExpr) + (genExpr fnExpr)
        with 
        | :? System.Collections.Generic.KeyNotFoundException ->
            if fnName = "RND" then
                (sprintf "\t\tldloc 'random'\n") + 
                (sprintf "\t\tcallvirt instance float64 class [mscorlib]System.Random::NextDouble()\n")
            else
                (genExpr argExpr) + 
                (sprintf "\t\tcall float64 class [mscorlib]System.Math::%s(float64)\n"
                (genPredifinedFun fnName))
                
    | Nothing (valString) ->
        valString
    
and genPredifinedFun fnName = 
    match fnName with
    | "SIN" ->  "Sin"
    | "COS" ->  "Cos"
    | "TAN" ->  "Tan"
    | "ATN" ->  "Tanh"
    | "EXP" ->  "Exp"
    | "ABS" ->  "Abs"
    | "LOG" ->  "Log"
    | "SQR" ->  "Sqrt"
    | "INT" ->  "Round"
    | _ -> ""
    
and genLet varName expr =
    match varName with
    | Escalar (_, varName) ->
        sprintf "%s\t\tstloc '%s'\n" (genExpr expr) varName
    | Vector (_, varName, index) ->
        (sprintf "\t\tldloc 'v%s'\n" varName) +
        (sprintf "%s\t\tconv.i4\n" (genExpr index)) +
        (sprintf "%s\t\tstelem.r8\n" (genExpr expr))
    | Matrix (_, varName, index1, index2) ->
        (sprintf "\t\tldloc 'm%s'\n" varName) +
        (sprintf "%s\t\tconv.i4\n" (genExpr index1)) +
        (sprintf "%s\t\tconv.i4\n" (genExpr index2)) +
        (sprintf "%s\t\tcall instance void float64[,]::Set(int32, int32, float64)\n" (genExpr expr))
    | _ -> ""

let genData stmtList =
    stmtList |>
    List.map (fun stmt -> 
        match stmt with
        | Data (_, numList) ->
             //dataList := List.append !dataList numList
            numList
            |> List.map (fun num ->
                (sprintf "\t\tldloc 'dataQueue'\n") +
                (genExpr num) + 
                (sprintf "\t\tcallvirt instance void class [System]System.Collections.Generic.Queue`1<float64>::Enqueue(!0)\n")) 
            |> String.concat ""
        | _ -> "")
    |> String.concat ""
    
let genPrint exprList =
    exprList
    |> List.map (fun expr -> 
        match expr with
        | Label (_,_) ->
            sprintf 
                "%s\t\tcall void class ['basiclib']'Basic'.'Utils'::'Print'(string)\n" 
                (genExpr expr)
        | _ ->
            sprintf 
                "%s\t\tcall void class ['basiclib']'Basic'.'Utils'::'Print'(float64)\n" 
                (genExpr expr))
    |> String.concat ""
    |> sprintf "%s\t\tcall void class ['basiclib']'Basic'.'Utils'::'PrintNewLine'()\n"
    
let genLabel numLine =
    match numLine with
    | Position (lineF, lineL) -> sprintf "\t$%06d:\n" lineL

let genRead exprList =
    exprList
    |> List.map (fun expr ->
        let label = Nothing("\t\tldloc 'dataQueue'\n\t\tcallvirt instance !0 class [System]System.Collections.Generic.Queue`1<float64>::Dequeue()\n")
        "\t\tldloc 'dataQueue'\n" +
        "\t\tcallvirt instance int32 class [System]System.Collections.Generic.Queue`1<float64>::get_Count()\n" +
        "\t\tbrfalse $999999\n" +
        (genLet expr label))
    |> String.concat ""

let genCondition expr lineTarget =
    (genExpr expr) + 
    sprintf "\t\tbrtrue $%06d\n" (int(lineTarget))
    
let getNumLine stmt =
    match stmt with
    | Let (numLine, _, _) -> getLineNumber numLine
    | Dim (numLine, _ ) -> getLineNumber numLine
    | Print (numLine, _) -> getLineNumber numLine
    | Condition (numLine, _, _) -> getLineNumber numLine
    | Return (numLine) -> getLineNumber numLine
    | Stop (numLine) -> getLineNumber numLine
    | GoSub (numLine, _) -> getLineNumber numLine
    | GoTo (numLine, _) -> getLineNumber numLine
    | Next (numLine, _) -> getLineNumber numLine
    | Defn (numLine, _, _, _) -> getLineNumber numLine
    | For (numLine, _, _, _, _) -> getLineNumber numLine
    | Read (numLine, _) -> getLineNumber numLine
    | Data (numLine, _) -> getLineNumber numLine
    | Rem (numLine) -> getLineNumber numLine
    | End (numLine) -> getLineNumber numLine
    
let getVarName expr = 
    match expr with
    | Escalar (_,varName) | Vector (_,varName,_) | Matrix (_,varName,_,_) -> varName
    | _ -> ""

let getLogicLineNumber numLine = 
    match numLine with
    | Position(physicL, logicL) -> logicL
    
let getNextNumLineFromFor forStmt varFor stmtList =
    let iForStmt = List.findIndex (fun stmt -> forStmt = stmt) stmtList
    let endIndex = (List.length stmtList) - 1
    let i = ref (iForStmt+1)
    let nextNumLine = ref 0
    let numLineAfterFor = ref 0
    while !i < endIndex do
        match (List.nth stmtList !i) with
        | Next (numLine,varNext) ->
            if getVarName(varFor) = getVarName(varNext) then 
                nextNumLine := getLogicLineNumber numLine
                numLineAfterFor := getNumLine (List.nth stmtList (iForStmt + 1))
                i := endIndex
        | _ -> ()
        incr i
    (!nextNumLine, !numLineAfterFor)
   
let genFor numLine varName index endIndex stepF forStmt stmtList =
    let nextNumLine, numLineAfterFor = getNextNumLineFromFor forStmt varName stmtList
    let forData = (varName, endIndex, stepF, numLineAfterFor)    
    mapNextPosForData := Map.add nextNumLine forData !mapNextPosForData 
    let nIndex = Minus (0, index, stepF)
    (genLet varName nIndex) +
    (sprintf "\t\tbr $%06d\n" nextNumLine)
    
let genNext numLine =
    let label = Nothing("")
    let nextNumLine = getLogicLineNumber numLine
    let varName, endIndex, stepF, numLineAfterFor = Map.find nextNumLine !mapNextPosForData
    (genLabel numLine) +
    (genExpr varName) + 
    (genExpr stepF) + 
    (sprintf "\t\tadd\n") +
    (genLet varName label) +
    (genExpr varName) + 
    (genExpr endIndex) + 
    (sprintf "\t\tldc.r8 1.0\n\t\tadd\n") +
    (sprintf "\t\tblt.un $%06d\n" numLineAfterFor)

let getLineAfterGoSub stmtList position =
    getNumLine (List.nth stmtList (position-1))
    
let genGoSub numLine goLine stmtList =
    let lineP = (getLineNumberP numLine) + 1
    (genLabel numLine) + 
    let lineAfterGS = getLineAfterGoSub stmtList lineP
    (sprintf "\t\tldloc 'linesAfterGS'\n") +
    (sprintf "\t\tldc.i4 %A\n" lineAfterGS) +
    (sprintf "\t\tcallvirt instance void class [System]System.Collections.Generic.Stack`1<int32>::Push(!0)\n") +
    (sprintf "\t\tbr $%06d\n" (int(goLine)))

let getLineGoSubs stmtList =
    stmtList
    |> List.map (fun stmt ->
        match stmt with
        | GoSub (numLine, _) -> 
            let lineP = (getLineNumberP numLine) + 1
            let lineAfterGoSub = (getLineAfterGoSub stmtList lineP)
            (sprintf "\t\tldloc 'tempLineAGS'\n") +
            (sprintf "\t\tldc.i4 %i\n" lineAfterGoSub) +
            (sprintf "\t\tbeq $%06d\n" lineAfterGoSub)
        | _ -> "")
    |> String.concat ""
    
let genReturn numLine stmtList =
    (genLabel numLine) +
    (sprintf "\t\tldloc 'linesAfterGS'\n") +
    (sprintf "\t\tcallvirt instance int32 class [System]System.Collections.Generic.Stack`1<int32>::get_Count()\n") +
    (sprintf "\t\tbrfalse $R%06d\n" (getLineNumber numLine)) +
    (sprintf "\t\tldloc 'linesAfterGS'\n") +
    (sprintf "\t\tcallvirt instance !0 class [System]System.Collections.Generic.Stack`1<int32>::Pop()\n") +
    (sprintf "\t\tstloc 'tempLineAGS'\n") + 
    (getLineGoSubs stmtList) +
    (sprintf "\t$R%06d:\n" (getLineNumber numLine))

let genStmt stmt stmtList =
    match stmt with 
    | Let (numLine, varName, expr)  -> (genLabel numLine) + (genLet varName expr)
    | Print (numLine, exprList)     -> (genLabel numLine) + (genPrint exprList)
    | Condition (numLine, expr, lineTarget) -> (genLabel numLine) + (genCondition expr lineTarget)
    | Return (numLine) ->  (genReturn numLine stmtList)
    | Stop (numLine) -> (genLabel numLine) + (sprintf "\t\tbr $999999\n")
    | GoSub (numLine, goLine) -> (genGoSub numLine goLine stmtList)
    | GoTo (numLine, goLine) -> (genLabel numLine) + (sprintf "\t\tbr $%06d\n" (int(goLine)))
    | For (numLine, varName, index, endIndex, step) -> (genLabel numLine) + (genFor numLine varName index endIndex step stmt stmtList)
    | Next (numLine,_) -> (genNext numLine)
    | Read (numLine, exprList) -> (genLabel numLine) + (genRead exprList)
    | End (numLine) -> (genLabel numLine) + (sprintf "\t\tbr $999999\n")
    | Rem (numLine) -> (genLabel numLine)
    | _ -> ""
        
let genStmts stmtList =
    stmtList
    |> List.map (fun stmt -> genStmt stmt stmtList) 
    |> String.concat ""

let obtainNextStmtLine parseTree = 
    match parseTree with
    | Program (stmtList) ->
        getLineAfterGoSub stmtList

let generateCode symbolTable parseTree =
    match parseTree with 
        | Program (stmtList) -> 
            createFunctions stmtList         
            let prologue =
                "// Code generated by the basic compiler.\n\n"
                + ".assembly extern mscorlib {}\n"
                + ".assembly extern System {"
                + "\n\t.ver 4:0:0:0\n\t.publickeytoken = (B7 7A 5C 56 19 34 E0 89 )\n}\n"
                + ".assembly 'basic' {}\n\n"
                + ".assembly extern 'basiclib' {}\n\n"
                + ".class public 'BasicProgram' extends "
                + "['mscorlib']'System'.'Object' {\n"
                + "\t.method public static void 'start'() {\n"
                + "\t\t.entrypoint\n"
                + "\t\t.maxstack 1024\n"
                + "\t\t.locals init (class [mscorlib]System.Random 'random')\n"
                + "\t\tnewobj instance void class [mscorlib]System.Random::'.ctor'()\n"
                + "\t\tstloc 'random'\n"
                + "\t\t.locals init (class [System]System.Collections.Generic.Queue`1<float64> 'dataQueue')\n" 
                + "\t\tnewobj instance void class [System]System.Collections.Generic.Queue`1<float64>::'.ctor'()\n" 
                + "\t\tstloc 'dataQueue'\n"
                + "\t\t.locals init (class [System]System.Collections.Generic.Stack`1<int32> 'linesAfterGS')\n" 
                + "\t\tnewobj instance void class [System]System.Collections.Generic.Stack`1<int32>::'.ctor'()\n" 
                + "\t\tstloc 'linesAfterGS'\n"  
                + "\t\t.locals init (int32 'tempLineAGS')\n"
            let epilogue =
                "\t$999999: \n\t\tret\n"
                + "\t}\n"
                + "}"          
            prologue 
                + (genData stmtList)
                + (genVars symbolTable)
                + (genStmts stmtList)
                + epilogue
