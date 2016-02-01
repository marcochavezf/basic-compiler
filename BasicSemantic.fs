(*
 * Análisis Semántico de Compilador BASIC 
 * Marco Chávez
 *)
 
module BasicSemantic

open BasicAST
open BasicUtils

let semanticError lineNum message =
    failwithf "Error in line %d: %s" lineNum message

let getNumber line expr =
    match expr with
    | Number (num) ->
      num
    | _ -> semanticError line (sprintf "this expression is not a Number %A" expr)
    
let rec checkExpression symbolTable expr =
    let scalarT, vectorT, matrixT, fnT = symbolTable
    match expr with
    | Escalar (line, varName) ->
        if List.exists (fun e -> e = varName) scalarT then
            symbolTable
        else 
            let newScalarT = List.append scalarT [varName] 
            (newScalarT, vectorT, matrixT, fnT)
    | Vector (line, varName, expr) ->
        if (not(Map.containsKey varName vectorT)) then
            let newVectorT = Map.add varName 11 vectorT
            (scalarT, newVectorT, matrixT, fnT)
        else
            symbolTable
    | Matrix (line, varName, expr1, expr2) ->
        if (not(Map.containsKey varName matrixT)) then
            let newMatrixT = Map.add varName (11, 11) matrixT
            (scalarT, vectorT, newMatrixT, fnT)
        else
            symbolTable
    | Plus (line, expr1, expr2) ->
        let symbolTable = checkExpression symbolTable expr1
        let symbolTable = checkExpression symbolTable expr2
        symbolTable
    | Minus (line, expr1, expr2) ->
        let symbolTable = checkExpression symbolTable expr1
        let symbolTable = checkExpression symbolTable expr2
        symbolTable
    | Times (line, expr1, expr2) ->
        let symbolTable = checkExpression symbolTable expr1
        let symbolTable = checkExpression symbolTable expr2
        symbolTable
    | Divided (line, expr1, expr2) ->
        let symbolTable = checkExpression symbolTable expr1
        let symbolTable = checkExpression symbolTable expr2
        symbolTable
    | And (line, expr1, expr2) ->
        let symbolTable = checkExpression symbolTable expr1
        let symbolTable = checkExpression symbolTable expr2
        symbolTable
    | Grtr (line, expr1, expr2) ->
        let symbolTable = checkExpression symbolTable expr1
        let symbolTable = checkExpression symbolTable expr2
        symbolTable
    | Less (line, expr1, expr2) ->
        let symbolTable = checkExpression symbolTable expr1
        let symbolTable = checkExpression symbolTable expr2
        symbolTable
    | Neg (line, expr) ->
        let symbolTable = checkExpression symbolTable expr
        symbolTable
    | _ -> symbolTable

let checkDim symbolTable expr =
    let scalarT, vectorT, matrixT, fnT = symbolTable
    match expr with
    | Vector (line, varName, expr) ->
        let num = int(getNumber line expr)
        let newVectorT = Map.add varName num vectorT
        (scalarT, newVectorT, matrixT, fnT)
    | Matrix (line, varName, expr1, expr2) ->
        let num1 = int(getNumber line expr1)
        let num2 = int(getNumber line expr2)
        let newMatrixT = Map.add varName (num1, num2) matrixT
        (scalarT, vectorT, newMatrixT, fnT)
    | _ -> symbolTable   

let checkSymbols symbolTable stmt = 
    match stmt with
    | Let (line, var, expr) ->
        let symbolTable = checkExpression symbolTable var
        let symbolTable = checkExpression symbolTable expr
        symbolTable
    | Defn (line, fnName, arg, expr) ->
        let symbolTable = checkExpression symbolTable arg
        let symbolTable = checkExpression symbolTable expr
        let scalarT, vectorT, matrixT, fnT = symbolTable
        if List.exists (fun e-> e = fnName) fnT then
            symbolTable
        else
            let newfnT = List.append fnT [fnName]
            (scalarT, vectorT, matrixT, newfnT)
    | Dim (line, exprList) ->
        List.fold checkDim symbolTable exprList
    | For (line, var, expr1, expr2, expr3) ->
        let symbolTable = checkExpression symbolTable var
        let symbolTable = checkExpression symbolTable expr1
        let symbolTable = checkExpression symbolTable expr2
        let symbolTable = checkExpression symbolTable expr3
        symbolTable
    | Print (line, exprList) ->
        List.fold checkExpression symbolTable exprList
    | Read (line, exprList) ->
        List.fold checkExpression symbolTable exprList
    | Condition (line, expr, returnLine) ->
        checkExpression symbolTable expr
    | _ -> symbolTable

let getPositionTup numLine =
    match numLine with
    | Position(phi,log) ->
        (phi,log)

let getLineNumbers stmt =
    match stmt with
    | Let(numLine, _, _ ) ->
        getPositionTup numLine
    | Dim(numLine, _ ) ->
        getPositionTup numLine
    | Print(numLine, _ ) ->
        getPositionTup numLine
    | Condition(numLine, _, _ ) ->
        getPositionTup numLine
    | Return(numLine ) ->
        getPositionTup numLine
    | Stop(numLine ) ->
        getPositionTup numLine
    | GoSub(numLine, _ ) ->
        getPositionTup numLine
    | GoTo(numLine, _ ) ->
        getPositionTup numLine
    | Next(numLine, _ ) ->
        getPositionTup numLine
    | Defn(numLine,_ , _, _ ) ->
        getPositionTup numLine
    | For(numLine, _, _, _, _ ) ->
        getPositionTup numLine
    | Read(numLine, _ ) ->
        getPositionTup numLine
    | Data(numLine, _ ) ->
        getPositionTup numLine
    | Rem(numLine) ->
        getPositionTup numLine
    | End(numLine) ->
        getPositionTup numLine

let compareLines stmt1 stmt2 =
    let phy1, line1 = getLineNumbers stmt1
    let phy2, line2 = getLineNumbers stmt2
    if line1 = line2 then
        semanticError phy2 (sprintf "Duplicated Line Number...")
    elif line1 > line2 then
        semanticError phy2 (sprintf "Incorrect Lines Order...")
    else 
        stmt2
   
let checkEnd stmtList = 
    let endStmt = List.head (List.rev stmtList)
    let line = List.length stmtList
    match endStmt with
    | End(line) -> 
        ()
    | _ -> semanticError line (sprintf "END is not the last instruction...")

let checkData stmt =
    match stmt with
    | Data( numLine, _) ->
        true
    | _ ->
        false

let checkRead stmt =
    match stmt with
    | Read( numLine, _) ->
        true
    | _ -> 
        false

let checkConsistencyRD stmtList =
    try
        let readStmt = List.find checkRead stmtList
        let phi, log = getLineNumbers readStmt
        try 
           ignore(List.find checkData stmtList)
        with 
           | :? System.Collections.Generic.KeyNotFoundException -> 
               semanticError phi (sprintf "A READ statement needs at least one DATA statement...")   
    with 
        | :? System.Collections.Generic.KeyNotFoundException -> ()
    
let rec checkExpressionFn fnT expr = 
    match expr with
    | Function (line, funName, expr) -> 
        let defaultFun = ["SIN"; "COS"; "TAN"; "ATN"; "EXP"; "ABS"; "LOG"; "SQR"; "RND"; "INT" ]
        if not (List.exists (fun e -> e = funName) fnT) then
            if not (List.exists (fun r -> r = funName) defaultFun) then 
                semanticError line (sprintf "This function doesn't exist...")
        checkExpressionFn fnT expr
    | Plus (line, expr1, expr2) ->
        checkExpressionFn fnT expr1
        checkExpressionFn fnT expr2
    | Minus (line, expr1, expr2) ->
        checkExpressionFn fnT expr1
        checkExpressionFn fnT expr2
    | Times (line, expr1, expr2) ->
        checkExpressionFn fnT expr1
        checkExpressionFn fnT expr2
    | Divided (line, expr1, expr2) ->
        checkExpressionFn fnT expr1
        checkExpressionFn fnT expr2
    | And (line, expr1, expr2) ->
        checkExpressionFn fnT expr1
        checkExpressionFn fnT expr2
    | Grtr (line, expr1, expr2) ->
        checkExpressionFn fnT expr1
        checkExpressionFn fnT expr2
    | Less (line, expr1, expr2) ->
        checkExpressionFn fnT expr1
        checkExpressionFn fnT expr2
    | Neg (line, expr) ->
        checkExpressionFn fnT expr
    | _ -> ()
        
let checkFunctions symbolTable stmt = 
    let scalarT, vectorT, matrixT, fnT = symbolTable
    match stmt with
    | Let (line, var, expr) ->
        checkExpressionFn fnT expr
    | Defn (line, fnName, arg, func) ->
        checkExpressionFn fnT func
    | For (line, var, expr1, expr2, expr3) ->
        checkExpressionFn fnT expr1
        checkExpressionFn fnT expr2
        checkExpressionFn fnT expr3
    | Print (line, exprList) ->
        List.iter (fun expr -> checkExpressionFn fnT expr) exprList
    | Condition (line, expr, returnLine) ->
        checkExpressionFn fnT expr
    | _ -> ()

let getId expr =
    match expr with
    | Escalar(line, varName) -> varName
    | _ -> ""
    
let checkNext varF stmt =
    match stmt with
    | Next(_,varN) ->
        getId(varF) = getId(varN)
    | _ ->
        false

let checkForStmt stmtList stmt =
    match stmt with
    | For(numLine,varF,_,_,_) ->
        try 
           ignore(List.find (fun stmt -> checkNext varF stmt) stmtList)
        with
           | :? System.Collections.Generic.KeyNotFoundException -> 
               let phy, log = getPositionTup numLine
               semanticError phy (sprintf "This FOR statement needs a correct NEXT statement...")
    | _ -> ()
    List.tail stmtList
    
let checkFor varN stmt =
    match stmt with
    | For(_,varF,_,_,_)  ->
        getId(varF) = getId(varN)
    | _ ->
        false
    
let checkNextStmt stmt stmtList = 
    match stmt with
    | Next(numLine, varN) ->
        try 
           ignore(List.find (fun stmt -> checkFor varN stmt) stmtList)
        with
           | :? System.Collections.Generic.KeyNotFoundException -> 
               let phy, log = getPositionTup numLine
               let id = getId(varN)
               semanticError phy (sprintf "The var %A from NEXT statement doesn't match with any FOR statement..." id)
    | _ -> ()
    
let compareLineNumbers stmt lineTarget =
    let phy, log = getLineNumbers stmt
    int(lineTarget) = log
    
    
let checkGoto stmt stmtList = 
    match stmt with
    | GoTo(numLine, lineTarget) | GoSub(numLine, lineTarget) ->
        let phy, log = getPositionTup numLine
        let exists = List.exists (fun stmt -> compareLineNumbers stmt lineTarget) stmtList
        if not exists then
            semanticError phy (sprintf "The target number %A doesn't exist in the program..." (int(lineTarget)))
    | _ -> ()
    
let semanticAnalysis parseTree =
    match parseTree with
    | Program (stmtList) ->
        let symbolTable = List.fold checkSymbols (List.empty, Map.empty, Map.empty, List.empty) stmtList
        ignore(List.reduce (fun stmt1 stmt2 -> compareLines stmt1 stmt2) stmtList)
        checkConsistencyRD stmtList
        checkEnd stmtList
        List.iter (fun stmt -> checkFunctions symbolTable stmt) stmtList
        ignore(List.fold checkForStmt stmtList stmtList)
        List.iter (fun stmt -> checkNextStmt stmt stmtList) stmtList
        List.iter (fun stmt -> checkGoto stmt stmtList) stmtList
        symbolTable                                          
