(*
 * Análisis léxico y sintáctico de compilar BASIC 
 * Marco Chávez 
 *)
module BasicAST

type BasicType = UnknownType | IntType | BoolType

type NumLine =
     Position of int * int

type Expr =
    | Escalar of int * string
    | Vector of int * string * Expr
    | Matrix of int * string * Expr * Expr
    | Number of float
    | Neg of int * Expr
    | And of int * Expr * Expr
    | Less of int * Expr * Expr
    | Grtr of int * Expr * Expr
    | Leq of int * Expr * Expr
    | Geq of int * Expr * Expr
    | Dif of int * Expr * Expr
    | Power of int * Expr * Expr
    | Plus of int * Expr * Expr
    | Minus of int * Expr * Expr
    | Equal of int * Expr * Expr
    | Times of int * Expr * Expr
    | Divided of int * Expr * Expr
    | Label of int * string
    | Function of int * string * Expr
    | Nothing of string
  
type Stmt =
    | Let of NumLine * Expr * Expr
    | Dim of NumLine * Expr list
    | Print of NumLine * Expr list
    | Condition of NumLine * Expr * float
    | Return of NumLine
    | Stop of NumLine
    | GoSub of NumLine * double
    | GoTo of NumLine * double
    | Next of NumLine * Expr
    | Defn of NumLine * string * Expr * Expr
    | For of NumLine * Expr * Expr * Expr * Expr
    | Read of NumLine * Expr list
    | Data of NumLine * Expr list
    | Rem of NumLine
    | End of NumLine

type Prog = Program of Stmt list
