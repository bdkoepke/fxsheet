namespace FxSheet

module Ast =

    type xlconstNum = float
    type xlconstInt = int64
    type xlconstText = string
    type xlconstBool = bool
    type xlconstErr = NULL | DIV | VALUE | NAME | NUM | NA | REF

    type xlconst =
        | Num of xlconstNum
        | Int of xlconstInt
        | Text of xlconstText
        | Bool of xlconstBool
        | Err of xlconstErr

    type xlconstArray = xlconst list

    type xlrefName = string
    type xlrefCell = int * int
    type xlrefSheetCell = string * xlrefCell

    type xlref = 
        | Name of xlrefName
        | Cell of xlrefCell
        | SheetCell of xlrefSheetCell

    type xltype =
        | Const of xlconst
        | Ref of xlref

    type binary = | Eq | Neq | Lt | Leq | Gt | Geq | Add | Sub | Mul | Div | Pow | And
    type unary = | Add' | Neg | Mod

    type xlexpr =
        | Binary of xlexpr * binary * xlexpr
        | Unary of unary * xlexpr 
        | Type of xltype
        | Function of string * (xlexpr list) 
        | Array of xlconstArray

    type xlcell =
        | Const' of xlconst
        | Expr of xlexpr
        | ArrayExpr of xlexpr