namespace FxSheet

module Ast =
    type xltypeNum = float
    type xltypeStr = string
    type xltypeBool = bool
    type xltypeRef = int * int
    type xltypeErr = string
    type xltypeSRef = string * xltypeRef
    type xltypeInt = int

    type xltypeName = string

    type xltype =
        | Num of xltypeNum
        | Int of xltypeInt
        | Str of xltypeStr
        | Ref of xltypeRef
        | SRef of xltypeSRef
        | Err of xltypeErr
        | Name of xltypeName

    type arithmetic = | Add | Sub | Mul | Div | Pow

    type comparison = | Eq | Neq | Lt | Leq | Gt | Geq

    type logical = | And

    type unary = | Neg | Mod

    type xlexpr =
        | Comparison of xlexpr * comparison * xlexpr
        | Logical of xlexpr * logical * xlexpr
        | Arithmetic of xlexpr * arithmetic * xlexpr
        | Unary of unary * xlexpr
        | Type of xltype
        | UdfExpr of string * (xlexpr list) 

    type xlcell =
        | Expr of xlexpr
        | ArrayExpr of xlexpr