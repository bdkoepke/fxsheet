namespace FxSheet

open ExcelDna.Integration
open System.Reflection
open System.Reflection.Emit
open FxSheet.Ast
open FxSheet.Compiler
open FxSheet.ILFunction
open FxSheet.ILExpr
open FxSheet.Parser
open System

module Compiler = 
    type ILCompiler() =
        interface ICompiler with
            member this.Compile (definition : Definition) = 
                let rec argTypeList =
                    let mutable locals = Set.empty
                    let rec exprTypeList = function
                        | Comparison(lhs, _, rhs) -> exprTypeList lhs @ exprTypeList rhs
                        | Logical(lhs, _, rhs) ->    exprTypeList lhs @ exprTypeList rhs
                        | Arithmetic(lhs, _, rhs) -> exprTypeList lhs @ exprTypeList rhs
                        | Unary(_, x) ->             exprTypeList x
                        | Type(x) ->                 match x with
                                                     | Ref r -> 
                                                        if Set.contains r locals then []
                                                        else locals <- Set.add r locals
                                                             [typeof<double>]
                                                     | SRef r -> failwith "Not implemented."
                                                     | _ ->      []
                        | Function(_, xs) ->         xs |> List.fold (fun acc x -> acc @ exprTypeList x) []
                    exprTypeList

                let emitComparison(il: ILGenerator) = function
                    | Eq ->  il.Emit(OpCodes.Ceq)
                    | Neq -> il.Emit(OpCodes.Ceq)
                             il.Emit(OpCodes.Ldc_I4_0)
                             il.Emit(OpCodes.Ceq)
                    | Lt ->  il.Emit(OpCodes.Clt)
                    | Leq -> il.Emit(OpCodes.Clt)
                             il.Emit(OpCodes.Ldc_I4_0)
                             il.Emit(OpCodes.Ceq)
                    | Gt ->  il.Emit(OpCodes.Cgt)
                    | Geq -> il.Emit(OpCodes.Cgt)
                             il.Emit(OpCodes.Ldc_I4_0)
                             il.Emit(OpCodes.Ceq)

                let emitLogical(il: ILGenerator) = function
                    | And -> il.Emit(OpCodes.And)

                let emitArithmetic(il: ILGenerator) f =
                    let pow = typeof<System.Math>.GetMethod("Pow", [|typeof<double>; typeof<double>|])
                    match f with
                    | Add -> il.Emit(OpCodes.Add)
                    | Sub -> il.Emit(OpCodes.Sub)
                    | Mul -> il.Emit(OpCodes.Mul)
                    | Div -> il.Emit(OpCodes.Div)
                    | Pow -> il.Emit(OpCodes.Call, pow)

                let emitUnary(il: ILGenerator) = function
                    | Neg -> il.Emit(OpCodes.Neg)
                    | Mod -> il.Emit(OpCodes.Ldc_R8, 100.0)
                             il.Emit(OpCodes.Div)

                let emitType' =
                    let mutable locals = Map.empty
                    let mutable nArgs = 0               
                    fun (il : ILGenerator) (type' : xltype) ->
                        match type' with
                        | Num(x) ->  il.Emit(OpCodes.Ldc_R8, x)
                        | Int(x) ->  il.Emit(OpCodes.Ldc_I8, x)
                        | Str(x) ->  il.Emit(OpCodes.Ldstr, x)
                        | Ref(x) ->  
                            if Map.containsKey x locals then
                                emitLdArg il (Map.find x locals)
                            else
                                locals <- Map.add x nArgs locals
                                emitLdArg il nArgs
                                nArgs <- nArgs + 1
                        | SRef(x) -> failwith "Unimplemented."
                        | Err(x) ->  failwith x
                        | Name(x) -> failwith x

                let emitType = emitType'

                let rec emitExpr (il : ILGenerator) expr =
                    let emitArraySet i x =
                        il.Emit(OpCodes.Dup)
                        emitLdcI4 il i
                        emitExpr il x
                        il.Emit(OpCodes.Box, typeof<double>)
                        il.Emit(OpCodes.Stelem_Ref)

                    let emitUdf (f: string) xs =
                        emitArrayNew il ((xs |> List.length) + 1)
                        emitArraySetStr il 0 f
                        xs |> List.iteri (fun i x -> emitArraySet (i + 1) x)
                        il.EmitCall(OpCodes.Call, this.GetType().GetMethod("runUdf"), null)
                    let emitXlf f xs =
                        emitLdcI4 il (Map.find f xlfMap)
                        emitArrayNew il (xs |> List.length)
                        xs |> List.iteri emitArraySet
                        il.EmitCall(OpCodes.Call, this.GetType().GetMethod("runXlf"), null)

                    match expr with
                    | Comparison(lhs, c, rhs) -> emitExpr il lhs
                                                 emitExpr il rhs
                                                 emitComparison il c
                    | Logical(lhs, l, rhs) ->    emitExpr il lhs
                                                 emitExpr il rhs
                                                 emitLogical il l
                    | Arithmetic(lhs, a, rhs) -> emitExpr il lhs
                                                 emitExpr il rhs
                                                 emitArithmetic il a
                    | Unary(u, x) ->             emitExpr il x
                                                 emitUnary il u
                    | Type(x) ->                 emitType il x
                    | Function(f, xs) ->         (if Map.containsKey f xlfMap then emitXlf else emitUdf) f xs

                let rec emitCell (il : ILGenerator) = function
                    | Expr(expr) ->      emitExpr il expr
                                         il.Emit(OpCodes.Ret)
                    | ArrayExpr(expr) -> emitExpr il expr
                                         il.Emit(OpCodes.Ret)

                let cell = qparse definition.Expression
                let dynamicMethod = new DynamicMethod(definition.Name,
                                                      typeof<double>,
                                                      argTypeList cell |> List.toArray)
                let il = dynamicMethod.GetILGenerator()
                emitCell il (Expr(cell))
                upcast dynamicMethod