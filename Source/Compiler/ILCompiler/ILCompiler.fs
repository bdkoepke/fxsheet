namespace FxSheet

open FxSheet.Ast
open FxSheet.Compiler
open FxSheet.ILExpr
open FxSheet.Parser
open System.Reflection.Emit

module Compiler =
    type ILCompiler() =
        interface ICompiler with
            member this.Compile(definition: Definition) =
                let rec argTypeList =
                    let mutable locals = Set.empty

                    let rec exprTypeList =
                        function
                        | Binary(lhs, _, rhs) -> exprTypeList lhs @ exprTypeList rhs
                        | Unary(_, x) -> exprTypeList x
                        | Type(x) ->
                            match x with
                            | Ref r ->
                                match r with
                                | Cell _ ->
                                    if Set.contains r locals then
                                        []
                                    else
                                        locals <- Set.add r locals
                                        [ typeof<double> ]
                                | _ -> failwith "Not implemented."
                            | _ -> []
                        | Function(_, xs) -> xs |> List.fold (fun acc x -> acc @ exprTypeList x) []
                        | Array _ -> failwith "Not implemented."

                    exprTypeList

                let emitBinary (il: ILGenerator) f =
                    let pow =
                        typeof<System.Math>.GetMethod ("Pow", [| typeof<double>; typeof<double> |])

                    match f with
                    | Eq -> il.Emit(OpCodes.Ceq)
                    | Neq ->
                        il.Emit(OpCodes.Ceq)
                        il.Emit(OpCodes.Ldc_I4_0)
                        il.Emit(OpCodes.Ceq)
                    | Lt -> il.Emit(OpCodes.Clt)
                    | Leq ->
                        il.Emit(OpCodes.Clt)
                        il.Emit(OpCodes.Ldc_I4_0)
                        il.Emit(OpCodes.Ceq)
                    | Gt -> il.Emit(OpCodes.Cgt)
                    | Geq ->
                        il.Emit(OpCodes.Cgt)
                        il.Emit(OpCodes.Ldc_I4_0)
                        il.Emit(OpCodes.Ceq)
                    | Add -> il.Emit(OpCodes.Add)
                    | Sub -> il.Emit(OpCodes.Sub)
                    | Mul -> il.Emit(OpCodes.Mul)
                    | Div -> il.Emit(OpCodes.Div)
                    | Pow -> il.Emit(OpCodes.Call, pow)
                    | And -> il.Emit(OpCodes.And)

                let emitUnary (il: ILGenerator) =
                    function
                    | Add' -> ()
                    | Neg -> il.Emit(OpCodes.Neg)
                    | Mod ->
                        il.Emit(OpCodes.Ldc_R8, 100.0)
                        il.Emit(OpCodes.Div)

                let emitConst (il: ILGenerator) =
                    function
                    | Num(x) -> il.Emit(OpCodes.Ldc_R8, x)
                    | Int(x) -> il.Emit(OpCodes.Ldc_I8, x)
                    | Text(x) -> il.Emit(OpCodes.Ldstr, x)
                    | Bool(x) -> emitBool il x
                    | Err(x) -> failwith (x.ToString())

                let emitType' =
                    let mutable locals = Map.empty
                    let mutable nArgs = 0

                    fun (il: ILGenerator) (type': xltype) ->
                        match type' with
                        | Const(x) -> emitConst il x
                        | Ref(x) ->
                            match x with
                            | Cell _ ->
                                if Map.containsKey x locals then
                                    emitLdArg il (Map.find x locals)
                                else
                                    locals <- Map.add x nArgs locals
                                    emitLdArg il nArgs
                                    nArgs <- nArgs + 1
                            | _ -> failwith "Unimplemented."

                let emitType = emitType'

                let rec emitExpr (il: ILGenerator) expr =
                    let emitArraySet i x =
                        il.Emit(OpCodes.Dup)
                        emitLdcI4 il i
                        emitExpr il x
                        il.Emit(OpCodes.Box, typeof<double>)
                        il.Emit(OpCodes.Stelem_Ref)

                    let emitUdf f xs =
                        emitArrayNew il ((xs |> List.length) + 1)
                        emitArraySetStr il 0 f
                        xs |> List.iteri (fun i -> emitArraySet (i + 1))
                        il.EmitCall(OpCodes.Call, typeof<ILFunction>.GetMethod ("runUdf"), null)

                    let emitXlf f xs =
                        emitLdcI4 il (Map.find f ILFunction.xlfMap)
                        emitArrayNew il (xs |> List.length)
                        xs |> List.iteri emitArraySet
                        il.EmitCall(OpCodes.Call, typeof<ILFunction>.GetMethod ("runXlf"), null)

                    let emitIf _ xs =
                        match xs with
                        | test :: lhs :: [ rhs ] ->
                            let equal = il.DefineLabel()
                            let not_equal = il.DefineLabel()
                            let end_if = il.DefineLabel()

                            emitExpr il test
                            il.Emit(OpCodes.Brfalse_S, not_equal)

                            il.MarkLabel(equal)
                            emitExpr il lhs
                            il.Emit(OpCodes.Br, end_if)

                            il.MarkLabel(not_equal)
                            emitExpr il rhs

                            il.MarkLabel(end_if)
                        | _ -> failwith "Too many parameters for IF."

                    match expr with
                    | Binary(lhs, c, rhs) ->
                        emitExpr il lhs
                        emitExpr il rhs
                        emitBinary il c
                    | Unary(u, x) ->
                        emitExpr il x
                        emitUnary il u
                    | Type(x) -> emitType il x
                    | Function(f, xs) ->
                        (if Map.containsKey f ILFunction.xlfMap then
                             emitXlf
                         else
                             (if f = "if" then emitIf else emitUdf))
                            f
                            xs
                    | Array _ -> failwith "Not implemented."

                let rec emitCell (il: ILGenerator) =
                    function
                    | Const'(c) -> emitConst il c
                    | Expr(expr) ->
                        emitExpr il expr
                        il.Emit(OpCodes.Ret)
                    | ArrayExpr(expr) ->
                        emitExpr il expr
                        il.Emit(OpCodes.Ret)

                let cell = qparse definition.Expression

                let dynamicMethod =
                    DynamicMethod(definition.Name, typeof<double>, argTypeList cell |> List.toArray)

                let il = dynamicMethod.GetILGenerator()
                emitCell il (Expr(cell))
                upcast dynamicMethod
