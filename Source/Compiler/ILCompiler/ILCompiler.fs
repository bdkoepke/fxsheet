namespace FxSheet

open System.Reflection
open System.Reflection.Emit
open FxSheet.Ast
open FxSheet.Parser

type ILCompiler() =
    interface ICompiler with
        member this.Compile (definition : FxSheet.Definition) = 
            let cell = qparse definition.Expression

            let rec argTypeList =
                let mutable ctx = Set.empty
                let rec exprTypeList = function
                    | Comparison(lhs, c, rhs) -> exprTypeList lhs @ exprTypeList rhs
                    | Logical(lhs, l, rhs) ->    exprTypeList lhs @ exprTypeList rhs
                    | Arithmetic(lhs, a, rhs) -> exprTypeList lhs @ exprTypeList rhs
                    | Unary(u, x) ->             exprTypeList x
                    | Type(x) ->                 match x with
                                                 | Ref r ->  if ctx.Contains(r) then []
                                                             else
                                                                ctx <- ctx.Add(r)
                                                                [typeof<double>]
                                                 | SRef r -> [typeof<double>]
                                                 | _ ->      []
                    | UdfExpr(e, xs) ->          failwith "Not implemented..."
                function
                | Expr(expr) -> exprTypeList expr
                | ArrayExpr(expr) -> exprTypeList expr

            let dynamicMethod = new DynamicMethod(definition.Name,
                                                  typeof<double>,
                                                  // TODO: Fix this...
                                                  argTypeList (Expr(cell)) |> List.toArray)

            let il = dynamicMethod.GetILGenerator()

            let mutable nArgs = 0
            let emitLdArg n (il: ILGenerator)=
                match n with
                | 0 -> il.Emit(OpCodes.Ldarg_0)
                | 1 -> il.Emit(OpCodes.Ldarg_1)
                | 2 -> il.Emit(OpCodes.Ldarg_2)
                | 3 -> il.Emit(OpCodes.Ldarg_3)
                | _ -> il.Emit(OpCodes.Ldarg, n)

            let emitComparison(il: ILGenerator) = function
                | Eq ->  il.Emit(OpCodes.Ceq)
                | Neq -> failwith "Unimplemented..."
                    (* il.Emit(OpCodes.cne)
                         il.Emit(OpCodes.Ceq) *)
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

            let emitArithmetic(il: ILGenerator) = function
                | Add -> il.Emit(OpCodes.Add)
                | Sub -> il.Emit(OpCodes.Sub)
                | Mul -> il.Emit(OpCodes.Mul)
                | Div -> il.Emit(OpCodes.Div)
                | Pow -> failwith "Unimplemented..."

            let emitUnary(il: ILGenerator) = function
                | Neg -> il.Emit(OpCodes.Neg)
                | Mod -> failwith "Unimplemented..."

            let mutable ctx = Map.empty
            let emitType(il: ILGenerator) = function
                | Num(x) ->  il.Emit(OpCodes.Ldc_R8, x)
                | Int(x) ->  il.Emit(OpCodes.Ldc_I8, x)
                | Str(_) ->  failwith "Unimplemented..."
                | Ref(x) ->  
                    if ctx.ContainsKey(x) then
                        emitLdArg (ctx.Item x) il
                    else
                        ctx <- ctx.Add(x, nArgs)
                        emitLdArg nArgs il
                        nArgs <- nArgs + 1
                | SRef(_) -> emitLdArg nArgs il
                | Err(_) ->  failwith "Unimplemented..."
                | Name(_) -> failwith "Unimplemented..."

            let rec emitExpr (il : ILGenerator) = function
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
                | UdfExpr(_, xs) ->          xs |> List.iter (emitExpr il)

            let rec emitCell (il : ILGenerator) = function
                | Expr(expr) ->      emitExpr il expr
                                     il.Emit(OpCodes.Ret)
                | ArrayExpr(expr) -> emitExpr il expr
                                     il.Emit(OpCodes.Ret)
            
            // TODO: fix this...
            emitCell il (Expr(cell))
            upcast dynamicMethod 