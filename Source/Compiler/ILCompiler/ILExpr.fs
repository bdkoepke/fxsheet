namespace FxSheet

open System.Reflection
open System.Reflection.Emit
open System

module ILExpr =
    let emitLdArg (il: ILGenerator) = function
        | 0 -> il.Emit(OpCodes.Ldarg_0)
        | 1 -> il.Emit(OpCodes.Ldarg_1)
        | 2 -> il.Emit(OpCodes.Ldarg_2)
        | 3 -> il.Emit(OpCodes.Ldarg_3)
        | n ->
            let opcode =
                if n < int(Byte.MaxValue) then
                    OpCodes.Ldarg_S
                else
                    OpCodes.Ldarg
            il.Emit(opcode, n)

    let emitStloc (il: ILGenerator) = function
        | 0 -> il.Emit(OpCodes.Stloc_0)
        | 1 -> il.Emit(OpCodes.Stloc_1)
        | 2 -> il.Emit(OpCodes.Stloc_2)
        | 3 -> il.Emit(OpCodes.Stloc_3)
        | n ->
            let opcode =
                if n < int(Byte.MaxValue) then
                    OpCodes.Stloc_S
                else
                    OpCodes.Stloc
            il.Emit(opcode, n)

    let emitLdloc (il: ILGenerator) = function
        | 0 -> il.Emit(OpCodes.Ldloc_0)
        | 1 -> il.Emit(OpCodes.Ldloc_1)
        | 2 -> il.Emit(OpCodes.Ldloc_2)
        | 3 -> il.Emit(OpCodes.Ldloc_3)
        | n ->
            let opcode =
                if n < int(Byte.MaxValue) then
                    OpCodes.Ldloc_S
                else
                    OpCodes.Ldloc
            il.Emit(opcode, n)

    let emitLdcI4 (il: ILGenerator) = function
        | 0 -> il.Emit(OpCodes.Ldc_I4_0)
        | 1 -> il.Emit(OpCodes.Ldc_I4_1)
        | 2 -> il.Emit(OpCodes.Ldc_I4_2)
        | 3 -> il.Emit(OpCodes.Ldc_I4_3)
        | 4 -> il.Emit(OpCodes.Ldc_I4_4)
        | 5 -> il.Emit(OpCodes.Ldc_I4_5)
        | 6 -> il.Emit(OpCodes.Ldc_I4_6)
        | 7 -> il.Emit(OpCodes.Ldc_I4_7)
        | 8 -> il.Emit(OpCodes.Ldc_I4_8)
        | n ->
            let opcode =
                if n < int(Byte.MaxValue) then
                    OpCodes.Ldc_I4_S
                else
                    OpCodes.Ldc_I4
            il.Emit(opcode, n)

    let emitArraySetStr (il: ILGenerator) i (x: string) =
        il.Emit(OpCodes.Dup)
        emitLdcI4 il i
        il.Emit(OpCodes.Ldstr, x)
        il.Emit(OpCodes.Stelem_Ref)

    let emitArrayNew il n =
        emitLdcI4 il n
        il.Emit(OpCodes.Newarr, typeof<obj>)