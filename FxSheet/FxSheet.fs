namespace FxSheet

open ExcelDna.ComInterop
open ExcelDna.Integration
open Microsoft.Office.Interop
open System.Reflection
open System.Reflection.Emit

type FxSheet() = 
    let Application : Excel.Application = downcast ExcelDnaUtil.Application
    let FxSheetRegister = new Excel.AppEvents_AfterCalculateEventHandler(
                              fun () -> Application.Run("FxSheetRegister") |> ignore)
    interface IExcelAddIn with
        member this.AutoClose() =
            ComServer.DllUnregisterServer() |> ignore
            Application.remove_AfterCalculate(FxSheetRegister)
        member this.AutoOpen() =
            ComServer.DllRegisterServer() |> ignore
            Application.add_AfterCalculate(FxSheetRegister)

module Definitions =
    [<NoComparison>]
    type Definition = {
        Name : string;
        Expression : string;
        Description : Option<string>;
        ArgumentDescriptions : System.Collections.Generic.List<obj> }

    [<NoComparison>]
    type CompiledDefinition = {
        Method : MethodInfo;
        FunctionAttribute : ExcelFunctionAttribute;
        ArgumentAttributes : System.Collections.Generic.List<obj> }

    type DefinitionCompiler() =
        member x.Compile(d : Definition) =
            let stringToOpCode(node : string) =
                match node with
                | "*" -> OpCodes.Mul
                | "/" -> OpCodes.Div
                | "-" -> OpCodes.Sub
                | "+" -> OpCodes.Add
                | _ -> failwith "Unknown opcode string."

            let dynamicMethod =
                let r = new DynamicMethod(d.Name, typeof<double>, [|typeof<double>;typeof<double>|])
                let il = r.GetILGenerator()
                il.Emit(OpCodes.Ldarg_0)
                il.Emit(OpCodes.Ldarg_1)
                il.Emit(stringToOpCode(d.Expression))
                il.Emit(OpCodes.Ret)
                r

            let attribute =
                match d.Description with
                | Some x -> x
                | None -> ""
        
            { Method=dynamicMethod;
              FunctionAttribute=new ExcelFunctionAttribute(attribute);
              ArgumentAttributes=d.ArgumentDescriptions }

    type DefinitionManager(pending : List<Definition>,
                           registered : Map<string, Definition>,
                           compiler : DefinitionCompiler) = 
        let mutable pending = pending
        let mutable registered = registered

        member this.EnqueueDefinition(name : string, expression: string) =
            pending <- {Name=name;
                        Expression=expression;
                        Description=Option.None;
                        ArgumentDescriptions=new System.Collections.Generic.List<obj>()}
                        :: pending

        member this.RegisterDefinitions() =
            let methods = new System.Collections.Generic.List<MethodInfo>()
            let functionAttributes = new System.Collections.Generic.List<obj>()
            let argumentAttributes = new System.Collections.Generic.List<System.Collections.Generic.List<obj>>()

            let uniquePending  = pending |> List.filter(fun d -> not(registered.ContainsKey(d.Name)))
            uniquePending |> List.iter(fun p -> registered <- registered.Add(p.Name, p))
            let compiled = uniquePending |> List.map(fun d -> compiler.Compile(d))

            compiled |> List.iter(fun c -> methods.Add(c.Method))
            compiled |> List.iter(fun c -> functionAttributes.Add(c.FunctionAttribute))
            compiled |> List.iter(fun c -> argumentAttributes.Add(c.ArgumentAttributes))

            ExcelIntegration.RegisterMethods(methods, functionAttributes, argumentAttributes)
            pending <- List.Empty

module Functions =
    let private manager = new Definitions.DefinitionManager(List.Empty, Map.empty, new Definitions.DefinitionCompiler())

    [<ExcelFunction(Description = "Defines a new function.")>]
    let DEFINE (name : string, expression : string) =
        manager.EnqueueDefinition(name, expression)
        name

    [<ExcelCommand>]
    let FxSheetRegister() = manager.RegisterDefinitions()