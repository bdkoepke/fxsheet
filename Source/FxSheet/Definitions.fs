namespace FxSheet

open System.Reflection
open ExcelDna.Integration
open FxSheet.Compiler

module Definitions =
    [<NoComparison>]
    type CompiledDefinition = {
        Method : MethodInfo;
        FunctionAttribute : ExcelFunctionAttribute;
        ArgumentAttributes : System.Collections.Generic.List<obj> }

    type DefinitionCompiler(compiler : ICompiler) =
        member x.Compile(d : Definition) =
            let attribute =
                match d.Description with
                | Some x -> x
                | None -> ""

            let dynamicMethod = compiler.Compile(d)
        
            { Method=dynamicMethod;
              FunctionAttribute= ExcelFunctionAttribute(attribute);
              ArgumentAttributes=d.ArgumentDescriptions
                              |> List.map(fun x -> x :> obj)
                              |> System.Linq.Enumerable.ToList } 

    type DefinitionManager(pending : List<Definition>,
                           registered : Map<string, Definition>,
                           compiler : DefinitionCompiler) = 
        let mutable pending = pending
        let mutable registered = registered
        (* static member ID (test: obj) : obj = test *)

        member this.EnqueueDefinition(name : string, expression: string) =
            pending <- {Name=name;
                        Expression=expression;
                        Description=Option.None;
                        ArgumentDescriptions=List.Empty}
                        :: pending
            name

        member this.RegisterDefinitions() =
            let methods = System.Collections.Generic.List<MethodInfo>()
            let functionAttributes = System.Collections.Generic.List<obj>()
            let argumentAttributes =
                System.Collections.Generic.List<
                System.Collections.Generic.List<obj>>()

            let uniquePending  =
                pending
                |> List.filter(fun d ->
                    (* TODO: Unregister existing functions... *)
                    not(registered.ContainsKey(d.Name)) ||
                    not((registered.Item d.Name).Expression.Equals(d.Expression)))
            uniquePending
                |> List.iter(fun d -> registered <- registered.Add(d.Name, d))
            let compiled = 
                uniquePending
                |> List.map(compiler.Compile)

            compiled |> List.iter(fun c -> methods.Add(c.Method))
            compiled |>
                List.iter(fun c -> functionAttributes.Add(c.FunctionAttribute))
            compiled |>
                List.iter(fun c -> argumentAttributes.Add(c.ArgumentAttributes))

            ExcelIntegration.RegisterMethods(methods,
                                             functionAttributes,
                                             argumentAttributes)
            pending <- List.Empty