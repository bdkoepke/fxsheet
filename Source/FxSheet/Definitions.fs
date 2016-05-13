namespace FxSheet

open System.Reflection
open ExcelDna.Integration

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
              FunctionAttribute=new ExcelFunctionAttribute(attribute);
              ArgumentAttributes=d.ArgumentDescriptions
                              |> List.map(fun x -> x :> obj)
                              |> System.Linq.Enumerable.ToList } 

    type DefinitionManager(pending : List<Definition>,
                           registered : Map<string, Definition>,
                           compiler : DefinitionCompiler) = 
        let mutable pending = pending
        let mutable registered = registered

        member this.EnqueueDefinition(name : string, expression: string) =
            pending <- {Name=name;
                        Expression=expression;
                        Description=Option.None;
                        ArgumentDescriptions=List.Empty}
                        :: pending
            name

        member this.RegisterDefinitions() =
            let methods = new System.Collections.Generic.List<MethodInfo>()
            let functionAttributes = new System.Collections.Generic.List<obj>()
            let argumentAttributes =
                new System.Collections.Generic.List<
                    System.Collections.Generic.List<obj>>()

            let uniquePending  =
                pending
                |> List.filter(fun d -> not(registered.ContainsKey(d.Name)))
            uniquePending
                |> List.iter(fun d -> registered <- registered.Add(d.Name, d))
            let compiled = 
                uniquePending
                |> List.map(fun d -> compiler.Compile(d))

            compiled |> List.iter(fun c -> methods.Add(c.Method))
            compiled |>
                List.iter(fun c -> functionAttributes.Add(c.FunctionAttribute))
            compiled |>
                List.iter(fun c -> argumentAttributes.Add(c.ArgumentAttributes))

            ExcelIntegration.RegisterMethods(methods,
                                             functionAttributes,
                                             argumentAttributes)
            pending <- List.Empty