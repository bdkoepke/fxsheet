namespace FxSheet

open System.Reflection

[<NoComparison>]
type Definition = {
    Name : string;
    Expression : string;
    Description : Option<string>;
    ArgumentDescriptions : List<string> }

type ICompiler =
    abstract Compile : Definition -> MethodInfo