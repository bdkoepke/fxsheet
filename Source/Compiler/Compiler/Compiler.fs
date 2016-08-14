namespace FxSheet

open System.Reflection

module Compiler =
    [<NoComparison>]
    type Definition = {
        Name : string;
        Expression : string;
        Description : Option<string>;
        ArgumentDescriptions : List<string> }

    type ICompiler =
        abstract Compile : Definition -> MethodInfo