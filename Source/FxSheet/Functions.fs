namespace FxSheet

open ExcelDna.Integration
open FxSheet.Compiler

module Functions =
    let private manager = new Definitions.DefinitionManager(
                              List.empty,
                              Map.empty,
                              new Definitions.DefinitionCompiler(
                                new ILCompiler()))

    [<ExcelFunction(Description = "Defines a new function.")>]
    let DEFINE (name : string, expression : string) = manager.EnqueueDefinition(name, expression)
    [<ExcelCommand>]
    let FxSheetRegister() = manager.RegisterDefinitions()