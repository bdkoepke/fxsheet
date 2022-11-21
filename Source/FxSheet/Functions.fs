namespace FxSheet

open ExcelDna.Integration
open FxSheet.Compiler

module Functions =
    let private manager = Definitions.DefinitionManager(
                              List.empty,
                              Map.empty,
                              Definitions.DefinitionCompiler(
                                ILCompiler()))

    [<ExcelFunction(Description = "Defines a new function.")>]
    let DEFINE (name : string, expression : string) = manager.EnqueueDefinition(name, expression)
    [<ExcelCommand>]
    let FxSheetRegister() = manager.RegisterDefinitions()