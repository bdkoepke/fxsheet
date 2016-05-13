namespace FxSheet

open ExcelDna.Integration

module Functions =
    let private manager = new Definitions.DefinitionManager(
                              List.Empty,
                              Map.empty,
                              new Definitions.DefinitionCompiler(
                                new ILCompiler()))

    [<ExcelFunction(Description = "Defines a new function.")>]
    let DEFINE (name : string, expression : string) = manager.EnqueueDefinition(name, expression)
    [<ExcelCommand>]
    let FxSheetRegister() = manager.RegisterDefinitions()