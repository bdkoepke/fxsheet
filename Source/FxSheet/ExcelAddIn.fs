namespace FxSheet

open ExcelDna.ComInterop
open ExcelDna.Integration
open Microsoft.Office.Interop

type ExcelAddIn() =
    let Application = ExcelDnaUtil.Application :?> Excel.Application

    let FxSheetRegister =
        new Excel.AppEvents_AfterCalculateEventHandler(fun () -> Application.Run("FxSheetRegister") |> ignore)

    interface IExcelAddIn with
        member this.AutoClose() =
            ComServer.DllUnregisterServer() |> ignore
            Application.remove_AfterCalculate (FxSheetRegister)

        member this.AutoOpen() =
            ComServer.DllRegisterServer() |> ignore
            Application.add_AfterCalculate (FxSheetRegister)
