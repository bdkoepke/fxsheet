namespace FxSheet

module ParserTest =
    open Xunit
    open FsUnit.Xunit
    open FxSheet.Parser
    open FxSheet.Ast

    [<Fact>]
    let ``evaluate =10.5+123.25+877 should equal 1010.75``() =
        qparse "10.5 + 123.25 + 877" 
        |> should equal (Binary(Binary(Type(Const(Num(10.5))), Add, Type(Const(Num(123.25)))), Add, Type(Const(Num(877)))))

    [<Fact>]
    let ``evaluate 10/2 + 123.125 + 877 should equal 1005.125``() =
        calculate "10/2 + 123.125 + 877" |> parse
        |> should equal (Binary(Binary(Binary(Type(Const(Num(10))), Div, Type(Const(Num(2)))), Add, Type(Const(Num(123.125)))), Add, Type(Const(Num(877)))))

    [<Fact>]
    let ``evaluate (123 + log 1 + 877) * 9/3 should equal 3000``() =
        calculate "(123 + log(1) + 877) * 9/3" |> parse
        |> should equal (Binary(Binary(Binary(Binary(Type(Const(Num(123))), Add, Function("log", [Type(Const(Num(1)))])), Add, Type(Const(Num(877)))), Mul, Type(Const(Num(9)))), Div, Type(Const(Num(3)))))