namespace FxSheet

module ParserTest =
    open Xunit
    open FsUnit.Xunit
    open FParsec
    open FxSheet.Parser
    open FxSheet.Ast

    [<Fact>]
    let ``evaluate =10.5+123.25+877 should equal 1010.75``() =
        qparse "10.5 + 123.25 + 877" 
        |> should equal (Arithmetic(Arithmetic(Type(Num(10.5)),Add, Type(Num(123.25))), Add, Type(Num(877.0))))

    [<Fact>]
    let ``evaluate 10/2 + 123.125 + 877 should equal 1005.125``() =
        calculate "10/2 + 123.125 + 877" |> parse
        |> should equal (Arithmetic(Arithmetic(Arithmetic(Type(Num(10.0)), Div, Type(Num(2.0))), Add, Type(Num(123.125))), Add, Type(Num(877.0))))

    [<Fact>]
    let ``evaluate (123 + log 1 + 877) * 9/3 should equal 3000``() =
        calculate "(123 + log(1) + 877) * 9/3" |> parse
        |> should equal (Arithmetic(Arithmetic(Arithmetic(Arithmetic(Type(Num(123.0)), Add, Function("log", [Type(Num(1.0))])), Add, Type(Num(877.0))), Mul, Type(Num(9.0))), Div, Type(Num(3.0))))

    (* [<Fact>]
    let ``evaluate ( ( exp 0 + (6 / ( 1 +2 ) )- 123456 )/ 2+123 + 877) * 3^2 / 3 should equal -182179.5``() =
        let r = calculate "( ( exp(0) + (6 / ( 1 +2 ) )- 123456 )/ 2+123 + 877) * 3^2 / 3" |> parse
        r |> should equal (DivExpr(MulExpr(AddExpr(AddExpr(DivExpr(SubExpr(AddExpr(UdfExpr("exp", [TypeExpr(NumType(0.0))]), DivExpr(TypeExpr(NumType(6.0)), AddExpr(TypeExpr(NumType(1.0)), TypeExpr(NumType(2.0))))), TypeExpr(NumType(123456.0))), TypeExpr(NumType(2.0))), TypeExpr(NumType(123.0))), TypeExpr(NumType(877.0))), PowExpr(TypeExpr(NumType(3.0)), TypeExpr(NumType(2.0)))), TypeExpr(NumType(3.0)))) *)