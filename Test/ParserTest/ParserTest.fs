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
        qparse "10/2 + 123.125 + 877"
        |> should equal (Binary(Binary(Binary(Type(Const(Num(10))), Div, Type(Const(Num(2)))), Add, Type(Const(Num(123.125)))), Add, Type(Const(Num(877)))))

    [<Fact>]
    let ``evaluate (123 + log 1 + 877) * 9/3 should equal 3000``() =
        qparse "(123 + log(1) + 877) * 9/3"
        |> should equal (Binary(Binary(Binary(Binary(Type(Const(Num(123))), Add, Function("log", [Type(Const(Num(1)))])), Add, Type(Const(Num(877)))), Mul, Type(Const(Num(9)))), Div, Type(Const(Num(3)))))
    
    [<Fact>]
    let ``should be able to parse IF(OR(A1=0,A1=1),1,fib(A1-1)+fib(A1-2))``() =
        qparse "if(A1=0,0,if(A1=1,1,fib(A1-1)+fib(A1-2)))"
        |> should equal (Function("if", [Binary (Type (Ref (Cell (0, 1))), Eq, Type (Const (Num 0.0))); Type (Const (Num 1.0)); Function("if", [Binary (Type (Ref (Cell (0, 1))), Eq, Type (Const (Num 1.0)));Type (Const (Num 1.0));Binary(Function("fib",[Binary (Type (Ref (Cell (0, 1))), Sub, Type (Const (Num 1.0)))]), Add, Function("fib",[Binary (Type (Ref (Cell (0, 1))), Sub, Type (Const (Num 2.0)))]))])]))