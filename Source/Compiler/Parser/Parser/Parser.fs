namespace FxSheet

module Parser =
    open FParsec
    open Ast

    // AST definition
    ////////////////////////

    (*type Expr = 
        | Pow of Expr*Expr
        | Mul of Expr*Expr
        | Div of Expr*Expr
        | Add of Expr*Expr
        | Sub of Expr*Expr
        | Unary of Expr
        | Num of float
        | Int of int
        | Func of string*Expr list *)

    // the parser definition
    ////////////////////////

    let ws = spaces // skips any whitespace

    let ch c = skipChar c >>. ws
    let str s = skipString s >>. ws

    let number: Parser<xlexpr, unit> = pfloat .>> ws |>> (fun x -> Type(Num(x)))

    let inner_term_p, pRef = createParserForwardedToRef()

    let idStr = many1Satisfy isLower

    // we set up an operator precedence parser for parsing the arithmetic expressions
    let opp = new OperatorPrecedenceParser<_,_,_>()

    let expr = opp.ExpressionParser
    let arg_list = sepBy expr (ch ',')
    let args = between (ch '(') (ch ')')  arg_list

    // function call
    let fcall_p = pipe2 idStr args (fun fname fargs -> UdfExpr(fname, fargs))

    opp.TermParser <- choice [number; fcall_p; between (ch '(') (ch ')') expr]

    // operator definitions follow the schema
    // operator type, string, trailing whitespace parser, precedence, associativity, function to apply

    opp.AddOperator(InfixOperator("+", ws, 1, Associativity.Left, fun x y -> Arithmetic(x, Add, y)))
    opp.AddOperator(InfixOperator("-", ws, 1, Associativity.Left, fun x y -> Arithmetic(x, Sub, y)))

    opp.AddOperator(InfixOperator("*", ws, 2, Associativity.Left, fun x y -> Arithmetic(x, Mul, y)))
    opp.AddOperator(InfixOperator("/", ws, 2, Associativity.Left, fun x y -> Arithmetic(x, Div, y)))

    //opp.AddOperator(InfixOperator("^", ws, 3, Associativity.Right, fun x y -> PowExpr(x, y)))

    opp.AddOperator(PrefixOperator("-", ws, 4, true, fun x -> Unary(Neg, x)))

    pRef := expr

    let completeExpression = ws >>. expr .>> eof // we append the eof parser to make
                                                // sure all input is consumed

    let calculate exp = run completeExpression exp

    let parse x =
        match x with
        | Success (x', _, _) -> x'
        | Failure (msg, _, _) -> failwith msg

    let qparse exp = calculate exp |> parse

    (* opp.AddOperator(InfixOperator("==", spaces, 1, Associativity.Left, (fun x y -> EqExpr(x, y))))
    opp.AddOperator(InfixOperator("<>", spaces, 1, Associativity.Left, (fun x y -> NeqExpr(x, y))))
    opp.AddOperator(InfixOperator("<", spaces, 1, Associativity.Left, (fun x y -> LExpr(x, y))))
    opp.AddOperator(InfixOperator("<=", spaces, 1, Associativity.Left, (fun x y -> LeqExpr(x, y))))
    opp.AddOperator(InfixOperator(">", spaces, 1, Associativity.Left, (fun x y -> GExpr(x, y))))
    opp.AddOperator(InfixOperator(">=", spaces, 1, Associativity.Left, (fun x y -> GeqExpr(x, y))))
    opp.AddOperator(InfixOperator("&", spaces, 2, Associativity.Left, (fun x y -> AndExpr(x, y))))
    opp.AddOperator(InfixOperator("+", spaces, 3, Associativity.Left, (fun addend augend -> AddExpr(addend, augend))))
    opp.AddOperator(InfixOperator("-", spaces, 3, Associativity.Left, (fun minuend subtrahend -> SubExpr(minuend, subtrahend))))
    opp.AddOperator(InfixOperator("*", spaces, 4, Associativity.Left, (fun multiplier multiplicand -> MulExpr(multiplier, multiplicand))))
    opp.AddOperator(InfixOperator("/", spaces, 4, Associativity.Left, (fun dividend divisor -> DivExpr(dividend, divisor))))
    opp.AddOperator(InfixOperator("^", spaces, 5, Associativity.Right, (fun base' exponent -> PowExpr(base', exponent))))
    opp.AddOperator(PostfixOperator("%", spaces, 6, true, (fun x -> PerExpr(x))))
    opp.AddOperator(PrefixOperator("-", spaces, 7, true, (fun x -> MinExpr(x)))) *)