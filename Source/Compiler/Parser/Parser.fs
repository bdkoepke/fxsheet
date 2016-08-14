namespace FxSheet

module Parser =
    open FParsec
    open FParsec.CharParsers
    open Ast

    let ws = spaces // skips any whitespace

    let ch c = skipChar c >>. ws

    let letters (x: char list) = x |> Array.ofList |> System.String.Concat
    let lettersToInt (xs: char list) = xs |> List.map System.Char.ToUpper |> List.fold (fun acc x -> acc * 26 + (int(x) - int('A'))) 0

    let error: Parser<xlexpr, unit> = (ch '#') >>. (many1 letter) .>> (ch '!') |>> (fun x -> Type(Str(letters x)))
    let ref: Parser<xlexpr, unit> = optional (ch '$') >>. (many1 letter) .>> optional (ch '$') .>>. pint32
                                    |>> (fun (x, y) -> Type(Ref((lettersToInt x, y))))
    let number: Parser<xlexpr, unit> = pfloat .>> ws |>> (fun x -> Type(Num(x)))

    let inner_term_p, pRef = createParserForwardedToRef()

    let idStr = many1Satisfy isLower

    // we set up an operator precedence parser for parsing the arithmetic expressions
    let opp = new OperatorPrecedenceParser<_,_,_>()

    let expr = opp.ExpressionParser
    let arg_list = sepBy expr (ch ',')
    let args = between (ch '(') (ch ')')  arg_list

    // function call
    let fcall_p =
        pipe2 idStr args (fun fname fargs -> Function(fname, fargs))

    opp.TermParser <- choice [fcall_p; number; ref; between (ch '(') (ch ')') expr]

    opp.AddOperator(InfixOperator("==", spaces, 1, Associativity.Left, fun x y -> Comparison(x, Eq, y)))
    opp.AddOperator(InfixOperator("<>", spaces, 1, Associativity.Left, fun x y -> Comparison(x, Neq, y)))
    opp.AddOperator(InfixOperator("<", spaces, 1, Associativity.Left, fun x y -> Comparison(x, Lt, y)))
    opp.AddOperator(InfixOperator("<=", spaces, 1, Associativity.Left, fun x y -> Comparison(x, Leq, y)))
    opp.AddOperator(InfixOperator(">", spaces, 1, Associativity.Left, fun x y -> Comparison(x, Gt, y)))
    opp.AddOperator(InfixOperator(">=", spaces, 1, Associativity.Left, fun x y -> Comparison(x, Geq, y)))

    opp.AddOperator(InfixOperator("&", spaces, 2, Associativity.Left, fun x y -> Logical(x, And, y)))

    opp.AddOperator(InfixOperator("+", ws, 3, Associativity.Left, fun x y -> Arithmetic(x, Add, y)))
    opp.AddOperator(InfixOperator("-", ws, 3, Associativity.Left, fun x y -> Arithmetic(x, Sub, y)))

    opp.AddOperator(InfixOperator("*", ws, 4, Associativity.Left, fun x y -> Arithmetic(x, Mul, y)))
    opp.AddOperator(InfixOperator("/", ws, 4, Associativity.Left, fun x y -> Arithmetic(x, Div, y)))

    opp.AddOperator(InfixOperator("^", ws, 5, Associativity.Left, fun x y -> Arithmetic(x, Pow, y)))

    opp.AddOperator(PrefixOperator("-", ws, 6, true, fun x -> Unary(Neg, x)))

    opp.AddOperator(PostfixOperator("%", ws, 6, true, fun x -> Unary(Mod, x)))

    pRef := expr

    let completeExpression = (optional (ch '=')) >>. ws >>. expr .>> eof // we append the eof parser to make
                                                // sure all input is consumed

    let calculate = run completeExpression

    let parse = function
        | Success (x', _, _) -> x'
        | Failure (msg, _, _) -> failwith msg

    let qparse exp = calculate exp |> parse