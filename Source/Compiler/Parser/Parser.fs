namespace FxSheet

module Parser =
    open FParsec
    open FParsec.CharParsers
    open Ast

    (* TODO: need to support intersection operator... *)
    let ws = spaces

    let ch c = skipChar c >>. ws

    let lettersToInt (xs: char list) = xs |> List.map System.Char.ToUpper |>
                                       List.fold (fun acc x -> acc * 26 + (int(x) - int('A'))) 0

    let xlerror: Parser<xlexpr, unit> =
        let errorStrToError = function
            | "NULL!" -> NULL
            | "DIV/0!" -> DIV
            | "VALUE!" -> VALUE
            | "NAME?" -> NAME
            | "NUM!" -> NUM
            | "N/A" -> NA
            | "REF" -> REF
            | _ -> failwith "Unknown error type."
        (ch '#') >>. regex "(N(\/A|AME\?|(ULL|UM)!)|(DIV\/0|VALUE|REF)!)" |>> errorStrToError |>> Err |>> Const |>> Type

    (* TODO: Confirm integer type... *)
    let xlint: Parser<xlexpr, unit> = pint64 .>> ws |>> Int |>> Const |>> Type

    let xlnumber: Parser<xlexpr, unit> = pfloat .>> ws |>> Num |>> Const |>> Type

    let xlbool: Parser<xlexpr, unit> =
        let boolStrToBool = function
            | "TRUE" -> true
            | "FALSE" -> false
            | _ -> failwith "Unknown bool type."
        regex "TRUE|FALSE" |>> boolStrToBool |>> Bool |>> Const |>> Type

    (* Fix this... not great at parsing references yet.... *)
    let xlref: Parser<xlexpr, unit> = optional (ch '$') >>. (many1 letter) .>> optional (ch '$') .>>. pint32 |>>
                                      (fun (x, y) -> (lettersToInt x, y) |> Cell |> Ref |> Type)

    let xltext: Parser<xlexpr, unit> = between (ch '"') (ch '"') (many1 letter) |>>
                                       System.String.Concat |>> Text |>> Const |>> Type

    let inner_term_p, pRef = createParserForwardedToRef()

    let idStr = many1Satisfy isLower

    // we set up an operator precedence parser for parsing the arithmetic expressions
    let opp = new OperatorPrecedenceParser<_,_,_>()

    let expr = opp.ExpressionParser
    let arg_list = sepBy expr (ch ',')
    let args = between (ch '(') (ch ')') arg_list

    // function call
    let fcall_p = pipe2 idStr args (fun fname fargs -> Function(fname, fargs))

    opp.TermParser <- choice [fcall_p; xlint; xlnumber; xlbool; xlerror; xlref; xltext; between (ch '(') (ch ')') expr]

    opp.AddOperator(InfixOperator("==", spaces, 1, Associativity.Left, fun x y -> Binary(x, Eq, y)))
    opp.AddOperator(InfixOperator("<>", spaces, 1, Associativity.Left, fun x y -> Binary(x, Neq, y)))
    opp.AddOperator(InfixOperator("<", spaces, 1, Associativity.Left, fun x y -> Binary(x, Lt, y)))
    opp.AddOperator(InfixOperator("<=", spaces, 1, Associativity.Left, fun x y -> Binary(x, Leq, y)))
    opp.AddOperator(InfixOperator(">", spaces, 1, Associativity.Left, fun x y -> Binary(x, Gt, y)))
    opp.AddOperator(InfixOperator(">=", spaces, 1, Associativity.Left, fun x y -> Binary(x, Geq, y)))

    opp.AddOperator(InfixOperator("&", spaces, 2, Associativity.Left, fun x y -> Binary(x, And, y)))

    opp.AddOperator(InfixOperator("+", ws, 3, Associativity.Left, fun x y -> Binary(x, Add, y)))
    opp.AddOperator(InfixOperator("-", ws, 3, Associativity.Left, fun x y -> Binary(x, Sub, y)))

    opp.AddOperator(InfixOperator("*", ws, 4, Associativity.Left, fun x y -> Binary(x, Mul, y)))
    opp.AddOperator(InfixOperator("/", ws, 4, Associativity.Left, fun x y -> Binary(x, Div, y)))

    opp.AddOperator(InfixOperator("^", ws, 5, Associativity.Left, fun x y -> Binary(x, Pow, y)))

    opp.AddOperator(PostfixOperator("%", ws, 6, true, fun x -> Unary(Mod, x)))

    opp.AddOperator(PrefixOperator("-", ws, 7, true, fun x -> Unary(Neg, x)))
    opp.AddOperator(PrefixOperator("+", ws, 7, true, fun x -> Unary(Add', x)))

    (* TODO: Add , '_', : operator? *)

    pRef := expr

    let completeExpression = (optional (ch '=')) >>. ws >>. expr .>> eof

    let calculate = run completeExpression

    let parse = function
        | Success (x', _, _) -> x'
        | Failure (msg, _, _) -> failwith msg

    let qparse exp = calculate exp |> parse