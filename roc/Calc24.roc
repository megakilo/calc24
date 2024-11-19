module [Operand, buildFormula, calc24]

Operator : [Add, Subtract, Multiply, Divide]

Operand : [
    Index U64,
    Expression
        {
            operator : Operator,
            left : Operand,
            right : Operand,
        },
]

eval : Operand, List I32 -> F64
eval = \operand, nums ->
    when operand is
        Index i -> List.get nums i |> Result.map Num.toF64 |> Result.withDefault 0
        Expression { operator, left, right } ->
            when operator is
                Add -> (eval left nums) + (eval right nums)
                Subtract -> (eval left nums) - (eval right nums)
                Multiply -> (eval left nums) * (eval right nums)
                Divide -> (eval left nums) / (eval right nums)

toStr : Operand, List I32 -> Str
toStr = \operand, nums ->
    when operand is
        Index i -> List.get nums i |> Result.withDefault 0 |> Num.toStr
        Expression { operator, left, right } ->
            when operator is
                Add -> "$(toStr left nums) + $(toStr right nums)"
                Subtract -> "$(toStr left nums) - $(toWrappedStr right nums Bool.false)"
                Multiply -> "$(toWrappedStr left nums Bool.false) * $(toWrappedStr right nums Bool.false)"
                Divide -> "$(toWrappedStr left nums Bool.false) / $(toWrappedStr right nums Bool.true)"

toWrappedStr = \operand, nums, isDenominator ->
    when operand is
        Index i -> List.get nums i |> Result.withDefault 0 |> Num.toStr
        Expression { operator } ->
            result = toStr operand nums
            when operator is
                Add | Subtract -> "($(result))"
                _ -> if isDenominator then "($(result))" else result

combine = \op1, op2 -> [
    Expression { left: op1, right: op2, operator: Add },
    Expression { left: op1, right: op2, operator: Multiply },
    Expression { left: op1, right: op2, operator: Subtract },
    Expression { left: op2, right: op1, operator: Subtract },
    Expression { left: op1, right: op2, operator: Divide },
    Expression { left: op2, right: op1, operator: Divide },
]

buildFormula : List Operand -> List Operand
buildFormula = \indexes ->
    when indexes is
        [_] -> indexes
        _ ->
            reduced =
                split indexes 2
                |> List.map \{ taken, nontaken } ->
                    when taken is
                        [a, b] ->
                            combine a b
                            |> List.map \r ->
                                nontaken |> List.append r |> buildFormula

                        _ -> crash "unexpected input"
            reduced |> List.join |> List.join

split : List a, U64 -> List { taken : List a, nontaken : List a }
split = \xs, n ->
    when xs is
        [] -> []
        [head, .. as tail] ->
            if n == 0 then
                [{ taken: [], nontaken: xs }]
            else if List.len xs <= n then
                [{ taken: xs, nontaken: [] }]
            else
                List.concat
                    (split tail n |> List.map (\{ taken, nontaken } -> { taken, nontaken: List.append nontaken head }))
                    (split tail (n - 1) |> List.map (\{ taken, nontaken } -> { taken: List.append taken head, nontaken }))

calc24 = \nums, formula ->
    # Debug: expressions |> List.map (\e -> toStr e nums) |> Str.joinWith "\n"
    formula |> List.findFirst (\f -> eval f nums |> Num.compare 24 == EQ) |> Result.map (\f -> toStr f nums)

# Tests
testCalc24 = \nums ->
    size = List.len nums
    formula = List.range { start: At 0, end: Length size } |> List.map (\i -> Index i) |> buildFormula
    calc24 nums formula

expect Result.isOk (testCalc24 [2, 3, 4])
expect Result.isErr (testCalc24 [1, 3, 4])
expect Result.isOk (testCalc24 [1, 2, 3, 4])
expect Result.isOk (testCalc24 [3, 3, 7, 7])
expect Result.isErr (testCalc24 [1, 1, 1, 4])
