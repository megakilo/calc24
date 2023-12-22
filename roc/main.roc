app "calc24"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
        rand: "https://github.com/lukewilliamboswell/roc-random/releases/download/0.1.0/OoD8jmqBLc0gyuaadckDMx1jedEa03EdGSR_V4KhH7g.tar.br",
    }
    imports [pf.Stdout, pf.Utc, pf.Task, Calc24.{ Operand, buildFormula, calc24 }, rand.Random]
    provides [main] to pf

randomList = \initialSeed, generator, size, count ->
    List.range { start: At 0, end: Length (size * count) }
    |> List.walk { seed: initialSeed, numbers: [] } \state, _ ->
        random = generator state.seed
        numbers = List.append state.numbers random.value
        { seed: random.state, numbers }
    |> .numbers
    |> List.chunksOf size

main =
    count = 1000
    size = 4

    formula = List.range { start: At 0, end: Length size } |> List.map (\i -> Index i) |> buildFormula

    ts <- Task.await (Utc.now |> Task.map Utc.toMillisSinceEpoch |> Task.map Num.toU32)
    initialSeed = Random.seed ts
    generator = Random.i32 1 13

    numsList = randomList initialSeed generator size count

    solve =
        prevTask, nums <- List.walk numsList (Task.ok {})
        problem = nums |> List.map Num.toStr |> Str.joinWith ","
        result =
            when calc24 nums formula is
                Ok expr -> "\(problem) -> \(expr)"
                _ -> "\(problem) -> no solution"
        _ <- prevTask |> Task.await
        Stdout.line result

    _ <- solve |> Task.await
    Task.ok {}
