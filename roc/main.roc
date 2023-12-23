app "calc24"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
        rand: "https://github.com/lukewilliamboswell/roc-random/releases/download/0.1.0/OoD8jmqBLc0gyuaadckDMx1jedEa03EdGSR_V4KhH7g.tar.br",
    }
    imports [pf.Stdout, pf.Utc, pf.Task, Calc24.{ Operand, buildFormula, calc24 }, rand.Random]
    provides [main] to pf

main =
    total = 1000
    size = 4

    indexes = List.range { start: At 0, end: Length size }
    formula = indexes |> List.map (\i -> Index i) |> buildFormula

    ts <- Task.await (Utc.now |> Task.map Utc.toMillisSinceEpoch |> Task.map Num.toU32)
    initialSeed = Random.seed ts
    rndgen = Random.i32 1 13

    playGame = \{ count, currentSeed } ->
        if count == total then
            Task.ok (Done {})
        else
            { seed: nextSeed, xs: nums } =
                indexes
                |> List.walk { seed: currentSeed, xs: [] } \{ seed, xs }, _ ->
                    { value, state } = rndgen seed
                    { seed: state, xs: List.append xs value }
            problem = nums |> List.map Num.toStr |> Str.joinWith ","
            result =
                when calc24 nums formula is
                    Ok expr -> "\(problem) -> \(expr)"
                    _ -> "\(problem) -> no solution"
            _ <- Stdout.line result |> Task.await
            Task.ok (Step { count: count + 1, currentSeed: nextSeed })

    Task.loop { count: 0, currentSeed: initialSeed } playGame
