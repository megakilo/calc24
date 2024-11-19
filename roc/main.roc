app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.16.0/O00IPk-Krg_diNS2dVWlI0ZQP794Vctxzv0ha96mK0E.tar.br",
    rand: "https://github.com/lukewilliamboswell/roc-random/releases/download/0.4.0/Ai2KfHOqOYXZmwdHX3g3ytbOUjTmZQmy0G2R9NuPBP0.tar.br",
}

import pf.Stdout
import pf.Utc
import Calc24 exposing [buildFormula, calc24]
import rand.Random

main =
    total = 100000
    size = 4

    indexes = List.range { start: At 0, end: Length size }
    formula = indexes |> List.map (\i -> Index i) |> buildFormula

    ts = Utc.now! {} |> Utc.toMillisSinceEpoch |> Num.toU32
    initialSeed = Random.seed ts
    rndgen = Random.boundedI32 1 13

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
                    Ok expr -> "$(problem) -> $(expr)"
                    _ -> "$(problem) -> no solution"
            Stdout.line! result
            Task.ok (Step { count: count + 1, currentSeed: nextSeed })

    Task.loop { count: 0, currentSeed: initialSeed } playGame
