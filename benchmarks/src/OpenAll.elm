module OpenAll exposing (main)

import Benchmark exposing (Benchmark)
import Benchmark.Runner exposing (BenchmarkProgram)
import List.Zipper exposing (Zipper)


main : BenchmarkProgram
main =
    Benchmark.Runner.program suite


suite : Benchmark
suite =
    let
        input =
            List.range 1 100
    in
    Benchmark.compare "List.Zipper.openAll"
        "List.indexedMap"
        (\_ -> implListIndexedMap input)
        "recursive"
        (\_ -> implRecursive input)



-- IMPLEMENTATIONS


implListIndexedMap : List a -> List (Zipper a)
implListIndexedMap input =
    List.indexedMap
        (\i x ->
            List.Zipper.from
                (List.take i input)
                x
                (List.drop (i + 1) input)
        )
        input


implRecursive : List a -> List (Zipper a)
implRecursive input =
    let
        go : Zipper a -> List (Zipper a) -> List (Zipper a)
        go accZipper accOutput =
            case List.Zipper.next accZipper of
                Nothing ->
                    List.reverse (accZipper :: accOutput)

                Just nextZipper ->
                    go nextZipper (accZipper :: accOutput)
    in
    case input of
        [] ->
            []

        x :: xs ->
            go (List.Zipper.fromCons x xs) []
