module Tests exposing (..)

import Expect
import Fuzz exposing (..)
import List.Zipper exposing (..)
import Test exposing (..)


all : Test
all =
    describe "List.Zipper"
        [ constructing
        , accessors
        , updating
        ]


constructing : Test
constructing =
    describe "Constructing a Zipper"
        [ describe "#singleton"
            [ fuzz int "should result in a zipper focussed on the only element" <|
                \i ->
                    Expect.equal (Zipper [] i []) <| singleton i
            ]
        , describe "#fromLIst"
            [ fuzz (list int) "should maybe return a zipper" <|
                \l ->
                    case l of
                        [] ->
                            Expect.equal Nothing (fromList l)

                        head :: tail ->
                            fromList l
                                |> Expect.equal (Just (Zipper [] head tail))
            ]
        , describe "#withDefault"
            [ test "should provide an alternative when constructing a Zipper fails" <|
                \() ->
                    fromList []
                        |> withDefault 42
                        |> Expect.equal (Zipper [] 42 [])
            ]
        ]


accessors : Test
accessors =
    describe "Accessors"
        [ describe "#before"
            [ fuzzZipper "should return the elements before the focussed element" <|
                \z b f a ->
                    Expect.equal (List.reverse b) <| before z
            ]
        , describe "#current"
            [ fuzzZipper "should return the focussed element" <|
                \z b f a ->
                    Expect.equal f <| current z
            ]
        , describe "#after"
            [ fuzzZipper "should return the elements after the focussed element" <|
                \z b f a ->
                    Expect.equal a <| after z
            ]
        , describe "#toList"
            [ fuzzZipper "should return a list of all elements" <|
                \z b f a ->
                    Expect.equal ((List.reverse b) ++ [ f ] ++ a) <| toList z
            ]
        ]


updating : Test
updating =
    let
        negtive =
            (*) -1

        negtiveAll =
            List.map negtive
    in
        describe "updating"
            [ describe "#updateBefore"
                [ fuzzZipper "should update all elements before the focussed element" <|
                    \z b f a ->
                        updateBefore negtiveAll z
                            |> Expect.equal (Zipper (negtiveAll b) f a)
                ]
            , describe "#update"
                [ fuzzZipper "should update the focussed element" <|
                    \z b f a ->
                        update negtive z
                            |> Expect.equal (Zipper b (negtive f) a)
                ]
            , describe "#updateAfter"
                [ fuzzZipper "should update all elements after the focussed element" <|
                    \z b f a ->
                        updateAfter negtiveAll z
                            |> Expect.equal (Zipper b f (negtiveAll a))
                ]
            ]


fuzzZipper : String -> (Zipper Int -> List Int -> Int -> List Int -> Expect.Expectation) -> Test
fuzzZipper title expectation =
    fuzz3 (list int) int (list int) title <|
        \b f a -> expectation (Zipper b f a) b f a
