port module Main exposing (..)

import FuzzTests
import Tests
import Test
import Test.Runner.Node exposing (run, TestProgram)
import Json.Encode exposing (Value)


main : TestProgram
main =
    run emit (Test.describe "FuzzTests and Tests" [ FuzzTests.all, Tests.all ])


port emit : ( String, Value ) -> Cmd msg
