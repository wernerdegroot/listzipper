module Tests exposing (..)

import ElmTest exposing (..)
import List.Zipper.Tests exposing (zipperTests)

tests = suite "Tests" [ zipperTests ]

main : Program Never
main = 
    runSuiteHtml tests