module Tests where

import ElmTest exposing (..)
import Graphics.Element exposing (Element, show)
import List.Zipper.Tests exposing (zipperTests)

tests = suite "Tests" [ zipperTests ]

main : Element
main = 
    elementRunner tests