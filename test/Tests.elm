module Tests where

import ElmTest exposing (..)
import Graphics.Element exposing (Element, show)
import List.ZipperTest exposing (zipperTest)

tests = suite "Tests" [ zipperTest ]

main : Element
main = 
    elementRunner tests