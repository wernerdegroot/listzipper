module List.ZipperTest where

import ElmTest exposing (..)
import Debug exposing (crash)
import List.Zipper exposing (..)
import Maybe exposing (andThen, map, withDefault)

-- Convenience functions:
flatMap : (a -> Maybe b) -> Maybe a -> Maybe b
flatMap = flip andThen

orCrash : Maybe a -> a
orCrash m =
  case m of
    Just value -> value
    Nothing -> crash ""

-- Test data:
someList = [1, 2, 3, 4]

focusOnThirdElement = someList 
  |> fromList 
  |> flatMap next 
  |> flatMap next
  
-- Tests:
creatingAZipperFromAnEmptyListShouldReturnNothing =
  let
    zipperFromEmptyList = fromList []
  in
    test "Creating a `Zipper` from an empty list should return `Nothing`" <| assertEqual Nothing zipperFromEmptyList

creatingAZipperFromAListShouldReturnAZipperFocussedOnTheFirstElement =
  let
    zipper = fromList someList |> orCrash
    valuesBefore = before zipper
    valueAtFocus = get zipper
    valuesAfter = after zipper
  in
    suite "Creating a `Zipper` from a list should return a `Zipper` focussed on the first element"
      [ test "Elements before focus" <| assertEqual [] valuesBefore
      , test "Element at focus" <| assertEqual 1 valueAtFocus
      , test "Elements after focus" <| assertEqual [2, 3, 4] valuesAfter 
      ]
      
movingToTheBeginningOfAListShouldReturnAZipperFocussedOnTheFirstElement =
  let
    zipper = focusOnThirdElement |> orCrash |> first
    valuesBefore = before zipper
    valueAtFocus = get zipper
    valuesAfter = after zipper
  in
    suite "Moving to the beginning of a list should return a `Zipper` focussed on the first element"
      [ test "Elements before focus" <| assertEqual [] valuesBefore
      , test "Element at focus" <| assertEqual 1 valueAtFocus
      , test "Elements after focus" <| assertEqual [2, 3, 4] valuesAfter 
      ]
      
movingAZipperFocussedOnTheThirdElementBackwardShouldReturnAZipperFocussedOnTheSecondElement =
  let
    zipper = focusOnThirdElement |> flatMap previous
    valuesBefore = zipper |> map before
    valueAtFocus = zipper |> map get
    valuesAfter = zipper |> map after
  in
    suite "Moving a `Zipper` focussed on the third element backward should return a `Zipper` focussed on the second element" 
      [ test "Elements before focus" <| assertEqual (Just [1]) valuesBefore
      , test "Element at focus" <| assertEqual (Just 2) valueAtFocus
      , test "Elements after focus" <| assertEqual (Just [3, 4]) valuesAfter
      ] 
    
movingAZipperFocussedOnTheThirdElementForwardShouldReturnAZipperFocussedOnTheFourthElement =
  let
    zipper = focusOnThirdElement |> flatMap next
    valuesBefore = zipper |> map before
    valueAtFocus = zipper |> map get
    valuesAfter = zipper |> map after
  in
    suite "Moving a `Zipper` focussed on the third element forward should return a `Zipper` focussed on the fourth element"
      [ test "Elements before focus" <| assertEqual (Just [1, 2, 3]) valuesBefore
      , test "Element at focus" <| assertEqual (Just 4) valueAtFocus
      , test "Elements after focus" <| assertEqual (Just []) valuesAfter
      ] 
      
movingToTheEndOfAListShouldReturnAZipperFocussedOnTheLastElement =
  let
    zipper = focusOnThirdElement |> map last
    valuesBefore = zipper |> map before
    valueAtFocus = zipper |> map get
    valuesAfter = zipper |> map after
  in
    suite "Moving to the end of a list should return a `Zipper` focussed on the last element"
      [ defaultTest <| assertEqual (Just [1, 2, 3]) valuesBefore
      , defaultTest <| assertEqual (Just 4) valueAtFocus
      , defaultTest <| assertEqual (Just []) valuesAfter 
      ]
      
updatingTheValuesBeforeTheFocusShouldWorkAsExpected =
  let
    updatedZipper = focusOnThirdElement |> orCrash |> updateBefore (always [8, 9])
    updatedList = toList updatedZipper
  in
    test "Updating the values before the focus should work as expected" (assertEqual [8, 9, 3, 4] updatedList)
    
updatingTheValueAtTheFocusShouldWorkAsExpected =
  let
    updatedZipper = focusOnThirdElement |> orCrash |> update (always 7)
    updatedList = toList updatedZipper
  in
    test "Updating the values before the focus should work as expected" (assertEqual [1, 2, 7, 4] updatedList)
    
updatingTheValuesAfterTheFocusShouldWorkAsExpected =
  let
    updatedZipper = focusOnThirdElement |> orCrash |> updateAfter (always [5, 6, 7])
    updatedList = toList updatedZipper
  in
    test "Updating the values before the focus should work as expected" (assertEqual [1, 2, 3, 5, 6, 7] updatedList)
    
searchingForTheNumberThreeShouldYieldAZipperFocussedOnTheFirstElementWithTheValueThree =
  let
    findThree = find (\x -> x == 3)
    startingPoint = fromList someList
    zipper = flatMap findThree startingPoint
    valuesBefore = zipper |> map before
    valueAtFocus = zipper |> map get
    valuesAfter = zipper |> map after
  in
    suite "Searching for the number 3 should yield a `Zipper` focussed on the first element with value 3"
      [ defaultTest <| assertEqual (Just [1, 2]) valuesBefore
      , defaultTest <| assertEqual (Just 3) valueAtFocus
      , defaultTest <| assertEqual (Just [4]) valuesAfter 
      ]
      
searchingForTheNumberOneShouldYieldNothingWhenTheNumberOneDoesNotOccurAfterTheStartingPoint =
  let
    findOne = find (\x -> x == 1)
    startingPoint = someList |> fromList |> flatMap next
    zipper = flatMap findOne startingPoint
  in
    test "Searching for the number 1 should yield `Nothing` when the number 1 does not occur after the starting point" <| assertEqual Nothing zipper

zipperTest = suite "List.Zipper" 
  [ creatingAZipperFromAnEmptyListShouldReturnNothing
  , creatingAZipperFromAListShouldReturnAZipperFocussedOnTheFirstElement 
  , movingToTheBeginningOfAListShouldReturnAZipperFocussedOnTheFirstElement
  , movingAZipperFocussedOnTheThirdElementBackwardShouldReturnAZipperFocussedOnTheSecondElement
  , movingAZipperFocussedOnTheThirdElementForwardShouldReturnAZipperFocussedOnTheFourthElement
  , movingToTheEndOfAListShouldReturnAZipperFocussedOnTheLastElement
  , updatingTheValuesBeforeTheFocusShouldWorkAsExpected
  , updatingTheValueAtTheFocusShouldWorkAsExpected
  , updatingTheValuesAfterTheFocusShouldWorkAsExpected
  , searchingForTheNumberThreeShouldYieldAZipperFocussedOnTheFirstElementWithTheValueThree
  , searchingForTheNumberOneShouldYieldNothingWhenTheNumberOneDoesNotOccurAfterTheStartingPoint
  ]