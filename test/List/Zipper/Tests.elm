module List.Zipper.Tests exposing (..)

import ElmTest exposing (..)
import Debug exposing (crash)
import List.Zipper exposing (..)
import Maybe exposing (andThen, map, withDefault)

-- Convenience functions:
flatMap : (a -> Maybe b) -> Maybe a -> Maybe b
flatMap = flip andThen

assertJust : a -> Maybe a -> Assertion
assertJust expectedValue possibleValue = 
  case possibleValue of 
    Just value -> assertEqual expectedValue value
    Nothing -> fail "Encountered `Nothing` where a value was expected!" 

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
    zipper = fromList someList
    valuesBefore = map before zipper
    valueAtFocus = map get zipper
    valuesAfter = map after zipper
  in
    suite "Creating a `Zipper` from a list should return a `Zipper` focussed on the first element"
      [ test "Elements before focus" <| assertJust [] valuesBefore
      , test "Element at focus" <| assertJust 1 valueAtFocus
      , test "Elements after focus" <| assertJust [2, 3, 4] valuesAfter 
      ]
      
movingToTheBeginningOfAListShouldReturnAZipperFocussedOnTheFirstElement =
  let
    zipper = map first focusOnThirdElement
    valuesBefore = map before zipper
    valueAtFocus = map get zipper
    valuesAfter = map after zipper
  in
    suite "Moving to the beginning of a list should return a `Zipper` focussed on the first element"
      [ test "Elements before focus" <| assertJust [] valuesBefore
      , test "Element at focus" <| assertJust 1 valueAtFocus
      , test "Elements after focus" <| assertJust [2, 3, 4] valuesAfter 
      ]
      
movingAZipperFocussedOnTheThirdElementBackwardShouldReturnAZipperFocussedOnTheSecondElement =
  let
    zipper = flatMap previous focusOnThirdElement
    valuesBefore = map before zipper
    valueAtFocus = map get zipper
    valuesAfter = map after zipper
  in
    suite "Moving a `Zipper` focussed on the third element backward should return a `Zipper` focussed on the second element" 
      [ test "Elements before focus" <| assertJust [1] valuesBefore
      , test "Element at focus" <| assertJust 2 valueAtFocus
      , test "Elements after focus" <| assertJust [3, 4] valuesAfter
      ] 
    
movingAZipperFocussedOnTheThirdElementForwardShouldReturnAZipperFocussedOnTheFourthElement =
  let
    zipper = flatMap next focusOnThirdElement
    valuesBefore = map before zipper
    valueAtFocus = map get zipper
    valuesAfter = map after zipper
  in
    suite "Moving a `Zipper` focussed on the third element forward should return a `Zipper` focussed on the fourth element"
      [ test "Elements before focus" <| assertJust [1, 2, 3] valuesBefore
      , test "Element at focus" <| assertJust 4 valueAtFocus
      , test "Elements after focus" <| assertJust [] valuesAfter
      ] 
      
movingToTheEndOfAListShouldReturnAZipperFocussedOnTheLastElement =
  let
    zipper = map last focusOnThirdElement
    valuesBefore = map before zipper
    valueAtFocus = map get zipper
    valuesAfter = map after zipper
  in
    suite "Moving to the end of a list should return a `Zipper` focussed on the last element"
      [ defaultTest <| assertJust [1, 2, 3] valuesBefore
      , defaultTest <| assertJust 4 valueAtFocus
      , defaultTest <| assertJust [] valuesAfter 
      ]
      
updatingTheValuesBeforeTheFocusShouldWorkAsExpected =
  let
    transformation = always [8, 9]
    updatedZipper = map (updateBefore transformation) focusOnThirdElement
    updatedList = map toList updatedZipper
  in
    test "Updating the values before the focus should work as expected" (assertJust [8, 9, 3, 4] updatedList)
    
updatingTheValueAtTheFocusShouldWorkAsExpected =
  let
    transformation = always 7
    updatedZipper = map (update transformation) focusOnThirdElement
    updatedList = map toList updatedZipper
  in
    test "Updating the values before the focus should work as expected" (assertJust [1, 2, 7, 4] updatedList)
    
updatingTheValuesAfterTheFocusShouldWorkAsExpected =
  let
    transformation = always [5, 6, 7]
    updatedZipper = map (updateAfter transformation) focusOnThirdElement
    updatedList = map toList updatedZipper
  in
    test "Updating the values before the focus should work as expected" (assertJust [1, 2, 3, 5, 6, 7] updatedList)
    
searchingForTheNumberThreeShouldYieldAZipperFocussedOnTheFirstElementWithTheValueThree =
  let
    findThree = find (\x -> x == 3)
    startingPoint = fromList someList
    zipper = flatMap findThree startingPoint
    valuesBefore = map before zipper
    valueAtFocus = map get zipper
    valuesAfter = map after zipper
  in
    suite "Searching for the number 3 should yield a `Zipper` focussed on the first element with value 3"
      [ defaultTest <| assertJust [1, 2] valuesBefore
      , defaultTest <| assertJust 3 valueAtFocus
      , defaultTest <| assertJust [4] valuesAfter 
      ]
      
searchingForTheNumberOneShouldYieldNothingWhenTheNumberOneDoesNotOccurAfterTheStartingPoint =
  let
    findOne = find (\x -> x == 1)
    startingPoint = fromList someList |> flatMap next
    zipper = flatMap findOne startingPoint
  in
    test "Searching for the number 1 should yield `Nothing` when the number 1 does not occur after the starting point" <| assertEqual Nothing zipper

zipperTests = suite "List.Zipper" 
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