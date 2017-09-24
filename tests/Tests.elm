module Tests exposing (..)

import Test exposing (..)
import List.Zipper exposing (..)
import Expect
import Maybe exposing (andThen)
import Lazy.List as LazyList

assertJust : a -> Maybe a -> Expect.Expectation
assertJust expectedValue possibleValue = 
  case possibleValue of 
    Just value -> Expect.equal expectedValue value
    Nothing -> Expect.fail "Encountered `Nothing` where a value was expected!" 

-- Test data:
someList = [1, 2, 3, 4]

focusOnThirdElement = someList
  |> fromList 
  |> andThen next 
  |> andThen next
  
-- Tests:
creatingASingletonShouldResultInAZipperFocussedOnTheOnlyElement =
  let
    zipper = singleton 1
    valuesBefore = before zipper
    valueAtFocus = current zipper
    valuesAfter = after zipper
  in
    describe "Creating a singleton should result in a `Zipper` with only one element"
      [ test "Elements before focus" <| \() -> Expect.equal [] valuesBefore
      , test "Element at focus" <| \() -> Expect.equal 1 valueAtFocus
      , test "Elements after focus" <| \() -> Expect.equal [] valuesAfter
      ]

creatingAZipperFromAnEmptyListShouldReturnNothing =
  let
    zipperFromEmptyList = fromList []
  in
    test "Creating a `Zipper` from an empty list should return `Nothing`" <| \() -> Expect.equal Nothing zipperFromEmptyList

creatingAZipperFromAListShouldReturnAZipperFocussedOnTheFirstElement =
  let
    zipper = fromList someList
    valuesBefore = Maybe.map before zipper
    valueAtFocus = Maybe.map current zipper
    valuesAfter = Maybe.map after zipper
  in
    describe "Creating a `Zipper` from a list should return a `Zipper` focussed on the first element"
      [ test "Elements before focus" <| \() -> assertJust [] valuesBefore
      , test "Element at focus" <| \() -> assertJust 1 valueAtFocus
      , test "Elements after focus" <| \() -> assertJust [2, 3, 4] valuesAfter 
      ]

providingAnAlternativeToAZipperConstructedFromAnEmptyListShouldYieldASingletonWithTheProvidedAlternative =
  let
    zipper = fromList []
    zipperOrAlternative = withDefault 1 zipper
    valuesBefore = before zipperOrAlternative
    valueAtFocus = current zipperOrAlternative
    valuesAfter = after zipperOrAlternative
  in
    describe "Providing an alternative to a `Zipper` constructed from an empty list should yield a singleton with the provided alternative"
      [ test "Elements before focus" <| \() -> Expect.equal [] valuesBefore
      , test "Element at focus" <| \() -> Expect.equal 1 valueAtFocus
      , test "Elements after focus" <| \() -> Expect.equal [] valuesAfter
      ]

providingAnAlternativeToAZipperConstructedFromAValidListShouldYieldAZipperFocussedOnTheFirstElementOfTheList =
  let
    zipper = fromList someList
    zipperOrAlternative = withDefault 1 zipper
    valuesBefore = before zipperOrAlternative
    valueAtFocus = current zipperOrAlternative
    valuesAfter = after zipperOrAlternative
  in
    describe "Providing an alternative to a `Zipper` constructed from a valid list should yield a `Zipper` focussed on the first element of the list"
      [ test "Elements before focus" <| \() -> Expect.equal [] valuesBefore
      , test "Element at focus" <| \() -> Expect.equal 1 valueAtFocus
      , test "Elements after focus" <| \() -> Expect.equal [2, 3, 4] valuesAfter 
      ]
      
movingToTheBeginningOfAListShouldReturnAZipperFocussedOnTheFirstElement =
  let
    zipper = Maybe.map first focusOnThirdElement
    valuesBefore = Maybe.map before zipper
    valueAtFocus = Maybe.map current zipper
    valuesAfter = Maybe.map after zipper
  in
    describe "Moving to the beginning of a list should return a `Zipper` focussed on the first element"
      [ test "Elements before focus" <| \() -> assertJust [] valuesBefore
      , test "Element at focus" <| \() -> assertJust 1 valueAtFocus
      , test "Elements after focus" <| \() -> assertJust [2, 3, 4] valuesAfter 
      ]
      
movingAZipperFocussedOnTheThirdElementBackwardShouldReturnAZipperFocussedOnTheSecondElement =
  let
    zipper = andThen previous focusOnThirdElement
    valuesBefore = Maybe.map before zipper
    valueAtFocus = Maybe.map current zipper
    valuesAfter = Maybe.map after zipper
  in
    describe "Moving a `Zipper` focussed on the third element backward should return a `Zipper` focussed on the second element" 
      [ test "Elements before focus" <| \() -> assertJust [1] valuesBefore
      , test "Element at focus" <| \() -> assertJust 2 valueAtFocus
      , test "Elements after focus" <| \() -> assertJust [3, 4] valuesAfter
      ] 
    
movingAZipperFocussedOnTheThirdElementForwardShouldReturnAZipperFocussedOnTheFourthElement =
  let
    zipper = andThen next focusOnThirdElement
    valuesBefore = Maybe.map before zipper
    valueAtFocus = Maybe.map current zipper
    valuesAfter = Maybe.map after zipper
  in
    describe "Moving a `Zipper` focussed on the third element forward should return a `Zipper` focussed on the fourth element"
      [ test "Elements before focus" <| \() -> assertJust [1, 2, 3] valuesBefore
      , test "Element at focus" <| \() -> assertJust 4 valueAtFocus
      , test "Elements after focus" <| \() -> assertJust [] valuesAfter
      ] 
      
movingToTheEndOfAListShouldReturnAZipperFocussedOnTheLastElement =
  let
    zipper = Maybe.map last focusOnThirdElement
    valuesBefore = Maybe.map before zipper
    valueAtFocus = Maybe.map current zipper
    valuesAfter = Maybe.map after zipper
  in
    describe "Moving to the end of a list should return a `Zipper` focussed on the last element"
      [ test "before" <| \() -> assertJust [1, 2, 3] valuesBefore
      , test "at" <| \() -> assertJust 4 valueAtFocus
      , test "after" <| \() -> assertJust [] valuesAfter 
      ]
      
updatingTheValuesBeforeTheFocusShouldWorkAsExpected =
  let
    transformation = always [8, 9]
    updatedZipper = Maybe.map (mapBefore transformation) focusOnThirdElement
    updatedList = Maybe.map toList updatedZipper
  in
    test "Updating the values before the focus should work as expected" <| \() -> (assertJust [8, 9, 3, 4] updatedList)
    
updatingTheValueAtTheFocusShouldWorkAsExpected =
  let
    transformation = always 7
    updatedZipper = Maybe.map (mapCurrent transformation) focusOnThirdElement
    updatedList = Maybe.map toList updatedZipper
  in
    test "Updating the values at the focus should work as expected" <| \() -> (assertJust [1, 2, 7, 4] updatedList)
    
updatingTheValuesAfterTheFocusShouldWorkAsExpected =
  let
    transformation = always [5, 6, 7]
    updatedZipper = Maybe.map (mapAfter transformation) focusOnThirdElement
    updatedList = Maybe.map toList updatedZipper
  in
    test "Updating the values after the focus should work as expected" <| \() -> (assertJust [1, 2, 3, 5, 6, 7] updatedList)
    
searchingForTheNumberThreeShouldYieldAZipperFocussedOnTheFirstElementWithTheValueThree =
  let
    findThree = find (\x -> x == 3)
    startingPoint = fromList someList
    zipper = andThen findThree startingPoint
    valuesBefore = Maybe.map before zipper
    valueAtFocus = Maybe.map current zipper
    valuesAfter = Maybe.map after zipper
  in
    describe "Searching for the number 3 should yield a `Zipper` focussed on the first element with value 3"
      [ test "before" <| \() -> assertJust [1, 2] valuesBefore
      , test "at" <| \() -> assertJust 3 valueAtFocus
      , test "after" <| \() -> assertJust [4] valuesAfter 
      ]
      
searchingForTheNumberOneShouldYieldNothingWhenTheNumberOneDoesNotOccurAfterTheStartingPoint =
  let
    findOne = find (\x -> x == 1)
    startingPoint = fromList someList |> andThen next
    zipper = andThen findOne startingPoint
  in
    test "Searching for the number 1 should yield `Nothing` when the number 1 does not occur after the starting point" <| \() -> Expect.equal Nothing zipper

searchingForThePositiveNumbersInAListWithBothPositiveAndNegativeNumbersShouldYieldAllPositiveNumbers =
  let
    isJust m = case m of
      Just _ -> True
      Nothing -> False
    listToSearchIn = [1, -4, -2, 3, -8, 6, 3]
    predicate = \x -> x > 0
    generator = Maybe.andThen (findNext predicate)
    positives = fromList listToSearchIn
      |> Maybe.andThen (find predicate) -- Finds the first positive
      |> LazyList.iterate generator -- Finds all other positives
      |> LazyList.map (Maybe.map current) -- Maps `Zipper` to its current item
      |> LazyList.takeWhile isJust -- Takes until we've passed the last positive
      |> LazyList.toList -- Transforms to a `List`
  in
    test "Searching for the positive numbers in a list with both positive and negative numbers should yield all positive numbers" 
      <| \() -> Expect.equal [Just 1, Just 3, Just 6, Just 3] positives
     

all = describe "List.Zipper" 
  [ creatingASingletonShouldResultInAZipperFocussedOnTheOnlyElement
  , creatingAZipperFromAnEmptyListShouldReturnNothing
  , creatingAZipperFromAListShouldReturnAZipperFocussedOnTheFirstElement
  , providingAnAlternativeToAZipperConstructedFromAnEmptyListShouldYieldASingletonWithTheProvidedAlternative
  , providingAnAlternativeToAZipperConstructedFromAValidListShouldYieldAZipperFocussedOnTheFirstElementOfTheList 
  , movingToTheBeginningOfAListShouldReturnAZipperFocussedOnTheFirstElement
  , movingAZipperFocussedOnTheThirdElementBackwardShouldReturnAZipperFocussedOnTheSecondElement
  , movingAZipperFocussedOnTheThirdElementForwardShouldReturnAZipperFocussedOnTheFourthElement
  , movingToTheEndOfAListShouldReturnAZipperFocussedOnTheLastElement
  , updatingTheValuesBeforeTheFocusShouldWorkAsExpected
  , updatingTheValueAtTheFocusShouldWorkAsExpected
  , updatingTheValuesAfterTheFocusShouldWorkAsExpected
  , searchingForTheNumberThreeShouldYieldAZipperFocussedOnTheFirstElementWithTheValueThree
  , searchingForTheNumberOneShouldYieldNothingWhenTheNumberOneDoesNotOccurAfterTheStartingPoint
  , searchingForThePositiveNumbersInAListWithBothPositiveAndNegativeNumbersShouldYieldAllPositiveNumbers
  ]