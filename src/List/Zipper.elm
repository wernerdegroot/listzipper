module List.Zipper exposing
    ( Zipper
    , singleton, fromList, fromCons, from, withDefault
    , before, current, after, toList
    , map, mapBefore, mapCurrent, mapAfter
    , first, previous, next, last, find, findFirst, findNext
    , isLast, isFirst
    )

{-| A zipper for `List`.


# The `Zipper` type

@docs Zipper


# Constructing a `Zipper`

@docs singleton, fromList, fromCons, from, withDefault


# Accessors

@docs before, current, after, toList


# Mapping

@docs map, mapBefore, mapCurrent, mapAfter


# Moving around

@docs first, previous, next, last, find, findFirst, findNext


# Predicates

@docs isLast, isFirst

-}

import List exposing (reverse)


{-| The `Zipper` type.
-}
type Zipper a
    = Zipper (List a) a (List a)


{-| Construct a `Zipper` focussed on the first element of a singleton list.
-}
singleton : a -> Zipper a
singleton x =
    Zipper [] x []


{-| Construct a `Zipper` from a list. The `Zipper` will focus on the first element (if there is a first element).
-}
fromList : List a -> Maybe (Zipper a)
fromList xs =
    case xs of
        [] ->
            Nothing

        y :: ys ->
            Just (Zipper [] y ys)


{-| Construct a `Zipper` from the first element and a list. The `Zipper` will focus on the first element.
-}
fromCons : a -> List a -> Zipper a
fromCons x xs =
    Zipper [] x xs


{-| Construct a `Zipper` from before, current, after.
-}
from : List a -> a -> List a -> Zipper a
from bef curr aft =
    Zipper (reverse bef) curr aft


{-| Provide an alternative when constructing a `Zipper` fails.
-}
withDefault : a -> Maybe (Zipper a) -> Zipper a
withDefault x =
    Maybe.withDefault (singleton x)


{-| Returns all elements before the element the `Zipper` is focussed on.
-}
before : Zipper a -> List a
before (Zipper ls _ _) =
    reverse ls


{-| Returns the element the `Zipper` is currently focussed on.
-}
current : Zipper a -> a
current (Zipper _ x _) =
    x


{-| Returns all elements after the element the `Zipper` is focussed on
-}
after : Zipper a -> List a
after (Zipper _ _ rs) =
    rs


{-| Reconstruct the list.
-}
toList : Zipper a -> List a
toList z =
    before z ++ [ current z ] ++ after z


{-| Apply a function to every element in the `Zipper`.
-}
map : (a -> b) -> Zipper a -> Zipper b
map f (Zipper ls x rs) =
    Zipper (List.map f ls) (f x) (List.map f rs)


{-| Apply a function to all elements before the element the `Zipper` is focussed on.
-}
mapBefore : (List a -> List a) -> Zipper a -> Zipper a
mapBefore f ((Zipper _ x rs) as zipper) =
    let
        elementsBefore =
            before zipper

        mappedElementsBefore =
            f elementsBefore
    in
    Zipper (reverse mappedElementsBefore) x rs


{-| Apply a function to the element the `Zipper` is focussed on.
-}
mapCurrent : (a -> a) -> Zipper a -> Zipper a
mapCurrent f (Zipper ls x rs) =
    Zipper ls (f x) rs


{-| Apply a function to all elements after the element the `Zipper` is focussed on.
-}
mapAfter : (List a -> List a) -> Zipper a -> Zipper a
mapAfter f (Zipper ls x rs) =
    Zipper ls x (f rs)


{-| Move the focus to the first element of the list.
-}
first : Zipper a -> Zipper a
first ((Zipper ls x rs) as zipper) =
    case reverse ls of
        [] ->
            zipper

        y :: ys ->
            Zipper [] y (ys ++ [ x ] ++ rs)


{-| Move the focus to the element before the element the `Zipper` is currently focussed on (if there is such an element).
-}
previous : Zipper a -> Maybe (Zipper a)
previous (Zipper ls x rs) =
    case ls of
        [] ->
            Nothing

        y :: ys ->
            Just <| Zipper ys y (x :: rs)


{-| Move the focus to the element after the element the `Zipper` is currently focussed on (if there is such an element).
-}
next : Zipper a -> Maybe (Zipper a)
next (Zipper ls x rs) =
    case rs of
        [] ->
            Nothing

        y :: ys ->
            Just <| Zipper (x :: ls) y ys


{-| Move the focus to the last element of the list.
-}
last : Zipper a -> Zipper a
last ((Zipper ls x rs) as zipper) =
    case reverse rs of
        [] ->
            zipper

        y :: ys ->
            Zipper (ys ++ [ x ] ++ ls) y []


{-| Returns a `Zipper` focussed on the first element for which the predicate returns `True` (starting from a given `Zipper`). Note that this function does not search the entire list, but starts searching from the current element.
-}
find : (a -> Bool) -> Zipper a -> Maybe (Zipper a)
find predicate ((Zipper ls x rs) as zipper) =
    if predicate x then
        Just zipper

    else
        case next zipper of
            Just nextZipper ->
                find predicate nextZipper

            Nothing ->
                Nothing


{-| Returns a `Zipper` focussed on the first element for which the predicate returns `True` (starting from the first element of a given `Zipper`).
-}
findFirst : (a -> Bool) -> Zipper a -> Maybe (Zipper a)
findFirst predicate =
    find predicate << first


{-| Returns a `Zipper` focussed on the first element for which the predicate returns `True` (starting from the next element of a given `Zipper` if there is a next element).
-}
findNext : (a -> Bool) -> Zipper a -> Maybe (Zipper a)
findNext predicate =
    Maybe.andThen (find predicate) << next


{-| Checks if the currently focused element is the first one.
-}
isFirst : Zipper a -> Bool
isFirst (Zipper ls _ _) =
    List.isEmpty ls


{-| Checks if the currently focused element is the last one.
-}
isLast : Zipper a -> Bool
isLast (Zipper _ _ rs) =
    List.isEmpty rs
