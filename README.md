# ListZipper

A zipper for `List`.

See module `List.Zipper` for more information about the functions it exposes.

## Examples

### Moving around

We can use a `Zipper` to move around in an array, changing values as we go.

```elm
import List.Zipper exposing (..)
import Maybe exposing (andThen)

-- Just for convenience:
flatMap = flip andThen

someList = [1, 2, 3, 4]

negative x = -x

-- Just [1, 2, -3, 4]
updatedList = someList
  |> fromList
  |> flatMap next
  |> flatMap next
  |> map (update negative)
  |> toList
```

### Finding values

We can use a `Zipper` to find values, changing them as we see fit. This can be used to implement a `replace`.

```elm
import List.Zipper exposing (..)
import Maybe exposing (andThen)

-- Just for convenience:
flatMap = flip andThen

someList = [1, 2, 3, 4]

even x = x % 2 == 0
negative x = -x

-- Just [1, -2, 3, 4]
updatedList = someList
  |> fromList
  |> flatMap (find even)
  |> map (update negative)
  |> toList

```