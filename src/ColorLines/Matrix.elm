module ColorLines.Matrix exposing (init, Matrix, set, get, foldl)

import Array

type Matrix a = Matrix
  { matrix: Array.Array a
  , x: Int
  , y: Int
  }

type alias Location = (Int, Int)


locationToPosition : Location -> Matrix a -> Int
locationToPosition (x, y) (Matrix matrix) =
  y * matrix.x + x


init : Int -> Int -> a -> Matrix a
init x y element =
  Matrix { matrix = Array.repeat (x * y) element
  , x = x
  , y = y
  }


set : Location -> a -> Matrix a -> Matrix a
set location element ((Matrix matrix) as m) =
  Matrix { matrix |
    matrix = Array.set (locationToPosition location m) element matrix.matrix
  }


get : Location -> Matrix a -> Maybe a
get location ((Matrix matrix) as m) =
  Array.get (locationToPosition location m) matrix.matrix


foldl : (a -> b -> b) -> b -> Matrix a -> b
foldl rec acc (Matrix matrix) =
  Array.foldl rec acc matrix.matrix
