module Grid where

import Config exposing (..)

type alias Bounds = {
  xMin : Int,
  xMax : Int,
  yMin : Int,
  yMax : Int
}

bounds : Bounds
bounds =
  case mode of
    WholeView -> { xMin = -2 * unitSize |> round,
                   xMax = unitSize |> round,
                   yMin = -unitSize |> round,
                   yMax = unitSize |> round }
    Zoomed -> { xMin = 0.3 * unitSize |> round,
                xMax = 0.48 * unitSize |> round,
                yMin = 0.28 * unitSize |> round,
                yMax = 0.42 * unitSize |> round }

height = bounds.yMax - bounds.yMin
width = bounds.xMax - bounds.xMin

grid : List (Int, Int)
grid =
  [bounds.xMin .. bounds.xMax]
  |> List.map (\x -> [bounds.yMin .. bounds.yMax] |> List.map ((,) x))
  |> List.concat
