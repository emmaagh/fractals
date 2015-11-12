import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Debug exposing (..)

{- for each c, render c black if z_n+1 = z_n^2 + c, z_0 = 0 is bounded
                        white otherwise -}

{- todo: try buddhabrot method -}

type alias EscapeTimeHistogram = {
  histogram : List (Int, Int),
  total : Int
}

type Mode =
  WholeView
  | Zoomed

type alias Bounds = {
  xMin : Int,
  xMax : Int,
  yMin : Int,
  yMax : Int
}

main =
  grid
  |> renderGrid
  |> collage width height

mode : Mode
mode = Zoomed

unitSize : Float
unitSize =
  case mode of
    WholeView -> 200
    Zoomed -> 3000

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

renderGrid : List (Int, Int) -> List Form
renderGrid coordinates =
  let escapeTimes =
        coordinates
        |> List.map (\c ->
              let escape = c |> scaled |> getEscapeTime
              in (c, escape))
      escapeTimeHistogram =
        escapeTimes
        |> List.filterMap snd
        |> getEscapeTimeHistogram
  in
    escapeTimes
    |> List.map (\(coords, time) ->
          colourEscapeTime time escapeTimeHistogram
          |> renderPixel coords)

maxIterations = 350

scaled : (Int, Int) -> (Float, Float)
scaled (px, py) =
  let x = (toFloat px) / unitSize
      y = (toFloat py) / unitSize
  in (x, y)

getEscapeTime : (Float, Float) -> Maybe Int
getEscapeTime (x0, y0) =
  let escapeTime' xn yn n =
        if (n > maxIterations) then
          Nothing
        else if (xn*xn + yn*yn > 4) then
          Just n
        else
          escapeTime' (xn*xn - yn*yn + x0) (2*xn*yn + y0) (n+1)
  in escapeTime' x0 y0 0

getEscapeTimeHistogram : List Int -> EscapeTimeHistogram
getEscapeTimeHistogram escapeTimes =
  let getCount n =
        escapeTimes
        |> List.filter (\t -> t == n)
        |> List.length
      histogram =
        [0..maxIterations]
        |> List.map (\n -> (n, getCount n))
      total =
        histogram
        |> List.map snd
        |> List.sum
  in { histogram = histogram, total = total }

colourEscapeTime : Maybe Int -> EscapeTimeHistogram -> Color
colourEscapeTime escapeTime escapeTimeHistogram =
  case escapeTime of
    Nothing     -> black
    Just escape -> let hueTotal =
                         escapeTimeHistogram.histogram
                         |> List.filter (\(t, c) -> t <= escape)
                         |> List.map snd
                         |> List.sum
                       hue = toFloat hueTotal / toFloat escapeTimeHistogram.total
                   in
                     let degs = hue * 5
                     in hsl degs 1 0.5

renderPixel coords colour =
  let (x, y) = coords
      midX = toFloat (bounds.xMax + bounds.xMin) / 2
      midY = toFloat (bounds.yMax + bounds.yMin) / 2
      moveX = toFloat x - midX
      moveY = toFloat y - midY
  in
    square (toFloat 1)
    |> filled colour
    |> move (moveX, moveY)

fst (x,y) = x
snd (x,y) = y
