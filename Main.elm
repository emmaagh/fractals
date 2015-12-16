import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Debug exposing (..)

{- for each c, render c black if z_n+1 = z_n^2 + c, z_0 = 0 is bounded
                        white otherwise -}

{- todo: try buddhabrot method -}

type alias Time = Int

type alias Count = Int

type alias EscapeTimeHistogram = {
  histogram : List (Time, Count),
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

type alias Range = {
  start: Float,
  range: Float
}

type alias ColourRange = {
  red: Range,
  green: Range,
  blue: Range
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
    WholeView -> 250
    Zoomed -> 4000

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

maxIterations = 500

calculateNextColourInterval previousInterval redRange greenRange blueRange =
  let red = previousInterval.red
      green = previousInterval.green
      blue = previousInterval.blue
  in
    { red    = { start = red.start + red.range,     range = redRange },
      green  = { start = green.start + green.range, range = greenRange },
      blue   = { start = blue.start + blue.range,   range = blueRange } }

colourInterval1 =
  { red   = { start = 15.0, range = -8.0 },
    green = { start = 178.0,  range = 26.0 },
    blue  = { start = 80.0,  range = 5.0 } }

colourInterval2 =
  calculateNextColourInterval colourInterval1 151.0 -170.0 255.0

colourInterval3 =
  calculateNextColourInterval colourInterval2 -55.0 -28.0 -77.0

colourInterval4 =
  calculateNextColourInterval colourInterval3 152.0 175.0 118.0

{-colourInterval1 =
  { red   = { start = 35.0, range = 0.0 },
    green = { start = 80.0,  range = 0.0 },
    blue  = { start = 170.0,  range = 0.0 } }

colourInterval2 =
  calculateNextColourInterval colourInterval1 70.0 -45.0 0.0

colourInterval3 =
  calculateNextColourInterval colourInterval2 105.0 -10.0 -100.0-}

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
        |> List.filter (\t -> t <= n)
        |> List.length
      histogram =
        [1..maxIterations]
        |> List.map (\n -> (n, getCount n))
      total = List.length escapeTimes
  in { histogram = histogram, total = total }

colourEscapeTime : Maybe Int -> EscapeTimeHistogram -> Color
colourEscapeTime escapeTime escapeTimeHistogram =
  case escapeTime of
    Nothing     -> black
    Just escape -> let histogramValue =
                         escapeTimeHistogram.histogram
                         |> List.filter (\(t, c) -> t == escape)
                         |> List.map snd
                         |> List.sum {- Easiest way to get the one element from the list -}
                   in
                     toFloat histogramValue / toFloat escapeTimeHistogram.total
                     |> getColourInThirds

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

getColourInThirds : Float -> Color
getColourInThirds value =
  if (value <= 0.4) then
    getColour colourInterval1 (value * 2.5)
  else if (value <= 0.8) then
    getColour colourInterval2 (value * 2.5 - 1.0)
  else if (value <= 0.9) then
    getColour colourInterval3 (value * 10.0 - 8.0)
  else
    getColour colourInterval4 (value * 10.0 - 9.0)

getColour : ColourRange -> Float -> Color
getColour colourIntervals value =
  let getColour' colourRange =
        colourRange.start + colourRange.range * value
        |> round
      red = getColour' colourIntervals.red
      green = getColour' colourIntervals.green
      blue = getColour' colourIntervals.blue
  in
    rgb red green blue

fst (x,y) = x
snd (x,y) = y
