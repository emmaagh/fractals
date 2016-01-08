import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Debug exposing (..)

import Colour exposing (getColourInRanges)
import Config exposing (unitSize, maxIterations)
import Grid exposing (..)

type alias Time = Int

type alias Count = Int

type alias EscapeEvent = {
  time : Int,
  smooth : Float
}

type alias EscapeTimeHistogram = {
  histogram : List (Time, Count),
  total : Int
}

main =
  grid
  |> renderGrid
  |> collage Grid.width Grid.height

renderGrid : List (Int, Int) -> List Form
renderGrid coordinates =
  let escapeTimes =
        coordinates
        |> List.map (\c ->
              let escape = c |> scaled |> getEscapeEvent
              in (c, escape))
      escapeTimeHistogram =
        escapeTimes
        |> List.filterMap snd
        |> List.map (\e -> e.time)
        |> getEscapeTimeHistogram
  in
    escapeTimes
    |> List.map (\(coords, escape) ->
          colourEscapeEvent escape escapeTimeHistogram
          |> renderPixel coords)

scaled : (Int, Int) -> (Float, Float)
scaled (px, py) =
  let x = (toFloat px) / unitSize
      y = (toFloat py) / unitSize
  in (x, y)

{- Would be good to use a better smoothing function -}
smooth : Float -> Float -> Int -> Float
smooth xn yn n =
  let logE = logBase e
      v =
        (logE (xn*xn + yn*yn)) / (2 * logE 2)
        |> logE
  in 1.0 - v

getEscapeEvent : (Float, Float) -> Maybe EscapeEvent
getEscapeEvent (x0, y0) =
  let escapeTime' xn yn n =
        if (n > maxIterations) then
          Nothing
        else if (xn*xn + yn*yn > 4) then
          Just { time = n, smooth = smooth xn yn n }
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

colourEscapeEvent : Maybe EscapeEvent -> EscapeTimeHistogram -> Color
colourEscapeEvent escape histogram =
  case escape of
    Nothing     -> black
    Just escape -> let getHistogramValue time =
                         histogram.histogram
                         |> List.filter (\(t, c) -> t == time)
                         |> List.map snd
                         |> List.sum {- Easiest way to get the one element from the list (as List.head returns Maybe) -}
                         |> toFloat
                       d =
                         (1.0 - escape.smooth) * getHistogramValue escape.time
                         + escape.smooth * getHistogramValue (escape.time + 1)
                   in d / toFloat histogram.total
                      |> getColourInRanges

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
