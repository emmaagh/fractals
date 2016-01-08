module Colour where

import Color exposing (..)

type alias Colour = {
  r: Int,
  g: Int,
  b: Int
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

type alias ColourSpace = {
  range1: ColourRange,
  range2: ColourRange,
  range3: ColourRange
}

mapColoursToInterval : Colour -> Colour -> ColourRange
mapColoursToInterval colourA colourB =
  let getColourPart getPart =
        { start = getPart colourA, range = getPart colourB - getPart colourA }
  in
    { red   = getColourPart (\c -> toFloat c.r),
      green = getColourPart (\c -> toFloat c.g),
      blue  = getColourPart (\c -> toFloat c.b) }

colourSpace : ColourSpace
colourSpace =
  let c1 = { r = 29, g = 121, b = 178 }
      c2 = { r = 90, g = 98, b = 156 }
      c3 = { r = 255, g = 92, b = 92 }
      c4 = { r = 255, g = 255, b = 255 }
  in
    { range1 = mapColoursToInterval c1 c2,
      range2 = mapColoursToInterval c2 c3,
      range3 = mapColoursToInterval c3 c4 }

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

getColourInRanges : Float -> Color
getColourInRanges value =
  let (range, k) =
        if value < 0.5 then
          (colourSpace.range1, value * 2.0)
        else if value < 0.95 then
          (colourSpace.range2, value * 20.0 / 9.0 - 10.0 / 9.0)
        else
          (colourSpace.range3, value * 20.0 - 19.0)
  in
    getColour range k
