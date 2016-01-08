module Config where

type Mode =
  WholeView
  | Zoomed

mode : Mode
mode = Zoomed

unitSize : Float
unitSize =
  case mode of
    WholeView -> 250
    Zoomed    -> 4800

maxIterations = 600
