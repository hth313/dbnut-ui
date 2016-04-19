import Color exposing (black)
import Graphics.Element exposing (..)
import Text exposing (defaultStyle)

main = calc

calcWidth  = 298
calcHeight = 557

background = image calcWidth calcHeight "image/hp41.png"

calc = flow outward [background, lcdContainer, ann]

lcdContainer = container calcWidth 86 middle
               <| width calcWidth
               <| height lcdHeight
               <| centered
               <| Text.style lcdStyle
               <| Text.fromString "HELLO WORLD2"

ann = container calcWidth 65 midBottom
      <| width calcWidth
      <| height annHeight
      <| centered
      <| Text.style annStyle
      <| Text.fromString "BAT USER GRAD SHIFT O1234 PRGM ALPHA"

lcdStyle : Text.Style
lcdStyle = { defaultStyle | typeface = ["HP41"], height = Just lcdHeight }

lcdHeight = 23

annStyle : Text.Style
annStyle = { defaultStyle | typeface = ["Andale Mono"], height = Just annHeight }

annHeight = 11
