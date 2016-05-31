module HP41 exposing (main)

{-| This is a user interface for showing an HP-41 calculator and interacting
with dbnut.

# Definition
@docs main

-}


import Html exposing (Html)
import Html.App as Html
import Keyboard
import Svg exposing (..)
import Svg.Attributes  exposing (..)
import Task
import Time
import WebSocket


{-| The main program entry point -}
main : Program Never
main = Html.program { init = init, update = update, view = view,
                      subscriptions = subscriptions }

-- | The size of the calculator image
calcWidth  = 298
calcHeight = 557


-- | How much we offset in LCD characters and annunciators
offsetLCD = "30"

-- background = image calcWidth calcHeight "image/hp41.png"

calc model =
  Svg.svg [ width (toString calcWidth)
          , height (toString calcHeight)]
     [ image [ width (toString calcWidth)
             , height (toString calcHeight)
             , xlinkHref "image/hp41.png"
             ]
             []
     , text' [ fontSize "23"
             , Svg.Attributes.style "font-family: 'HP41'"
             , x offsetLCD
             , y "48" ]
         [ text "hello world!"]
     , text' [ fontSize "11"
             , x offsetLCD
             , y "62"
             , Svg.Attributes.style "font-family: 'Andale Mono'"]
         [ text "BAT USER GRAD SHIFT O1234 PRGM ALPHA" ]
     ]


type alias Model = {
    lcd : String
  , annunciators : String
}


init : (Model, Cmd Msg)
init = (blankLCD, Cmd.none)


blankLCD : Model
blankLCD = { lcd = "            "
           , annunciators = "                                    " }


type Msg = UpdateLCD String String
         | KeyEvent Bool Int
         | SendMessage String


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    UpdateLCD text ann -> ({ model | lcd = text, annunciators = ann }, Cmd.none)
    KeyEvent pressed kc -> (model, Task.perform (\_ -> SendMessage "")
                                                (sendKey pressed kc) Time.now)
    SendMessage body -> (model, WebSocket.send webSocket body)


view : Model -> Html Msg
view model = calc model


subscriptions model =
  Sub.batch [ Keyboard.downs (KeyEvent True)
            , Keyboard.ups (KeyEvent False)
            , WebSocket.keepAlive webSocket]

-- 112 is F1
-- keyEvent pressed kc = KeyEvent (KeyData pressed timestamp kc)

sendKey pressed kc time =
  SendMessage (toString (round (Time.inMilliseconds time)))


webSocket = "ws://localhost:8080"
