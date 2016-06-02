module HP41 exposing (main)

{-| This is a user interface for showing an HP-41 calculator and interacting
with dbnut.

# Definition
@docs main

-}


import Dict
import Html exposing (Html)
import Html.App as Html
import Keyboard
import Json.Encode as Encode
import Json.Decode as Decode
import Json.Decode exposing ((:=), decodeString)
import Maybe exposing (withDefault)
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
             , xmlSpace "preserve"
             , y "48" ]
         [ text (model.lcd) ]
     , text' [ fontSize "11"
             , x offsetLCD
             , y "62"
             , xmlSpace "preserve"
             , Svg.Attributes.style "font-family: 'Andale Mono'"]
         [ text (model.annunciators) ]
     ]


type alias Model = {
    lcd : String
  , annunciators : String
}


init : (Model, Cmd Msg)
init = ({ lcd = "hello world!", annunciators = "BAT USER GRAD SHIFT O1234 PRGM ALPHA" },
        Cmd.none)


blankLCD : Model
blankLCD = { lcd = "            "
           , annunciators = "                                    " }


type Msg = UpdateLCD String String
         | KeyEvent Bool Int
         | SendMessage String
         | Null


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    UpdateLCD text ann -> ({ model | lcd = text, annunciators = ann }, Cmd.none)
    KeyEvent pressed kc -> (model, Task.perform (\_ -> SendMessage "")
                                                (sendKey pressed kc) Time.now)
    SendMessage body -> (model, WebSocket.send webSocket body)
    Null -> (model, Cmd.none)


view : Model -> Html Msg
view model = calc model


subscriptions model =
  Sub.batch [ Keyboard.downs (key (KeyEvent True))
            , Keyboard.ups   (key (KeyEvent False))
            , WebSocket.listen webSocket receiveMessage]

-- 112 is F1
-- keyEvent pressed kc = KeyEvent (KeyData pressed timestamp kc)


-- Put together an outgoing key event.
sendKey pressed kc time =
  let intTime = round (Time.inMilliseconds time)
      method = if pressed then "key_press" else "key_release"
  in sendNotification method (Encode.object [ ("code", Encode.int kc)
                                            , ("timestamp", Encode.int intTime)])


-- Prepare an outgoing JSON-RPC notification message.
sendNotification method params =
  let call = Encode.object [ ("jsonrpc", Encode.string "2.0")
                           , ("method", Encode.string method)
                           , ("params", params)]
  in SendMessage (Encode.encode 0 call)


receiveMessage s =
  case decodeString (Decode.object2 (,)
                       ("method" := Decode.string)
                       ("params" := Decode.dict Decode.string)) s of
    Ok ("lcd-update", params) ->
       let text = withDefault "" (Dict.get "lcd" params)
           ann  = withDefault "" (Dict.get "ann" params)
       in UpdateLCD text ann
    -- An unimplemented message, we just ignore it
    Ok (method, _) -> Null
    -- Errors are silently dropped here.
    Err err -> Null



webSocket = "ws://localhost:8080"


-- Convert a key press to a key code event
key ctor k = case Dict.get k keyCodes of
                Just kc -> ctor kc
                Nothing -> Null

-- The mapping of keyboard keys to keycodes
keyCodes =
  Dict.fromList [ (112, keyON)    -- F1
                , (113, keyUSER)  -- F2
                , (114, keyPRGM)  -- F3
                , (115, keyALPHA) -- F4
                , (116, keyRUN)   -- F5
                , ( 65, keySIGMA) -- A
                , ( 66, keyINV)   -- B
                , ( 67, keySQRT)  -- C
                , ( 68, keyLOG)   -- D
                , ( 69, keyLN)    -- E
                , ( 70, keySWAP)  -- F
                , ( 71, keyRDN)   -- G
                , ( 72, keySIN)   -- H
                , ( 73, keyCOS)   -- I
                , ( 74, keyTAN)   -- J
                , ( 16, keySHIFT) -- shift
                , ( 74, keyXEQ)   -- K
                , ( 75, keySTO)   -- L
                , ( 76, keyRCL)   -- M
                , (  9, keySST)   -- TAB
                , ( 77, keyENTER) -- N
                , ( 13, keyENTER) -- enter
                , ( 79, keyCHS)   -- O
                , ( 80, keyEEX)   -- P
                , (  8, keyARROW) -- backspace
                , ( 81, keyMINUS) -- Q
                , ( 82, key7)     -- R
                , ( 83, key8)     -- S
                , ( 84, key9)     -- T
                , ( 85, keyPLUS)  -- U
                , ( 86, key4)     -- V
                , ( 87, key5)     -- W
                , ( 88, key6)     -- X
                , ( 89, keyMUL)   -- Y
                , ( 90, key1)     -- Z
                , ( 32, key0)     -- space
                , ( 48, key0)     -- 0
                , ( 49, key1)     -- 1
                , ( 50, key2)     -- 2
                , ( 51, key3)     -- 3
                , ( 52, key4)     -- 4
                , ( 53, key5)     -- 5
                , ( 54, key6)     -- 6
                , ( 55, key7)     -- 7
                , ( 56, key8)     -- 8
                , ( 57, key9)     -- 9
                , (191, keyDIV)   -- /
                , (187, key2)     -- =
                , (188, keyDOT)   -- .
                , (190, keyDOT)   -- ,
                ]



{-
    112 -> keyOn   -- F1
    113 -> keyUSER -- F2
    114 -> keyPRGM -- F3
    115 -> keyALPHA -- F4
    116 -> keyRUN   -- F5
    65 -> keySIGMA  -- A
    66 -> keyINV    -- B
    67 -> keySQRT   -- C
    68  -> keyLOG   -- D
    69  -> keyLN   -- E
    70  -> keySWAP  -- F
    71 -> keyRDN -- G
    72 -> keySIN   -- H
    73 -> keyCOS -- I
    74 -> keyTAN  -- J
    16 -> keySHIFT -- shift
    74 -> keyXEQ -- K
    75 -> keySTO -- L
    76 -> keyRCL -- M
    keySST
    77 -> keyENTER
    13 -> keyENTER
    79 -> keyCHS
    80 -> keyEEX
    8 -> keyARROW
    81 -> keyMINUS
    82 -> key7
    83 -> key8
    84 -> key9
    85 -> keyPLUS
    86 -> key4
    87 -> key5
    88 -> key6
    89 -> keyMUL
    90 -> key1
    32 -> key0

    48 -> key0   -- 0
    49 -> key1   -- 1
    50 -> key2   -- 2
    51 -> key3   -- 3
    52 -> key4   -- 4
    53 -> key5
    54 -> key6
    55 -> key7
    56 -> key8
    57 -> key9


    191 -> keyDIV  -- /
    187 -> key2    -- =
    188 -> keyDOT  -- .
    190 -> keyDOT  -- ,
-}

-- Key codes.
keyON    = 0x18
keyUSER  = 0xc6
keyPRGM  = 0xc5
keyALPHA = 0xc4
keySIGMA = 0x10
keyINV   = 0x30
keySQRT  = 0x70
keyLOG   = 0x80
keyLN    = 0xc0
keySWAP  = 0x11
keyRDN   = 0x31
keySIN   = 0x71
keyCOS   = 0x80
keyTAN   = 0xc1
keySHIFT = 0x12
keyXEQ   = 0x32
keySTO   = 0x72
keyRCL   = 0x82
keySST   = 0xc2
keyENTER = 0x13
keyCHS   = 0x73
keyEEX   = 0x83
keyARROW = 0xc3
keyMINUS = 0x14
key7     = 0x34
key8     = 0x74
key9     = 0x84
keyPLUS  = 0x15
key4     = 0x35
key5     = 0x75
key6     = 0x85
keyMUL   = 0x16
key1     = 0x36
key2     = 0x76
key3     = 0x86
keyDIV   = 0x17
key0     = 0x37
keyDOT   = 0x77
keyRUN   = 0x87
