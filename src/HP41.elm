module HP41 exposing (main)

{-| This is a user interface for showing an HP-41 calculator and interacting
with dbnut.


# Definition

@docs main

-}

import Dict
import Html exposing (Html)
import Html.Events
import Keyboard
import Json.Encode as Encode
import Json.Decode as Decode
import Json.Decode exposing (decodeString)
import Maybe exposing (withDefault)
import Set exposing (Set)
import Set as Set
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onMouseDown, onMouseUp)
import Task
import Time
import WebSocket


{-| The main program entry point
-}
main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- | The size of the calculator image


calcWidth =
    298


calcHeight =
    557



-- | How much we offset in LCD characters and annunciators


offsetLCD =
    "30"



-- background = image calcWidth calcHeight "image/hp41.png"


calc model =
    let
        keyHeight =
            31

        normalKey =
            keyImage "33" keyHeight

        wideKey =
            keyImage "80" keyHeight

        keyImage w h xpos ypos name code =
            let
                ( imageName, h_, ypos_ ) =
                    if Set.member code (model.pressed) then
                        ( name ++ "_pressed", h + 2, ypos - 2 )
                    else
                        ( name, h, ypos )
            in
                image
                    [ width w
                    , height (toString h_)
                    , x xpos
                    , y (toString ypos_)
                    , xlinkHref ("image-ladybug/key_" ++ imageName ++ ".png")
                    , onMouseDown (KeyEvent True code)
                    , onMouseUp (KeyEvent False code)
                    ]
                    []

        rockerKey xpos imageName code dir buddyCode =
            let
                dx =
                    if Set.member code (model.pressed) then
                        2 * dir
                    else if Set.member buddyCode (model.pressed) then
                        -2 * dir
                    else
                        0
            in
                image
                    [ width "52"
                    , height "19"
                    , x (toString (xpos + dx))
                    , y "90"
                    , xlinkHref ("image-ladybug/key_" ++ imageName ++ ".png")
                    , onMouseDown (KeyEvent True code)
                    , onMouseUp (KeyEvent False code)
                    ]
                    []
    in
        Svg.svg
            [ width (toString calcWidth)
            , height (toString calcHeight)
            ]
            [ image
                [ width (toString calcWidth)
                , height (toString calcHeight)
                , xlinkHref "image-ladybug/ladybug.png"
                ]
                []
            , text_
                [ fontSize "23"
                , Svg.Attributes.style "font-family: 'HP41'"
                , x offsetLCD
                , xmlSpace "preserve"
                , y "48"
                ]
                [ text (model.lcd) ]
            , text_
                [ fontSize "11"
                , x offsetLCD
                , y "62"
                , xmlSpace "preserve"
                , Svg.Attributes.style "font-family: 'Andale Mono'"
                ]
                [ text (model.annunciators) ]
            , rockerKey 20 "ON" keyON -1 keyUSER
            , rockerKey 77 "USER" keyUSER 1 keyON
            , rockerKey 169 "PRGM" keyPRGM -1 keyALPHA
            , rockerKey 226 "ALPHA" keyALPHA 1 keyPRGM
            , normalKey "35" 148 "SIGMA" keySIGMA
            , normalKey "82" 148 "INV" keyINV
            , normalKey "129" 148 "SQRT" keySQRT
            , normalKey "176" 148 "LOG" keyLOG
            , normalKey "223" 148 "LN" keyLN
            , normalKey "35" 197 "SWAP" keySWAP
            , normalKey "82" 197 "RDN" keyRDN
            , normalKey "129" 197 "SIN" keySIN
            , normalKey "176" 197 "COS" keyCOS
            , normalKey "223" 197 "TAN" keyTAN
            , normalKey "35" 246 "SHIFT" keySHIFT
            , normalKey "82" 246 "XEQ" keyXEQ
            , normalKey "129" 246 "STO" keySTO
            , normalKey "176" 246 "RCL" keyRCL
            , normalKey "223" 246 "SST" keySST
            , wideKey "35" 295 "ENTER" keyENTER
            , normalKey "129" 295 "CHS" keyCHS
            , normalKey "176" 295 "EEX" keyEEX
            , normalKey "223" 295 "ARROW" keyARROW
            , normalKey "35" 344 "MINUS" keyMINUS
            , normalKey "98" 344 "7" key7
            , normalKey "161" 344 "8" key8
            , normalKey "224" 344 "9" key9
            , normalKey "35" 393 "PLUS" keyPLUS
            , normalKey "98" 393 "4" key4
            , normalKey "161" 393 "5" key5
            , normalKey "224" 393 "6" key6
            , normalKey "35" 442 "MUL" keyMUL
            , normalKey "98" 442 "1" key1
            , normalKey "161" 442 "2" key2
            , normalKey "224" 442 "3" key3
            , normalKey "35" 491 "DIV" keyDIV
            , normalKey "98" 491 "0" key0
            , normalKey "161" 491 "DOT" keyDOT
            , normalKey "224" 491 "RUN" keyRUN
            ]


type alias Model =
    { lcd : String
    , annunciators : String
    , pressed : Set Int
    }


init : ( Model, Cmd Msg )
init =
    ( { lcd = ""
      , annunciators = ""
      , pressed = Set.empty
      }
    , Cmd.none
    )


type Msg
    = UpdateLCD String String
    | KeyEvent Bool Int
    | SendMessage String
    | Null


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateLCD text ann ->
            ( { model | lcd = text, annunciators = ann }, Cmd.none )

        KeyEvent pressed kc ->
            ( updatePressedKey pressed kc model
            , Task.perform
                (sendKey pressed kc)
                Time.now
            )

        SendMessage body ->
            ( model, WebSocket.send webSocket body )

        Null ->
            ( model, Cmd.none )



-- Update key press status


updatePressedKey down kc model =
    let
        f =
            if down then
                Set.insert
            else
                Set.remove
    in
        { model | pressed = f kc (model.pressed) }


view : Model -> Html Msg
view model =
    calc model


subscriptions model =
    Sub.batch
        [ Keyboard.downs (key (KeyEvent True))
        , Keyboard.ups (key (KeyEvent False))
        , WebSocket.listen webSocket receiveMessage
        ]



-- 112 is F1
-- keyEvent pressed kc = KeyEvent (KeyData pressed timestamp kc)
-- Put together an outgoing key event.


sendKey pressed kc time =
    let
        intTime =
            round (Time.inMilliseconds time)

        method =
            if pressed then
                "key_press"
            else
                "key_release"
    in
        sendNotification method
            (Encode.object
                [ ( "code", Encode.int kc )
                , ( "timestamp", Encode.int intTime )
                ]
            )



-- Prepare an outgoing JSON-RPC notification message.


sendNotification method params =
    let
        call =
            Encode.object
                [ ( "jsonrpc", Encode.string "2.0" )
                , ( "method", Encode.string method )
                , ( "params", params )
                ]
    in
        SendMessage (Encode.encode 0 call)


receiveMessage s =
    case
        decodeString
            (Decode.map2 (,)
                (Decode.field "method" Decode.string)
                (Decode.field "params" (Decode.dict Decode.string))
                )
{-
            (Decode.object2 (,)
                ("method" := Decode.string)
                ("params" := Decode.dict Decode.string)
            )
-}
            s
    of
        Ok ( "lcd-update", params ) ->
            let
                text =
                    withDefault "" (Dict.get "lcd" params)

                ann =
                    withDefault "" (Dict.get "ann" params)
            in
                UpdateLCD text ann

        -- An unimplemented message, we just ignore it
        Ok ( method, _ ) ->
            Null

        -- Errors are silently dropped here.
        Err err ->
            Null


webSocket =
    "ws://localhost:8080"



-- Convert a key press/release to a key code event


key ctor k =
    case Dict.get k keyCodes of
        Just kc ->
            ctor kc

        Nothing ->
            Null



-- The mapping of keyboard keys to keycodes


keyCodes =
    Dict.fromList
        [ ( 112, keyON ) -- F1
        , ( 113, keyUSER ) -- F2
        , ( 114, keyPRGM ) -- F3
        , ( 115, keyALPHA ) -- F4
        , ( 116, keyRUN ) -- F5
        , ( 65, keySIGMA ) -- A
        , ( 66, keyINV ) -- B
        , ( 67, keySQRT ) -- C
        , ( 68, keyLOG ) -- D
        , ( 69, keyLN ) -- E
        , ( 70, keySWAP ) -- F
        , ( 71, keyRDN ) -- G
        , ( 72, keySIN ) -- H
        , ( 73, keyCOS ) -- I
        , ( 74, keyTAN ) -- J
        , ( 16, keySHIFT ) -- shift
        , ( 75, keyXEQ ) -- K
        , ( 76, keySTO ) -- L
        , ( 77, keyRCL ) -- M
        , ( 9, keySST ) -- TAB
        , ( 78, keyENTER ) -- N
        , ( 13, keyENTER ) -- enter
        , ( 79, keyCHS ) -- O
        , ( 80, keyEEX ) -- P
        , ( 8, keyARROW ) -- backspace
        , ( 81, keyMINUS ) -- Q
        , ( 82, key7 ) -- R
        , ( 83, key8 ) -- S
        , ( 84, key9 ) -- T
        , ( 85, keyPLUS ) -- U
        , ( 86, key4 ) -- V
        , ( 87, key5 ) -- W
        , ( 88, key6 ) -- X
        , ( 89, keyMUL ) -- Y
        , ( 90, key1 ) -- Z
        , ( 32, key0 ) -- space
        , ( 48, key0 ) -- 0
        , ( 49, key1 ) -- 1
        , ( 50, key2 ) -- 2
        , ( 51, key3 ) -- 3
        , ( 52, key4 ) -- 4
        , ( 53, key5 ) -- 5
        , ( 54, key6 ) -- 6
        , ( 55, key7 ) -- 7
        , ( 56, key8 ) -- 8
        , ( 57, key9 ) -- 9
        , ( 191, keyDIV ) -- /
        , ( 187, key2 ) -- =
        , ( 188, keyDOT ) -- .
        , ( 190, keyDOT ) -- ,
        ]



-- Key codes.


keyON =
    0x18


keyUSER =
    0xC6


keyPRGM =
    0xC5


keyALPHA =
    0xC4


keySIGMA =
    0x10


keyINV =
    0x30


keySQRT =
    0x70


keyLOG =
    0x80


keyLN =
    0xC0


keySWAP =
    0x11


keyRDN =
    0x31


keySIN =
    0x71


keyCOS =
    0x81


keyTAN =
    0xC1


keySHIFT =
    0x12


keyXEQ =
    0x32


keySTO =
    0x72


keyRCL =
    0x82


keySST =
    0xC2


keyENTER =
    0x13


keyCHS =
    0x73


keyEEX =
    0x83


keyARROW =
    0xC3


keyMINUS =
    0x14


key7 =
    0x34


key8 =
    0x74


key9 =
    0x84


keyPLUS =
    0x15


key4 =
    0x35


key5 =
    0x75


key6 =
    0x85


keyMUL =
    0x16


key1 =
    0x36


key2 =
    0x76


key3 =
    0x86


keyDIV =
    0x17


key0 =
    0x37


keyDOT =
    0x77


keyRUN =
    0x87
