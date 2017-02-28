module App exposing (..)

import Html exposing (Html, text, div, ul, li, span)
import Html.Attributes exposing (style)
import Html.Events exposing (on, onMouseEnter, onMouseLeave)
import Json.Decode exposing (Decoder)
import Mouse exposing (Position)
import DOM


init =
    ( { items =
            [ "Item 1"
            , "Item 2"
            , "Item 3"
            , "Item 4"
            , "Item 5"
            , "Item 6"
            ]
      , dragging = Nothing
      , draggingOver = Nothing
      }
    , Cmd.none
    )


type Msg
    = DragStart Int Float
    | DragStop
    | DragOver Int
    | ContainerLeave


update msg model =
    case msg of
        DragStart index height ->
            ( { model | dragging = Just ( index, height ) }, Cmd.none )

        DragOver index ->
            ( { model | draggingOver = Just index }, Cmd.none )

        DragStop ->
            ( { model | dragging = Nothing }, Cmd.none )

        ContainerLeave ->
            ( { model | draggingOver = Nothing }, Cmd.none )


decoder =
    DOM.target DOM.offsetHeight


view model =
    div []
        [ ul
            [ style [ ( "user-select", "none" ) ]
            , onMouseLeave ContainerLeave
            ]
            (List.indexedMap
                (\index item ->
                    li
                        (model.dragging
                            |> Maybe.map
                                (\( draggingIndex, height ) ->
                                    if index == draggingIndex then
                                        [ style [ ( "visibility", "hidden" ) ] ]
                                    else
                                        [ onMouseEnter (DragOver index) ]
                                )
                            |> Maybe.withDefault [ on "mousedown" (Json.Decode.map (DragStart index) decoder) ]
                        )
                        [ text item ]
                )
                model.items
            )
        ]


subscriptions model =
    case model.dragging of
        Just index ->
            Mouse.ups (always DragStop)

        Nothing ->
            Sub.none
