module App exposing (..)

import Html exposing (Html, text, div, ul, li, span)
import Html.Attributes exposing (style)
import Html.Events exposing (on)
import Json.Decode exposing (Decoder)
import List.Extra
import Mouse exposing (Position)
import DOM exposing (Rectangle)
import PageVisibility exposing (Visibility(..))


init =
    ( { items =
            [ "Item 1"
            , "Item 2"
            , "Item 3"
            , "Item 4"
            , "Item 5"
            , "Item 6"
            ]
      , mouse = Nothing
      , dragging = False
      , draggingIndex = Nothing
      , dragTarget = Nothing
      , itemOffsets = []
      , draggingOver = Nothing
      }
    , Cmd.none
    )


type Msg
    = DragStart Int DragTarget
    | DragStop Position
    | VisibilityChange Visibility
    | Move Position


update msg model =
    case msg of
        DragStart index dragTarget ->
            let
                -- Calculate the boundaries on y axis
                itemOffsets =
                    dragTarget.itemsSizes
                        |> List.map .height
                        |> List.scanl (+) 0
                        |> List.filter ((/=) 0)
            in
                ( { model
                    | dragging = True
                    , draggingIndex = Just index
                    , dragTarget = Just dragTarget
                    , itemOffsets = itemOffsets
                  }
                , Cmd.none
                )

        DragStop position ->
            let
                disableDragging =
                    { model
                        | dragging = False
                        , draggingIndex = Nothing
                        , mouse = Nothing
                        , dragTarget = Nothing
                        , draggingOver = Nothing
                        , itemOffsets = []
                    }
            in
                ( Maybe.map2
                    (\from to ->
                        let
                            fromValMaybe =
                                List.Extra.getAt from model.items
                        in
                            case fromValMaybe of
                                Just fromVal ->
                                    model
                                        |> .items
                                        |> List.indexedMap (,)
                                        |> List.foldl
                                            (\( index, value ) acc ->
                                                if index == to then
                                                    --  If dragging element is below, then prepend in after
                                                    if from < to then
                                                        fromVal :: value :: acc
                                                    else if from > to then
                                                        value :: fromVal :: acc
                                                    else
                                                        fromVal :: acc
                                                else if index == from then
                                                    acc
                                                else
                                                    value :: acc
                                            )
                                            []
                                        |> List.reverse

                                Nothing ->
                                    model.items
                    )
                    model.draggingIndex
                    model.draggingOver
                    |> Maybe.map (\items -> { disableDragging | items = items })
                    |> Maybe.withDefault disableDragging
                , Cmd.none
                )

        Move position ->
            model
                |> .dragTarget
                |> Maybe.map (\{ parentBoundingClientRect } -> List.map (\v -> v + parentBoundingClientRect.top) model.itemOffsets)
                |> Maybe.map (List.indexedMap (,))
                |> Maybe.map (List.Extra.find (\( index, offset ) -> position.y < truncate offset))
                |> Maybe.andThen (Maybe.map (\( index, _ ) -> { model | mouse = Just position, draggingOver = Just index }))
                |> Maybe.withDefault model
                |> \m -> ( m, Cmd.none )

        VisibilityChange visibility ->
            case visibility of
                Hidden ->
                    ( { model
                        | dragging = False
                        , draggingIndex = Nothing
                        , mouse = Nothing
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )


type alias Size =
    { width : Float
    , height : Float
    }


type alias DragTarget =
    { parentBoundingClientRect : Rectangle
    , itemsSizes : List Size
    }


decoder : Decoder DragTarget
decoder =
    DOM.target
        (Json.Decode.map2
            DragTarget
            (DOM.parentElement (DOM.parentElement DOM.boundingClientRect))
            (DOM.parentElement
                (DOM.parentElement
                    (DOM.childNodes
                        (Json.Decode.map2
                            Size
                            DOM.offsetWidth
                            DOM.offsetHeight
                        )
                    )
                )
            )
        )


view model =
    div []
        [ ul
            [ style [ ( "user-select", "none" ) ] ]
            (model.items
                |> List.map (\t -> span [] [ text t ])
                |> List.indexedMap
                    (\index item ->
                        li
                            [ on "mousedown" (Json.Decode.map (DragStart index) decoder) ]
                            [ item ]
                    )
            )
        ]


subscriptions model =
    if model.dragging == True then
        Sub.batch
            [ Mouse.moves Move
            , Mouse.ups DragStop
            , PageVisibility.visibilityChanges VisibilityChange
            ]
    else
        Sub.none
