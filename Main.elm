port module Main exposing
    ( main
    , portSyncAcquiredCellsToLocal
    , portRequestSyncAcquiredCellsFromLocal
    , portResponseSyncAcquiredCellsFromLocal
    )

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Monocle.Lens exposing (Lens)
import Json.Encode as Json
import Json.Decode as UnJson
import Return exposing (Return, singleton, command)
import Array exposing (Array)
import Result exposing (withDefault)
import Set exposing (Set, empty, member, remove, insert, toList, fromList)

port portSyncAcquiredCellsToLocal : String -> Cmd a
port portRequestSyncAcquiredCellsFromLocal : String -> Cmd a
port portResponseSyncAcquiredCellsFromLocal : (String -> a) -> Sub a

type Action
    = ToggleCell Int
    | SyncAcquiredCells (Set Int)
    | NoOp

type alias Cell =
    { thumbnail : String
    , location : String
    , notes : String
    }

type alias Model =
    { cells : Array Cell
    , acquiredCells : Set Int
    }

type alias Flags =
    List Cell

acquiredCells : Lens Model (Set Int)
acquiredCells =
    let
        set val model =
            { model | acquiredCells = val }
    in
        Lens .acquiredCells set

integerPercentage : Int -> Int -> String
integerPercentage a b =
    flip (++) "%"
        << toString
        << (*) 100
        <| toFloat a / toFloat b

init : Flags -> Return Action Model
init cells =
    singleton
        { cells = Array.fromList cells
        , acquiredCells = empty
        }
        |> command (portRequestSyncAcquiredCellsFromLocal "")

viewCell : Set Int -> Int -> Cell -> Html Action
viewCell activeCellIndeces index cell =
    let
        isActiveCellIndex =
            member index activeCellIndeces
    in
        li
            [ onClick <| ToggleCell index
            ]
            [ div
                [ classList
                    [ ("checked", isActiveCellIndex)
                    , ("thumb", True)
                    ]
                ]
                [ img
                    [ src cell.thumbnail
                    ]
                    []
                ]
            , em
                [ classList
                    [ ("struck", isActiveCellIndex)
                    ]
                ]
                [ text cell.location
                ]
            , i [] [ text cell.notes ]
            ]

viewCellCounter : Int -> Int -> List (Html Action)
viewCellCounter numCells numAcquiredCells =
    [ div
        [ class "progress-bar"
        , style
            [ ("width", integerPercentage numAcquiredCells numCells)
            ]
        ]
        []
    , em
        [ class "count"
        ]
        [ text << toString <| numAcquiredCells
        ]
    , span
        [ class "total"
        ]
        [ text << toString <| numCells
        ]
    ]

view : Model -> Html Action
view model =
    div
        [ class "list"
        ]
        [ h1 [] [ text "Zygarde Cell Checklist" ]
        , ul
            [ class "cells"
            ]
            <| Array.toList
            <| Array.indexedMap (viewCell model.acquiredCells) model.cells
        , div
            [ class "counter"
            ]
            <| viewCellCounter (Array.length model.cells) (Set.size model.acquiredCells)
        ]

syncAcquiredCellsToLocal : Set Int -> Cmd Action
syncAcquiredCellsToLocal =
    portSyncAcquiredCellsToLocal << jsonEncodeAqcuiredCells

jsonEncodeAqcuiredCells : Set Int -> String
jsonEncodeAqcuiredCells =
    Json.encode 0
        << Json.list
        << List.map Json.int
        << toList

jsonDecodeAqcuiredCells : String -> Set Int
jsonDecodeAqcuiredCells =
    withDefault empty
        << Result.map fromList
        << UnJson.decodeString (UnJson.list UnJson.int)

update : Action -> Model -> (Model, Cmd Action)
update action model =
    case action of
        ToggleCell index ->
            let
                method =
                    if member index model.acquiredCells then remove else insert
                updatedModel =
                    acquiredCells.set (method index model.acquiredCells) model
            in
                singleton updatedModel
                    |> command (syncAcquiredCellsToLocal updatedModel.acquiredCells)
        SyncAcquiredCells cellsFromLocal ->
            singleton model
                |> Return.map (acquiredCells.set cellsFromLocal)
        NoOp ->
            singleton model

subscriptions : Model -> Sub Action
subscriptions model =
    portResponseSyncAcquiredCellsFromLocal (SyncAcquiredCells << jsonDecodeAqcuiredCells)

main : Program Flags Model Action
main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
