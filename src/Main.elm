module Main exposing (main)

import Browser
import Browser.Events
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Meter exposing (Meter)
import Ticker exposing (Ticker)


main : Program () Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- MODEL


type Position
    = First
    | Second
    | Third


type UserInput
    = ActivateOne
    | ActivateTwo
    | ActivateThree
    | Cancel
    | UnknownInput


type Game
    = Fight FightData
    | AfterFight


type Command
    = Attack Int


type Cooldown
    = OffCooldown
    | OnCooldown Ticker


type alias FightData =
    { allyOne : Ally
    , allyTwo : Ally
    , allyThree : Ally
    , enemy : Enemy
    , commandPanel : CommandPanel
    , bonusAttack : Float
    }


type alias AllyStats =
    { name : String
    , health : Meter
    , command : Command
    , cooldownTime : Float -- time in seconds for their ability to be off cooldown
    }


type alias Ally =
    { stats : AllyStats
    , cooldown : Cooldown
    }


type alias Enemy =
    { name : String
    , health : Meter
    , reward : Reward
    }


type CommandPanel
    = Closed
    | Open Ally


type Reward
    = BonusAttack


type alias Model =
    { game : Game }


ralphStats : AllyStats
ralphStats =
    { name = "Ralph", health = Meter.create 100, command = Attack 150, cooldownTime = 4.0 }


sophieStats : AllyStats
sophieStats =
    { name = "Sophie", health = Meter.create 100, command = Attack 20, cooldownTime = 8.0 }


ernestStats : AllyStats
ernestStats =
    { name = "Ernest", health = Meter.create 100, command = Attack 40, cooldownTime = 1.0 }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        initialFightData : FightData
        initialFightData =
            { allyOne = { stats = ralphStats, cooldown = OffCooldown }
            , allyTwo = { stats = sophieStats, cooldown = OffCooldown }
            , allyThree = { stats = ernestStats, cooldown = OffCooldown }
            , enemy = { name = "Shrek", health = Meter.create 300, reward = BonusAttack }
            , commandPanel = Closed
            , bonusAttack = 1.0
            }
    in
    ( { game = Fight initialFightData }, Cmd.none )


nextEnemy : Enemy -> Enemy
nextEnemy enemy =
    { name = enemy.name ++ " *"
    , health = Meter.create <| Meter.getMax enemy.health + 100
    , reward = BonusAttack
    }



-- UPDATE


type Msg
    = NoOp
    | OpenCommandPanel Position
    | CloseCommandPanel
    | CommandAttack
    | HandleAnimationFrame Float


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        noOp =
            ( model, Cmd.none )
    in
    case ( model.game, msg ) of
        ( _, NoOp ) ->
            noOp

        ( AfterFight, _ ) ->
            noOp

        ( Fight fightData, OpenCommandPanel position ) ->
            let
                ally =
                    case position of
                        First ->
                            fightData.allyOne

                        Second ->
                            fightData.allyTwo

                        Third ->
                            fightData.allyThree
            in
            case ally.cooldown of
                OnCooldown _ ->
                    noOp

                OffCooldown ->
                    let
                        newFightData =
                            { fightData
                                | commandPanel = Open ally
                            }
                    in
                    ( { model | game = Fight newFightData }, Cmd.none )

        ( Fight fightData, CloseCommandPanel ) ->
            let
                newFightData : FightData
                newFightData =
                    { fightData
                        | commandPanel = Closed
                    }
            in
            ( { model | game = Fight newFightData }, Cmd.none )

        ( Fight fightData, CommandAttack ) ->
            case fightData.commandPanel of
                Closed ->
                    noOp

                Open ally ->
                    let
                        stats =
                            ally.stats

                        damage =
                            case stats.command of
                                Attack value ->
                                    value
                                        |> toFloat
                                        |> (*) fightData.bonusAttack
                                        |> round

                        enemy =
                            fightData.enemy

                        newHealth =
                            Meter.decrement damage enemy.health

                        ( newEnemy, newBonusAttack ) =
                            if Meter.getValue newHealth > 0 then
                                ( { enemy | health = newHealth }, fightData.bonusAttack )

                            else
                                ( nextEnemy enemy, fightData.bonusAttack + 0.25 )

                        newAlly =
                            { ally | cooldown = OnCooldown Ticker.create }

                        newFightData =
                            { fightData | commandPanel = Closed, enemy = newEnemy, bonusAttack = newBonusAttack }

                        newNewFightData =
                            if fightData.allyOne == ally then
                                { newFightData | allyOne = newAlly }

                            else if fightData.allyTwo == ally then
                                { newFightData | allyTwo = newAlly }

                            else if fightData.allyThree == ally then
                                { newFightData | allyThree = newAlly }

                            else
                                -- TODO: this is an impossible state and it should not have to be accounted for
                                newFightData
                    in
                    ( { model | game = Fight newNewFightData }, Cmd.none )

        ( Fight fightData, HandleAnimationFrame delta ) ->
            let
                updateAlly : Ally -> Ally
                updateAlly ally =
                    case ally.cooldown of
                        OffCooldown ->
                            ally

                        OnCooldown ticker ->
                            let
                                modifiedDelta =
                                    delta / ally.stats.cooldownTime

                                tickOutcome =
                                    Ticker.advance modifiedDelta ticker
                            in
                            case tickOutcome of
                                Ticker.NoTick newCooldown ->
                                    { ally | cooldown = OnCooldown newCooldown }

                                Ticker.Ticked _ _ ->
                                    { ally | cooldown = OffCooldown }
            in
            ( { model
                | game =
                    Fight
                        { fightData
                            | allyOne = updateAlly fightData.allyOne
                            , allyTwo = updateAlly fightData.allyTwo
                            , allyThree = updateAlly fightData.allyThree
                        }
              }
            , Cmd.none
            )



-- SUBSCRIPTIONS


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map toUserInput (Decode.field "key" Decode.string)


toUserInput : String -> Msg
toUserInput string =
    case string of
        "1" ->
            OpenCommandPanel First

        "2" ->
            OpenCommandPanel Second

        "3" ->
            OpenCommandPanel Third

        "Escape" ->
            CloseCommandPanel

        "q" ->
            CloseCommandPanel

        "a" ->
            CommandAttack

        _ ->
            NoOp


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.game of
        AfterFight ->
            Sub.none

        Fight fightData ->
            Sub.batch
                [ Browser.Events.onKeyUp keyDecoder
                , Browser.Events.onAnimationFrameDelta HandleAnimationFrame
                ]



-- VIEW


renderAllies : FightData -> Html Msg
renderAllies fightData =
    let
        renderAlly : Ally -> Position -> Html Msg
        renderAlly ally position =
            let
                { stats, cooldown } =
                    ally

                positionText =
                    case position of
                        First ->
                            "1"

                        Second ->
                            "2"

                        Third ->
                            "3"

                isCommandPanelOpen =
                    case fightData.commandPanel of
                        Closed ->
                            False

                        Open openAlly ->
                            openAlly == ally
            in
            div [ class "flex space-x-2" ]
                [ div
                    [ class <|
                        if isCommandPanelOpen then
                            "underline"

                        else
                            ""
                    ]
                    [ text <| "(" ++ positionText ++ ") " ++ stats.name ]
                , case ally.cooldown of
                    OffCooldown ->
                        button [ onClick (OpenCommandPanel position) ] [ text "Command" ]

                    OnCooldown ticker ->
                        div [] [ text <| "on cooldown (" ++ String.fromInt (Ticker.toPercent ticker) ++ ")" ]
                ]
    in
    div []
        [ renderAlly fightData.allyOne First
        , renderAlly fightData.allyTwo Second
        , renderAlly fightData.allyThree Third
        ]


renderCommandPanel : FightData -> Html Msg
renderCommandPanel { commandPanel, bonusAttack } =
    case commandPanel of
        Closed ->
            div [] []

        Open ally ->
            let
                damage =
                    case ally.stats.command of
                        Attack value ->
                            value
                                |> toFloat
                                |> (*) bonusAttack
                                |> round
            in
            div [ class "w-full relative" ]
                [ button [ class "absolute top-0 right-0", onClick CloseCommandPanel ] [ text "(q) Close" ]
                , div [] [ text "Command Panel" ]
                , button [ onClick CommandAttack ] [ text <| "(a) Attack - " ++ String.fromInt damage ]
                ]


renderEnemy : FightData -> Html Msg
renderEnemy { enemy } =
    let
        currentHealth =
            Meter.getValue enemy.health

        maxHealth =
            Meter.getMax enemy.health
    in
    div []
        [ div [] [ text enemy.name ]
        , div [] [ text <| "Health: " ++ String.fromInt currentHealth ++ " / " ++ String.fromInt maxHealth ]
        ]


view : Model -> Html Msg
view model =
    case model.game of
        AfterFight ->
            div [] []

        Fight fightData ->
            let
                commandPanelText =
                    case fightData.commandPanel of
                        Open _ ->
                            "Command Panel (open)"

                        Closed ->
                            ""

                sectionClass =
                    "flex-1 border border-gray-100 shadow h-72 p-2"
            in
            div [ class "w-full flex space-x-4 mt-4" ]
                [ div [ class sectionClass ] [ renderAllies fightData ]
                , div [ class sectionClass ] [ renderCommandPanel fightData ]
                , div [ class sectionClass ] [ renderEnemy fightData ]
                ]
