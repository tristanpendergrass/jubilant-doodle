port module Main exposing (main)

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
    , maxHealth : Int
    , command : Command
    , cooldownTime : Float -- time in seconds for their ability to be off cooldown
    , avatar : String
    }


type alias Ally =
    { stats : AllyStats
    , cooldown : Cooldown
    , health : Meter
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
    { name = "Ralph", maxHealth = 100, command = Attack 150, cooldownTime = 4.0, avatar = "derp_cropped.png" }


sophieStats : AllyStats
sophieStats =
    { name = "Sophie", maxHealth = 100, command = Attack 20, cooldownTime = 8.0, avatar = "derp2_cropped.png" }


ernestStats : AllyStats
ernestStats =
    { name = "Ernest", maxHealth = 100, command = Attack 40, cooldownTime = 1.0, avatar = "derp3_cropped.png" }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        initialFightData : FightData
        initialFightData =
            { allyOne = { stats = ralphStats, cooldown = OffCooldown, health = Meter.create ralphStats.maxHealth }
            , allyTwo = { stats = sophieStats, cooldown = OffCooldown, health = Meter.create sophieStats.maxHealth }
            , allyThree = { stats = ernestStats, cooldown = OffCooldown, health = Meter.create ernestStats.maxHealth }
            , enemy = { name = "Cave Turkey", health = Meter.create 300, reward = BonusAttack }
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


port emitSound : String -> Cmd msg


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
                        newCommandPanel =
                            case fightData.commandPanel of
                                Open openAlly ->
                                    if openAlly == ally then
                                        Closed

                                    else
                                        Open ally

                                _ ->
                                    Open ally

                        newFightData =
                            { fightData
                                | commandPanel = newCommandPanel
                            }
                    in
                    ( { model | game = Fight newFightData }, emitSound "interface3" )

        -- change
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


selectedColor : String
selectedColor =
    "bg-blue-500"


renderHealth : Meter -> Html Msg
renderHealth health =
    let
        current =
            Meter.getValue health

        max =
            Meter.getMax health

        percent =
            toFloat current / toFloat max

        pixelWidthMax =
            200

        pixelWidthCurrent =
            round (pixelWidthMax * percent)

        maxHealthValue =
            String.fromInt pixelWidthMax ++ "px"

        currentHealthValue =
            String.fromInt pixelWidthCurrent ++ "px"
    in
    div [ style "width" maxHealthValue, class "relative h-6 bg-red-500" ]
        [ div [ style "width" currentHealthValue, class "absolute top-0 left-0 bg-green-600 h-full" ] []
        ]


renderCooldown : Cooldown -> Html Msg
renderCooldown cooldown =
    case cooldown of
        OffCooldown ->
            div [ style "width" "200px", class "relative h-2 bg-yellow-300" ] []

        OnCooldown ticker ->
            let
                width =
                    2 * Ticker.toPercent ticker

                widthStyle =
                    String.fromInt width ++ "px"
            in
            div [ style "width" "200px", class "relative h-2 bg-yellow-600" ]
                [ div [ style "width" widthStyle, class "absolute top-0 left-0 bg-yellow-300 h-full" ] []
                ]


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

                isAllySelected =
                    case fightData.commandPanel of
                        Closed ->
                            False

                        Open openAlly ->
                            openAlly == ally

                allyName =
                    div
                        [ class <|
                            if isAllySelected then
                                "underline"

                            else
                                ""
                        , class "text-lg font-thin leading-tight"
                        ]
                        [ text stats.name ]

                renderPortrait =
                    div [ class "relative" ]
                        [ div [ class "w-24 h-24 border border-gray-100" ]
                            [ img [ class "object-cover h-full", src ally.stats.avatar ] []
                            ]
                        , div [ class "absolute top-0 left-0 mt-1 ml-1 border border-gray-100 w-6 h-6 flex items-center justify-center bg-gray-100 text-gray-900 rounded" ] [ text <| positionText ]
                        ]

                renderOpenCommandPanel =
                    case ally.cooldown of
                        OffCooldown ->
                            button [ onClick <| OpenCommandPanel position ] [ text "Command" ]

                        OnCooldown _ ->
                            div [] []
            in
            div
                [ class "flex items-stretch space-x-2 p-2"
                , class <|
                    if isAllySelected then
                        selectedColor

                    else
                        ""
                ]
                [ renderPortrait
                , div [ class "flex flex-col justify-start flex-grow space-y-2" ]
                    [ allyName
                    , renderHealth ally.health
                    , renderCooldown ally.cooldown
                    ]
                , div [ class "flex flex-col justify-center" ]
                    [ renderOpenCommandPanel ]
                ]
    in
    div [ class "flex flex-col" ]
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
            div [ class "w-full h-full relative p-2", class selectedColor ]
                [ button [ class "absolute top-0 right-0 mr-2 mt-2", onClick CloseCommandPanel ] [ text "(q) Close" ]
                , div [] [ text "Command Panel" ]
                , button [ onClick CommandAttack ] [ text <| "(a) Attack - " ++ String.fromInt damage ]
                ]


renderEnemy : FightData -> Html Msg
renderEnemy { enemy } =
    let
        renderName : Html Msg
        renderName =
            div
                [ class "text-lg font-thin leading-tight"
                ]
                [ text "Cave Turkey" ]

        renderPortrait =
            div [ class "w-24 h-24 border border-gray-100" ]
                [ img [ class "object-cover h-full", src "caveturkey.png" ] []
                ]
    in
    div
        [ class "flex space-x-2 p-2 justify-center mt-24" ]
        [ div [ class "flex flex-col justify-start items-end space-y-2" ]
            [ renderName
            , renderHealth enemy.health
            ]
        , renderPortrait
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
                    "flex-1 border border-gray-100 h-full overflow-auto"
            in
            div [ class "w-screen h-screen flex space-x-4 p-4" ]
                [ div [ class sectionClass, class "p-2" ] [ renderAllies fightData ]
                , div [ class sectionClass ] [ renderCommandPanel fightData ]
                , div [ class sectionClass, class "p-2" ] [ renderEnemy fightData ]
                ]
