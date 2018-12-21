import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Maybe.Extra exposing (isNothing)
import Round exposing (..)
import Time exposing (..)
import Time.Extra as Time
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Decode.Extra as Decode  --"elm-community/json-extra"

-- TYPES

type alias Amount = String

type alias Interest = String

type alias Duration = String

-- MAIN


main =
  Browser.element 
    { init = init
    , update = update
    , subscriptions = \s -> Sub.none
    , view = view 
    }



-- MODEL


type alias Model =
  { amount : Amount
  , interest : Interest
  , duration : Duration
  }

type alias Flags = Decode.Value



flagsDecoder : Decoder Model
flagsDecoder =
    Decode.map3 Model
        (Decode.field "amount" Decode.string |> Decode.withDefault "")
        (Decode.field "interest" Decode.string |> Decode.withDefault "")
        (Decode.field "duration" Decode.string |> Decode.withDefault "")


init : Flags -> ( Model, Cmd msg )
init flags =
    case Decode.decodeValue flagsDecoder flags of
        Err _ ->
            ( Model "" "" "", Cmd.none )

        Ok model ->
            ( model, Cmd.none )



-- UPDATE


type Msg
  = ChangeAmount Amount
  | ChangeInterest Interest
  | ChangeDuration Duration


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
  case msg of
    ChangeAmount amount ->
      ( { model | amount = amount }, Cmd.none )

    ChangeInterest interest ->
      ( { model | interest = interest }, Cmd.none )

    ChangeDuration duration ->
      ( { model | duration = duration }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ viewInput "Сумма кредита" model.amount ChangeAmount
    , viewInput "Годовая ставка" model.interest ChangeInterest
    , viewInput "Срок погашения" model.duration ChangeDuration
    , viewValidation model
    ]


viewInput : String -> String -> (String -> msg) -> Html msg
viewInput p v toMsg =
  input [ type_ "text", placeholder p, value v, onInput toMsg ] []


viewValidation : Model -> Html msg
viewValidation model =
  let
      amountMaybe =
        String.toFloat model.amount

      interestMaybe =
        String.toFloat model.interest

      durationMaybe =
        String.toFloat model.duration
  in
    if isNothing(amountMaybe) || isNothing(interestMaybe) || isNothing(durationMaybe) then
      div [ style "color" "red" ] [ text "Параметры для расчета не указаны или указаны не корректно" ]
    else
      let
          amount =
            Maybe.withDefault 0 amountMaybe

          interest = Maybe.withDefault 0 interestMaybe

          monthInterest =
            interest / ( 12 * interestFactor )

          duration =
            Maybe.withDefault 0 durationMaybe

          k = monthInterest / ( 1 - ( 1 + monthInterest)^( -1 * duration ) )

          anuitent = k * amount

          anuitentInt = {-- Debug.log "anuintenInt" <| --}
            Basics.round <| anuitent * minorUnitsFactor

          amountInt = {-- Debug.log "amountInt" <| --}
            Basics.round <| amount * minorUnitsFactor

          lst = calcPayments (Payment anuitentInt 0 0 interest amountInt 
                {-- ( Time.partsToPosix utc (Time.Parts 2017 Dec 13 0 0 0 0) ) --}
                {--} ( millisToPosix 0 ) --}
                0)
      in
        div [] 
        [ div [ style "color" "green" ] [ text ( "Вот вам результат: " ++ Round.round 2 anuitent ) ]
        , table [] (List.map (\l -> tr [] 
                    [ td [] [ text <| ( String.fromInt l.periodNum ) ]
                    , td [] [ text <| ( toString l.payment ) ]
                    , td [] [ text <| ( toString l.debtPayment ) ]
                    , td [] [ text <| ( toString l.interestPayment ) ]
                    , td [] [ text <| ( Round.round minorUnits l.interest ) ]
                    , td [] [ text <| ( toString l.debt ) ] 
                    ] ) lst)
        ]

minorUnits : Int
minorUnits = 2

minorUnitsFactorInt : Int
minorUnitsFactorInt = 10 ^ minorUnits

minorUnitsFactor : Float
minorUnitsFactor = toFloat minorUnitsFactorInt

interestFactor : Float
interestFactor = 100

type alias Payment =
  { payment : Int
  , debtPayment : Int
  , interestPayment : Int
  , interest : Float
  , debt : Int
  , date : Posix
  , periodNum : Int
  }

calcPayments : Payment -> List Payment
calcPayments payment =
  if payment.debt <= 0 || payment.debtPayment < 0 then
    [payment]
  else
    payment :: calcPayments (calcNexPayment payment)

calcNexPayment : Payment -> Payment
calcNexPayment {payment, interest, debt, date, periodNum} =
  let
    newDate = calcDate date

    days = calcDiff date newDate

    interestPayment = {-- Debug.log "interestPayment" <| --}
      Basics.min payment <| Basics.round ( ( toFloat debt ) * days * interest / ( 365 * interestFactor ) ) 

    debtPayment = {-- Debug.log "debtPayment" <| --}
      Basics.min (payment - interestPayment) debt

    newDebt = {-- Debug.log "newDebt" <| --}
      debt - debtPayment 
  in
    Payment payment debtPayment interestPayment interest newDebt newDate (periodNum + 1)

toString : Int -> String
toString value =
  let
    str = String.fromInt value
  in
    if value == 0 then
      "0." ++ String.repeat minorUnits "0"
    else if value < minorUnitsFactorInt && value > 0 then
      "0." ++ (String.repeat (minorUnits - String.length str) "0" ) ++ str
    else if value > minorUnitsFactorInt  && value < 0 then
      "-0." ++ (String.repeat (minorUnits - 1 - String.length str) "0" ) ++ (String.dropLeft 1 str)
    else
      let
        minorUnitsString = String.right minorUnits str
      in
        String.dropRight minorUnits str ++ "." ++ minorUnitsString

calcDate : Posix -> Posix
calcDate date =
  if posixToMillis date == 0 then
    date
  else
    Time.add Time.Month 1 utc date

calcDiff : Posix -> Posix -> Float
calcDiff dateAfter dateBefore =
  if posixToMillis dateBefore == 0 then
    365 / 12
  else
    Time.diff Time.Day utc dateAfter dateBefore |> toFloat