import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Maybe.Extra exposing (isNothing)
import Round exposing (..)


-- TYPES

type alias Amount = String

type alias Interest = String

type alias Duration = String

-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
  { amount : Amount
  , interest : Interest
  , duration : Duration
  }


init : Model
init =
  Model "" "" ""



-- UPDATE


type Msg
  = ChangeAmount Amount
  | ChangeInterest Interest
  | ChangeDuration Duration


update : Msg -> Model -> Model
update msg model =
  case msg of
    ChangeAmount amount ->
      { model | amount = amount }

    ChangeInterest interest ->
      { model | interest = interest }

    ChangeDuration duration ->
      { model | duration = duration }



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

          anuitentInt = Debug.log "anuintenInt" <| Basics.round <| anuitent * minorUnitsFactor

          amountInt = Debug.log "amountInt" <| Basics.round <| amount * minorUnitsFactor

          lst = calcPayments (Payment anuitentInt 0 0 interest amountInt)
      in
        div [] 
        [ div [ style "color" "green" ] [ text ( "Вот вам результат: " ++ Round.round 2 anuitent ) ]
        , table [] (List.map (\l -> tr [] 
                    [ td [] [ text <| ( toString l.payment ) ]
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
  }

calcPayments : Payment -> List Payment
calcPayments payment =
  if payment.debt <= 0 || payment.debtPayment < 0 then
    [payment]
  else
    payment :: calcPayments (calcNexPayment payment)

calcNexPayment : Payment -> Payment
calcNexPayment {payment, interest, debt} =
  let 
    interestPayment = Debug.log "interestPayment" <| Basics.round ( ( toFloat debt ) * interest * 31 / ( 365 * interestFactor ) )

    debtPayment = Debug.log "debtPayment" <| Basics.min (payment - interestPayment) debt

    newDebt = Debug.log "newDebt" <| debt - debtPayment 
  in
    Payment payment debtPayment interestPayment interest newDebt

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
