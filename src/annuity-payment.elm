import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Maybe.Extra exposing (isNothing)
import Round exposing (round)


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

          monthInterest =
            (Maybe.withDefault 0 interestMaybe) / (12 * 100)

          duration =
            Maybe.withDefault 0 durationMaybe
          
          x = ( 1 + monthInterest ) ^ duration

          k = ( monthInterest * x ) / (x - 1)

          anuitent = k * amount
      in
        div [ style "color" "green" ] [ text ( "Вот вам результат: " ++ round 2 anuitent ) ]
