module Main exposing (Model, Msg(..), init, main, update, view)

import Array exposing (Array, empty, push, toList)
import Browser
import Html exposing (Attribute, Html, button, div, form, h1, input, label, li, ol, p, text)
import Html.Attributes exposing (attribute, class, for, id, placeholder, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import List.Extra as ListExtra
import Random
import Random.List as RandomList


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- MODEL


type WizardStep
    = AddingTerms
    | GeneratingCard


type alias Model =
    { newTerm : String
    , terms : List String
    , termsRequired : Int
    , feedback : Maybe String
    , wizardStep : WizardStep
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


randomWordList : List String
randomWordList =
    [ "attraction", "satisfy", "direful", "fog", "alarm", "cross", "number", "gigantic", "worthless", "fuzzy", "abandoned", "conscious", "macabre", "rainstorm", "sheet", "act", "stone", "like", "rot", "guarantee", "powerful", "careful", "lamp", "dramatic", "frogs" ]


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model "" randomWordList 25 Nothing AddingTerms, Cmd.none )



-- UPDATE


type Msg
    = AddTerm String
    | RemoveTerm String
    | UpdateNewTerm String
    | RandomizeTerms
    | RandomList (List String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddTerm newTerm ->
            if termIsUnique model then
                ( { model
                    | newTerm = ""
                    , terms = newTerm :: model.terms
                    , feedback = Nothing
                  }
                , Cmd.none
                )

            else
                ( { model | feedback = Just "Each term can only be used once." }, Cmd.none )

        UpdateNewTerm updatedTerm ->
            let
                wizardStep =
                    if List.length model.terms >= model.termsRequired then
                        GeneratingCard

                    else
                        AddingTerms
            in
            ( { model
                | newTerm = updatedTerm
                , feedback = Nothing
                , wizardStep = wizardStep
              }
            , Cmd.none
            )

        RemoveTerm existingTerm ->
            let
                updatedTerms =
                    List.filter (\term -> not (term == existingTerm)) model.terms
            in
            ( { model | terms = updatedTerms }, Cmd.none )

        RandomizeTerms ->
            ( model, generateRandomList model.terms )

        RandomList randomizedTerms ->
            ( { model | terms = randomizedTerms }, Cmd.none )



-- UPDATE HELPERS


shuffleListGenerator : List String -> Random.Generator (List String)
shuffleListGenerator list =
    RandomList.shuffle list


generateRandomList : List String -> Cmd Msg
generateRandomList terms =
    Random.generate RandomList (shuffleListGenerator terms)


termIsUnique : Model -> Bool
termIsUnique model =
    not (List.member model.newTerm model.terms)



-- VIEW HELPERS


role : String -> Attribute msg
role roleValue =
    attribute "role" roleValue



-- VIEW


view : Model -> Html Msg
view model =
    let
        remainingTermNumber =
            String.fromInt <| model.termsRequired - List.length model.terms
    in
    div [ class "container" ]
        ([ feedbackDiv model.feedback
         , p [ class "lead" ] [ text <| "Add " ++ remainingTermNumber ++ " terms to create your custom bingo card." ]
         ]
            ++ bingoCardWizard model
        )


feedbackDiv : Maybe String -> Html msg
feedbackDiv maybeFeedback =
    case maybeFeedback of
        Just feedback ->
            div [ class "alert alert-primary", role "alert" ] [ text feedback ]

        Nothing ->
            div [] []


bingoCardWizard : Model -> List (Html Msg)
bingoCardWizard model =
    case model.wizardStep of
        AddingTerms ->
            [ ol []
                (List.map
                    (\term ->
                        li []
                            [ button
                                [ onClick (RemoveTerm term)
                                , class "btn btn-link"
                                ]
                                [ text term ]
                            ]
                    )
                    model.terms
                )
            , termForm model
            ]

        GeneratingCard ->
            let
                sqrtOfRequiredTerms =
                    model.termsRequired
                        |> toFloat
                        |> sqrt
                        |> round

                listOfTermsLists =
                    ListExtra.groupsOf sqrtOfRequiredTerms model.terms

                termColumn term =
                    div
                        [ class "col" ]
                        [ text term ]

                termRow termList =
                    div
                        [ class "row" ]
                        (List.map
                            termColumn
                            termList
                        )

                columnsForEachRow =
                    List.repeat
                        sqrtOfRequiredTerms
                        (div
                            [ class "col" ]
                            [ text "term" ]
                        )

                rowsWithColumns =
                    List.repeat
                        sqrtOfRequiredTerms
                        (div
                            [ class "row" ]
                            columnsForEachRow
                        )
            in
            [ h1 [] [ text "Your Card" ]
            , button [ onClick RandomizeTerms ] [ text "Randomize your card" ]
            ]
                ++ List.map termRow listOfTermsLists


termForm : Model -> Html Msg
termForm model =
    form [ onSubmit (AddTerm model.newTerm) ]
        [ div [ class "form-group" ]
            [ label [ for "newTermInput" ] [ text "New Term" ]
            , input
                [ type_ "text"
                , onInput UpdateNewTerm
                , placeholder "Add a new term"
                , id "newTermInput"
                , class "form-control"
                , value model.newTerm
                ]
                []
            ]
        , button [ type_ "submit", class "btn btn-primary", value model.newTerm ] [ text "Add" ]
        ]
