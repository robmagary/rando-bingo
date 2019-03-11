module Main exposing (Model, Msg(..), init, main, update, view)

import Array exposing (Array, empty, push, toList)
import Browser
import Html exposing (Attribute, Html, button, div, form, input, label, li, ol, p, text)
import Html.Attributes exposing (attribute, class, for, id, placeholder, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type WizardStep
    = AddingTerms
    | GeneratingCard


type alias Model =
    { newTerm : String
    , terms : List String
    , feedback : Maybe String
    , wizardStep : WizardStep
    }


init : Model
init =
    Model "" [] Nothing AddingTerms



-- UPDATE


type Msg
    = AddTerm String
    | RemoveTerm String
    | UpdateNewTerm String


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddTerm newTerm ->
            if termIsUnique model then
                { model
                    | newTerm = ""
                    , terms = newTerm :: model.terms
                    , feedback = Nothing
                }

            else
                { model | feedback = Just "Each term can only be used once." }

        UpdateNewTerm updatedTerm ->
            let
                wizardStep =
                    case List.length model.terms of
                        24 ->
                            GeneratingCard

                        _ ->
                            AddingTerms
            in
            { model
                | newTerm = updatedTerm
                , feedback = Nothing
                , wizardStep = wizardStep
            }

        RemoveTerm existingTerm ->
            let
                updatedTerms =
                    List.filter (\term -> not (term == existingTerm)) model.terms
            in
            { model | terms = updatedTerms }


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
    div [ class "container" ]
        ([ feedbackDiv model
         , p [ class "lead" ] [ text "Add 24 terms to create your custom bingo card." ]
         ]
            ++ bingoCardWizard model
        )


feedbackDiv : Model -> Html msg
feedbackDiv model =
    case model.feedback of
        Just feedback ->
            div [ class "alert alert-primary", role "alert" ] [ text feedback ]

        Nothing ->
            div [] []


bingoCardWizard : Model -> List (Html Msg)
bingoCardWizard model =
    case model.wizardStep of
        AddingTerms ->
            [ ol [] (termsList model.terms)
            , termForm model
            ]

        GeneratingCard ->
            [ text "My cool card" ]


termsList : List String -> List (Html Msg)
termsList terms =
    List.map termItem terms


termItem : String -> Html Msg
termItem term =
    li []
        [ button
            [ onClick (RemoveTerm term)
            , class "btn btn-link"
            ]
            [ text term ]
        ]


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
