module Main exposing (Model, Msg(..), init, main, update)

import Array exposing (Array, empty, push, toList)
import BrandColors exposing (..)
import Browser
import Data exposing (randomWordList)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import List.Extra as ListExtra
import Random
import Random.List as RandomList
import UiHelpers exposing (onInputEnterKey)


main =
    Browser.element { init = init, update = update, view = elementView, subscriptions = subscriptions }



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


init : () -> ( Model, Cmd Msg )
init _ =
    let
        initialModel =
            Model "" randomWordList 25 Nothing AddingTerms
    in
    ( setWizardStep initialModel, Cmd.none )


setWizardStep : Model -> Model
setWizardStep modelToSet =
    let
        numberOfTerms =
            List.length modelToSet.terms

        termsRequired =
            modelToSet.termsRequired

        wizardStep =
            if numberOfTerms >= termsRequired then
                GeneratingCard

            else
                AddingTerms
    in
    { modelToSet | wizardStep = wizardStep }



-- UPDATE


type Msg
    = AddTerm
    | RemoveTerm String
    | UpdateNewTerm String
    | RandomizeTerms
    | RandomList (List String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddTerm ->
            if termIsUnique model then
                let
                    termInModel =
                        { model
                            | newTerm = ""
                            , terms = model.newTerm :: model.terms
                            , feedback = Nothing
                        }

                    updatedModel =
                        setWizardStep termInModel
                in
                ( updatedModel, Cmd.none )

            else
                ( { model | feedback = Just "Each term can only be used once." }, Cmd.none )

        UpdateNewTerm updatedTerm ->
            ( { model
                | newTerm = updatedTerm
                , feedback = Nothing
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
-- role : String -> Attribute msg
-- role roleValue =
--     attribute "role" roleValue
-- VIEW


elementView : Model -> Html Msg
elementView model =
    layout
        [ Background.color rgbNuetral ]
    <|
        column
            [ width (fill |> maximum 800)
            , centerX
            ]
            [ wrappedRow
                [ width fill ]
                [ el
                    [ Font.color rgbFontMain
                    , Font.size 50
                    , Font.bold
                    , paddingXY 0 20
                    ]
                    (text "Rando-Bingo")
                ]
            , feedbackElmnt model.feedback
            , bingoCardWizard model
            ]


feedbackElmnt : Maybe String -> Element msg
feedbackElmnt maybeFeedback =
    case maybeFeedback of
        Just feedback ->
            wrappedRow
                [ width fill ]
                [ el
                    [ width fill
                    , Background.color rgbPrimary
                    , Border.color <| darkenColor rgbPrimary 0.75
                    , Border.width 1
                    , Border.rounded 2
                    , padding 10
                    ]
                  <|
                    text feedback
                ]

        Nothing ->
            none


bingoCardWizard : Model -> Element Msg
bingoCardWizard model =
    case model.wizardStep of
        AddingTerms ->
            column
                [ width fill
                , spacing 10
                , paddingXY 0 20
                ]
                [ termInput model, termsList model ]

        --
        GeneratingCard ->
            bingoCards model


termInput : Model -> Element Msg
termInput model =
    el [ width fill ]
        (Input.text
            [ onInputEnterKey AddTerm ]
            { onChange = UpdateNewTerm
            , text = model.newTerm
            , placeholder = Nothing
            , label = Input.labelHidden "Add a new term"
            }
        )


termsList : Model -> Element Msg
termsList model =
    wrappedRow
        [ width fill
        , spacingXY 20 10
        ]
        (el [] (text "Your Terms:")
            :: List.map
                (\term ->
                    el [] <|
                        Input.button
                            []
                            { onPress = Just (RemoveTerm term)
                            , label = text term
                            }
                )
                model.terms
        )


bingoCards : Model -> Element Msg
bingoCards model =
    let
        sqrtOfRequiredTerms =
            model.termsRequired
                |> toFloat
                |> sqrt
                |> round

        listOfTermsLists =
            ListExtra.groupsOf sqrtOfRequiredTerms model.terms

        termColumn term =
            column
                [ width fill ]
                [ el
                    [ width fill
                    , Font.center
                    , paddingXY 0 30
                    , Background.color white
                    , Border.color <| darkenColor rgbHighlight2 0.75
                    , Border.width 1
                    ]
                    (text term)
                ]

        termRow termList =
            row
                [ width fill
                ]
                (List.map
                    termColumn
                    termList
                )
    in
    column
        [ width fill ]
        [ row
            [ width fill
            , paddingXY 0 20
            ]
            [ el [ alignLeft ] (text "Your Card")
            , Input.button
                [ alignRight
                , Font.color white
                , Background.color rgbPrimary
                , Border.color <| darkenColor rgbPrimary 0.75
                , Border.width 1
                , Border.rounded 2
                , paddingXY 5 10
                ]
                { onPress = Just RandomizeTerms
                , label = text "Randomize"
                }
            ]
        , row [ width fill ]
            [ column [ width fill ] <|
                List.map termRow listOfTermsLists
            ]
        ]
