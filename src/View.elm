module View exposing (elementView)

import BrandColors exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import List.Extra as ListExtra
import Model exposing (Model, Msg(..), WizardStep(..))
import Responsive exposing (ScreenSize(..), containerWidth)
import UiHelpers exposing (onInputEnterKey)


elementView : Model -> Html Msg
elementView model =
    layout
        [ Background.color rgbNuetral ]
    <|
        column
            [ containerWidth model.screenSize
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
            , screenSizeSpy model.screenSize
            , feedbackElmnt model.feedback
            , bingoCardWizard model
            ]


screenSizeSpy : ScreenSize -> Element msg
screenSizeSpy screenSize =
    let
        screenSizeText =
            case screenSize of
                PhoneWidth ->
                    "Phone"

                PhabletWidth ->
                    "Phablet"

                TabletWidth ->
                    "Tablet"

                LaptopWidth ->
                    "Laptop"

                DesktopWidth ->
                    "Desktop"

                LargeDesktopWidth ->
                    "LargeDesktop"
    in
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
            text ("Screen Size:" ++ screenSizeText)
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