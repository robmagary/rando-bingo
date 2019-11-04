module View exposing (elementView)

import BrandColors exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import List.Extra as ListExtra
import Model exposing (Model, Msg(..), Term, WizardStep(..), unwrapTerm)
import Responsive exposing (Container, ScreenSize(..), containerWidth)
import SvgGraphics exposing(logo)
import UiHelpers exposing (onInputEnterKey)


elementView : Model -> Html Msg
elementView model =
    layout
        [ Background.color rgbNuetral ]
    <|
        column
            [ containerWidth model.uiContainer
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
                    (html logo)
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
            column
                [ width fill
                , spacing 40
                ] <|
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
                        { onPress = Just NewRandomizeTerms
                        , label = text "Add a Card"
                        }
                    ]
                ]
                    ++ List.map
                        (\ terms ->
                            bingoCards model terms
                        )
                        model.cards


termInput : Model -> Element Msg
termInput model =
    let
        bottomBorder =
            { bottom = 2
            , left = 0
            , right = 0
            , top = 0  
            }
    in
    el [ width fill ]
        (Input.text
            [ onInputEnterKey AddTerm
            , Border.widthEach bottomBorder
            , Border.solid
            , Border.rounded 0
            , Border.color rgbFontMain
            , Background.color transparentRgba
            ]
            { onChange = UpdateNewTerm
            , text = model.newTerm
            , placeholder = Just <| Input.placeholder [] (text "Enter a term")
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
                            , label = text <| unwrapTerm term
                            }
                )
                model.terms
        )


bingoCards : Model -> List Term -> Element Msg
bingoCards model terms =
    let
        cardColumns =
            model.cardColumns
        
        containerWidthFloat =
            toFloat model.uiContainer.width

        termSpacing =
            ( containerWidthFloat / ( toFloat cardColumns ) ) / 3
                |> round

        listOfTermsLists =
            ListExtra.groupsOf cardColumns terms

    in
    
        row [ width fill ]
            [ column [ width fill ] <|
                List.indexedMap
                    (\index term ->
                        termRow termSpacing index term
                    )
                    listOfTermsLists
            ]


termRow : Int -> Int -> List Term -> Element Msg
termRow termSpacing rowIndex termList =
    let
        cardColumns =   
            List.length termList
        
        centerColumnIndex =
            ((cardColumns + 1) // 2) - 1
        
        isCenterRow =
            centerColumnIndex == rowIndex
    in
    row
        [ width fill
        ]
        (List.indexedMap
            (\columnIndex term ->
                let
                    isCenterColumn =
                        centerColumnIndex == columnIndex && isCenterRow
                in
                termColumn isCenterColumn termSpacing term
            )
            termList
        )


termColumn : Bool -> Int -> Term -> Element Msg
termColumn isCenterColumn termSpacing term =
    let
        termFontSize =
            ( toFloat termSpacing) / 3
                |> round
        
        termOrFree =
            if isCenterColumn then
                "FREE"
            else
                unwrapTerm term
    in
    column
        [ width fill
        , paddingXY 0 termSpacing
        , Background.color white
        , Border.color <| darkenColor rgbHighlight2 0.75
        , Border.width 1
        ]
        [ paragraph
            [ width fill
            , Font.center
            , Font.size termFontSize
            ]
            [ el
                [ width fill
                
                ]
                (text termOrFree)
            ]

        ]