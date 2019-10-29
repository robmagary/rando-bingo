module Main exposing (init, main, update)

import Array exposing (Array, empty, push, toList)
import Browser
import Browser.Events exposing (onResize)
import Data exposing (randomWordList)
import Model exposing (Flags, Model, Msg(..), WizardStep(..))
import Random
import Random.List as RandomList
import Responsive exposing (ScreenSize(..), containerWidth, widthToScreenSize)
import View exposing (elementView)


main =
    Browser.element { init = init, update = update, view = elementView, subscriptions = subscriptions }


updateScreenSize : Int -> Int -> Msg
updateScreenSize width _ =
    SetScreenSize (widthToScreenSize width)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ onResize updateScreenSize ]


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        initialScreenSize =
            widthToScreenSize flags.width

        initialModel =
            Model "" randomWordList 25 Nothing AddingTerms initialScreenSize
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

        SetScreenSize newScreenSize ->
            ( { model | screenSize = newScreenSize }, Cmd.none )



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
