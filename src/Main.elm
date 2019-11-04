module Main exposing (init, main, update)

import Array exposing (Array, empty, push, toList)
import Browser
import Browser.Events exposing (onResize)
import Data exposing (randomWordList)
import Model exposing (Flags, Model, Msg(..), Term(..), WizardStep(..))
import Random
import Random.List as RandomList
import Responsive exposing (Container, ScreenSize(..), containerWidth, widthToScreenSize)
import View exposing (elementView)


main =
    Browser.element { init = init, update = update, view = elementView, subscriptions = subscriptions }


updateScreenSize : Int -> Int -> Msg
updateScreenSize width _ =
    SetScreenSize (widthToScreenSize width) width


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ onResize updateScreenSize ]


randomTermsList : List String -> List Term
randomTermsList wordList =
    List.map
        (\word ->
            Term word
        )
        wordList


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        initialScreenSize =
            widthToScreenSize flags.width
        
        uContainer =
            Container initialScreenSize flags.width

        initialModel =
            Model "" (randomTermsList randomWordList) 5 Nothing AddingTerms uContainer []
    in
    ( setWizardStep initialModel, Cmd.none )


setWizardStep : Model -> Model
setWizardStep modelToSet =
    let
        numberOfTerms =
            List.length modelToSet.terms

        termsRequired =
            modelToSet.cardColumns * modelToSet.cardColumns

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
                    newTerm =
                        Term model.newTerm
                    
                    termInModel =
                        { model
                            | newTerm = ""
                            , terms = newTerm :: model.terms
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

        NewRandomizeTerms ->
            ( model, generateRandomList model.terms )

        RandomList randomizedTerms ->
            ( { model | cards = randomizedTerms :: model.cards }, Cmd.none )

        SetScreenSize newScreenSize width ->
            (
                { model
                    | uiContainer = ( Container newScreenSize width )
                }
                , Cmd.none
            )



-- UPDATE HELPERS


shuffleListGenerator : List Term -> Random.Generator (List Term)
shuffleListGenerator list =
    RandomList.shuffle list


generateRandomList : List Term -> Cmd Msg
generateRandomList terms =
    Random.generate RandomList (shuffleListGenerator terms)


termIsUnique : Model -> Bool
termIsUnique model =
    let
        newTerm =
            Term model.newTerm
    in
    not (List.member newTerm model.terms)
