module Model exposing (Flags, Model, Msg(..), WizardStep(..))

import Responsive exposing (Container, ScreenSize(..))


type Msg
    = AddTerm
    | RemoveTerm String
    | UpdateNewTerm String
    | RandomizeTerms
    | RandomList (List String)
    | SetScreenSize ScreenSize Int


type WizardStep
    = AddingTerms
    | GeneratingCard


type alias Model =
    { newTerm : String
    , terms : List String
    , termsRequired : Int
    , feedback : Maybe String
    , wizardStep : WizardStep
    , uiContainer : Container
    }


type alias Flags =
    { width : Int
    , height : Int
    }
