module Model exposing (Flags, Model, Msg(..), WizardStep(..))

import Responsive exposing (ScreenSize(..))


type Msg
    = AddTerm
    | RemoveTerm String
    | UpdateNewTerm String
    | RandomizeTerms
    | RandomList (List String)
    | SetScreenSize ScreenSize


type WizardStep
    = AddingTerms
    | GeneratingCard


type alias Model =
    { newTerm : String
    , terms : List String
    , termsRequired : Int
    , feedback : Maybe String
    , wizardStep : WizardStep
    , screenSize : ScreenSize
    }


type alias Flags =
    { width : Int
    , height : Int
    }
