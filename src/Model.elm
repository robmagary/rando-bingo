module Model exposing (Flags, Model, Msg(..), Term(..), WizardStep(..), unwrapTerm)

import Responsive exposing (Container, ScreenSize(..))


type alias Model =
    { newTerm : String
    , terms : List Term
    , cardColumns : Int
    , feedback : Maybe String
    , wizardStep : WizardStep
    , uiContainer : Container
    , cards : List (List Term)
    }


type Term
    = Term String


unwrapTerm : Term -> String
unwrapTerm (Term string) =
    string

type Msg
    = AddTerm
    | RemoveTerm Term
    | UpdateNewTerm String
    | NewRandomizeTerms
    | RandomList (List Term)
    | SetScreenSize ScreenSize Int


type WizardStep
    = AddingTerms
    | GeneratingCard


type alias Flags =
    { width : Int
    , height : Int
    }
