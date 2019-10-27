module UiHelpers exposing (..)

import Element exposing (..)
import Html.Events as HtmlEvents
import Json.Decode as Decode


onInputEnterKey : msg -> Element.Attribute msg
onInputEnterKey msg =
    htmlAttribute <|
        HtmlEvents.on "keyup"
            (Decode.field "key" Decode.string
                |> Decode.andThen
                    (\key ->
                        if key == "Enter" then
                            Decode.succeed msg

                        else
                            Decode.fail "Not the enter key"
                    )
            )
