module Responsive exposing (Container, ScreenSize(..), containerWidth, widthToScreenSize)

import Element exposing (..)


type ScreenSize
    = PhoneWidth
    | PhabletWidth
    | TabletWidth
    | LaptopWidth
    | DesktopWidth
    | LargeDesktopWidth


type alias Container =
    { screenSize : ScreenSize
    , width : Int
    }


widthToScreenSize : Int -> ScreenSize
widthToScreenSize width =
    if width <= 576 then
        PhoneWidth

    else if width <= 768 then
        PhabletWidth

    else if width <= 992 then
        TabletWidth

    else if width <= 1200 then
        LaptopWidth

    else if width <= 1600 then
        DesktopWidth

    else
        LargeDesktopWidth


containerWidth : Container -> Attribute msg
containerWidth container =
    case container.screenSize of
        PhoneWidth ->
            width fill

        PhabletWidth ->
            width (fill |> maximum 576)

        TabletWidth ->
            width (fill |> maximum 768)

        LaptopWidth ->
            width (fill |> maximum 992)

        DesktopWidth ->
            width (fill |> maximum 1200)

        LargeDesktopWidth ->
            width (fill |> maximum 1600)
