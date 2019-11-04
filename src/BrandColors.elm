module BrandColors exposing (..)

import Element exposing (Color, fromRgb, rgba, rgb255, toRgb)


darkenColor : Color -> Float -> Color
darkenColor color factor =
    let
        colorToDarken =
            toRgb color
    in
    fromRgb
        { colorToDarken
            | red = colorToDarken.red * factor
            , green = colorToDarken.green * factor
            , blue = colorToDarken.blue * factor
        }


rgbPrimary : Color
rgbPrimary =
    rgb255 91 46 255


rgbSecondary : Color
rgbSecondary =
    rgb255 38 194 42


rgbNuetral : Color
rgbNuetral =
    rgb255 184 186 184


rgbHighlight1 : Color
rgbHighlight1 =
    rgb255 38 194 42


rgbHighlight2 : Color
rgbHighlight2 =
    rgb255 183 179 199


white : Color
white =
    rgb255 255 255 255


transparentRgba : Color
transparentRgba =
    rgba 0 0 0 0


-- TYPOGRAPHY


rgbFontMain : Color
rgbFontMain =
    rgb255 46 52 54
