module LineChart exposing (viewAsLineChart)

import Axis
import Color exposing (Color)
import Path exposing (Path)
import Scale exposing (ContinuousScale, OrdinalScale)
import Scale.Color
import Shape
import Statistics
import TypedSvg exposing (g, svg, text_)
import TypedSvg.Attributes exposing (class, dy, fill, fontFamily, stroke, textAnchor, transform, viewBox, width, height, x, y)
import TypedSvg.Attributes.InPx exposing (fontSize, strokeWidth, x, y)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (AnchorAlignment(..), Paint(..), Transform(..), em, px)

viewAsLineChart : Float -> Float -> Float -> Float -> Float -> String -> String -> List ( String, List Int ) -> List Int -> ( Float, Float ) -> Float -> Int -> Svg msg
viewAsLineChart x y w h padding title source model srzX xRangeDef yDef mzero =
    let
        values : List (String, List Int) -> List (List Float)
        values i =
            List.map (\a -> List.map toFloat <| Tuple.second a) i

        colorScale : OrdinalScale String Color
        colorScale =
            List.map Tuple.first model
                |> Scale.ordinal Scale.Color.category10

        color : String -> Color
        color =
            Scale.convert colorScale >> Maybe.withDefault Color.black


--        lastWithDefault : a -> List a -> a
        lastWithDefault d ds =
            List.reverse ds
                |> List.head
                |> Maybe.withDefault d 

        xScale : ContinuousScale Float
        xScale =
            srzX
                |> List.map toFloat
                |> Statistics.extent
                |> Maybe.withDefault xRangeDef
                |> Scale.linear ( 0, w - 2 * padding )

        yScale : ContinuousScale Float
        yScale =
            values model
                |> List.map (List.maximum >> Maybe.withDefault yDef)
                |> List.maximum
                |> Maybe.withDefault yDef
                |> (\b -> ( 0, b ))
                |> Scale.linear ( h - 2 * padding, 0 )
                |> Scale.nice 4

        lineGenerator : ( Int, Int ) -> Maybe ( Float, Float )
        lineGenerator ( x1, y1 ) =
            Just ( Scale.convert xScale (toFloat x1), Scale.convert yScale (toFloat y1) )

        line : List Int -> Path
        line vals =
            List.map2 (\a b -> (a, b)) srzX (vals)
                |> List.map lineGenerator
                |> Shape.line Shape.linearCurve
    in
    svg [ TypedSvg.Attributes.InPx.x x, TypedSvg.Attributes.InPx.y y, width (px w), height (px h), viewBox 0 0 w h ]
        [ g [ transform [ Translate padding padding ], class [ "series" ] ]
            (List.map
                (\( label, vals ) ->
                    Path.element (line vals)
                        [ stroke <| Paint <| color label
                        , strokeWidth 3
                        , fill PaintNone
                        ]
                )
                model
            )
        , g [ fontFamily [ "sans-serif" ], fontSize 10 ]
            (List.map
                (\( label, vals ) ->
                    g
                        [ transform
                            [ Translate (w - padding - 100) (padding - 5 + Scale.convert yScale (toFloat <| lastWithDefault mzero <| vals))
                            ]
                        ]
                        [ text_ [ fill (Paint (color label)) ] [ TypedSvg.Core.text label ] ]
                )
                model
            )
        , g [ transform [ Translate (w / 2) padding] ]
            [ text_ [ fontFamily [ "sans-serif" ], fontSize 20, textAnchor AnchorMiddle ] [ TypedSvg.Core.text title ]
            , text_ [ fontFamily [ "sans-serif" ], fontSize 10, textAnchor AnchorMiddle, dy (TypedSvg.Types.em 1) ] [ TypedSvg.Core.text source ]
            ]
        , g [ transform [ Translate (padding - 1) (h - padding) ] ]
            [ Axis.bottom [ Axis.tickCount 10 ] xScale ]
        , g [ transform [ Translate (w - padding) padding ] ]
            [ Axis.left [ Axis.ticks (List.map (lastWithDefault (toFloat mzero)) (values model)) ] yScale
            , text_ [ fontFamily [ "sans-serif" ], fontSize 10, TypedSvg.Attributes.InPx.x 5, TypedSvg.Attributes.InPx.y 5 ] [ TypedSvg.Core.text "Occurences" ]
            ]
        ]
