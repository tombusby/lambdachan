module View.Modal exposing (view)

import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)


-- | A simple confirmation modal overlay.
-- Renders only when `visible` is True.
view :
    { visible : Bool
    , message : String
    , onConfirm : msg
    , onCancel : msg
    }
    -> Html msg
view { visible, message, onConfirm, onCancel } =
    if not visible then
        text ""

    else
        div [ class "modal-overlay" ]
            [ div [ class "modal-box" ]
                [ p [] [ text message ]
                , div [ class "modal-buttons" ]
                    [ button [ onClick onConfirm, class "btn-danger" ] [ text "Delete" ]
                    , text " "
                    , button [ onClick onCancel ] [ text "Cancel" ]
                    ]
                ]
            ]
