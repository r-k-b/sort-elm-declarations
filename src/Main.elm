module Main exposing (main)

import Browser
import Dummy
import Elm.Parser
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Node as Node exposing (Node(..))
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Parser exposing (DeadEnd)
import Review


main : Program () Model Msg
main =
    Browser.element
        { init =
            \() ->
                ( { denormalizedOutput =
                        let
                            preppedSrc =
                                Dummy.data |> srcPrepper
                        in
                        preppedSrc |> Elm.Parser.parseToFile |> renderOutput preppedSrc
                  , inputSource = Dummy.data
                  }
                , Cmd.none
                )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { denormalizedOutput : String
    , inputSource : String
    }


type Msg
    = InputChanged String


srcPrepper : String -> String
srcPrepper src =
    "module Dummy exposing (..)\n\n" ++ src


renderOutput : String -> Result (List DeadEnd) File -> String
renderOutput preppedInputSource result =
    case result of
        Ok file ->
            file.declarations
                |> List.sortBy (Node.value >> declName)
                |> List.map (getOriginal preppedInputSource)
                |> String.join "\n\n\n"

        Err deadEnds ->
            "Parsing failed: " ++ Parser.deadEndsToString deadEnds


getOriginal : String -> Node Declaration -> String
getOriginal inputSource (Node originalRange _) =
    Review.sourceExtractInRange originalRange inputSource


declName : Declaration -> String
declName declaration =
    case declaration of
        AliasDeclaration typeAlias ->
            "~" ++ Node.value typeAlias.name

        CustomTypeDeclaration type_ ->
            "!" ++ Node.value type_.name

        Destructuring _ _ ->
            "tba:destructuring decl"

        FunctionDeclaration function ->
            function.declaration |> Node.value |> .name |> Node.value

        InfixDeclaration _ ->
            "tba:infix decl"

        PortDeclaration signature ->
            signature.name |> Node.value


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputChanged newVal ->
            let
                preppedSrc =
                    Debug.log "zzz x" (srcPrepper newVal)
            in
            ( { model
                | inputSource = newVal
                , denormalizedOutput =
                    preppedSrc
                        |> Elm.Parser.parseToFile
                        |> renderOutput preppedSrc
              }
            , Cmd.none
            )


{-| Wrapping in a `let` block allows intellij-elm to collapse the code properly;
handy for large json.
-}
view : Model -> H.Html Msg
view model =
    H.div [ HA.class "taGrid" ]
        [ H.textarea [ HE.onInput InputChanged ] [ H.text model.inputSource ]
        , H.textarea [] [ H.text model.denormalizedOutput ]
        ]
