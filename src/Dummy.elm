module Dummy exposing (data)

import Json.Encode as JE


data =
    """cDeclaration : String -> String
cDeclaration a =
   a
       ++ "3"


bDeclaration =
   let
       x =
           "2"
   in
   x


{-| some docs -}
aDeclaration =
   "1"
"""
