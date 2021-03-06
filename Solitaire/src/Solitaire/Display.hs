module Solitaire.Display where

import Solitaire.Data

displayHtml :: String -> String
displayHtml inner = "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 3.2 Final//EN\">" ++
                 tag "html" ( tag "head" "<style type=\"text/css\"><!-- .card { //border: 2px solid black; height: 75px; max-height: 75px; overflow:hidden;}\n\
                 \td { vertical-align:top} --></style> ")
                 ++ tag "body" inner

displayGameState :: GameState -> String
displayGameState (GameState cols undealt) = tag "table" .
                                   tag "tr"  $
                                   concatMap displayCol cols

displayCol :: Column -> String
displayCol (Column hidden open) = tag "td"  $ displayCards (reverse hidden) ++ displayCards (reverse open)

displayCards :: Cards -> String
displayCards cards = concatMap displayCard cards

displayCard :: Card -> String
displayCard c = "<div class=\"card\"><img src=\"img/"++name c++".png\"></div>"

tag :: String -> String -> String
tag t payload = "<"++t++">"++payload++"</"++t++">\n"

name :: Card -> String
name (rank :/ suit) = show rank ++ " of " ++ show suit
