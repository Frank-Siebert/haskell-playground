module Solitaire.Display where

import Solitaire.Data

displayHtml :: GameState -> String
displayHtml gs = "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 3.2 Final//EN\">" ++
                 tag "html" ( tag "head" "<style type=\"text/css\"><!-- .card { //border: 2px solid black; height: 75px; max-height: 75px; overflow:hidden;}--></style> ")
                 ++ tag "body" (displayGameState gs)

displayGameState :: GameState -> String
displayGameState (GameState cols undealt) = tag "table" .
                                   tag "tr"  $
                                   concatMap displayCol cols

displayCol :: Column -> String
displayCol (Column hidden open) = displayCards (reverse hidden) ++ displayCards (reverse open)

displayCards :: Cards -> String
displayCards cards = concatMap (tag "td" . displayCard) cards

displayCard :: Card -> String
displayCard c = "<div class=\"card\"><img src=\"file://localhost/K:/protected/poker/"++name c++".png\"></div>"

tag :: String -> String -> String
tag t payload = "<"++t++">"++payload++"</"++t++">\n"

name :: Card -> String
name (rank :/ suit) = show rank ++ " of " ++ show suit
