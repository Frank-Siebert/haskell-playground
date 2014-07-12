import Solitaire.Data
import Solitaire.Solitaire
import Solitaire.Display


followups :: [GameState]
followups = let (GameState t ud) = gs in [(GameState t' ud) | t' <- moves t]

showFollowups = concatMap (\x -> tag "h2" "Followup" ++ displayGameState x) followups

main :: IO ()
main = do
    putStr $ displayHtml $ (displayGameState gs) ++showFollowups
