import Solitaire.Data
import Solitaire.Solitaire
import Solitaire.Display
import Control.Monad((>=>))


followups :: [GameState]
followups = let (GameState t ud) = gs in [(GameState t' ud) | t' <- iterateM moves 2 t]

showFollowups :: String
showFollowups = concatMap (\x -> tag "h2" "Followup" ++ displayGameState x) followups

iterateM :: (Monad m) => (a -> m a) -> Int -> a -> m a
iterateM k 0 = return
iterateM k 1 = k
iterateM k n = k >=> iterateM k (n-1)

main :: IO ()
main = do
    putStr $ displayHtml $ (displayGameState gs) ++showFollowups
