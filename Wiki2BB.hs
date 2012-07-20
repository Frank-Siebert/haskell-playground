import List

convert :: String -> String
convert "" = ""
convert s = case firstList convertTokens s of
              Just l -> process l s
              Nothing -> (head s) : convert (tail s)
-- scheiße. irgendwas mit any?

process :: [String] -> String -> String
process l s = (l!!1) ++ process' (tail . tail $ l) (drop (length (head l)) s) where
              process' _ "" = ""
              process' [] s = convert s
              process' l s = if isPrefixOf (head l) s
                               then process l s
                               else (head s):process' l (tail s)


convertTokens = [["====","[i]","====","[/i]"],
                 ["===","[size=3][b][u]","===","[/u][/b][/size]"],
                 ["==","[size=4][b][u]","==","[/u][/b][/size]"],
                 ["[","[url="," ","]","]","[/url]"],
                 ["'''","[b]","'''","[/b]"],
                 ["''","[i]","''","[/i]"]]

firstList :: [[String]] -> String -> Maybe [String]
firstList [] _ = Nothing
firstList (x:xs) s = if isPrefixOf (head x) s 
                       then Just x
                       else firstList xs s
                     
main :: IO ()
main = interact convert                     