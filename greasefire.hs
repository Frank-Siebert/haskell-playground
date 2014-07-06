-- greasefire
-- http://codegolf.stackexchange.com/questions/32024/i-am-passive-aggressive-and-want-to-insult-my-boss-without-him-finding-out/32029#32029
--import Control.Applicative ((<$>))
example = "Dear Boss, how are things? It has come to my attention that I received all the blame for the mishap last Friday. Not just the majority of it. Every last bit of it. Is it wrong for me to think that the rest of the team were at least in part responsible? After all, all six of us were involved from the get-go. Not that I think I should stand without blame. Not at all. All I'm saying is this: I do my best. I try hard. I improve constantly. And I am constantly taking responsibility. Generally speaking, I am very okay with taking full responsibility for my actions. But after this spring, it seems I get more than I deserve. Remember the Flakenhauser contract? Everything went down just about as smooth as one could have hoped. Or so it seemed at first. It was just at the very last minute that things fell apart. All of the team agreed that it was more akin to a freak accident than sloppy planning or mismanaged resources. Still, I - alone - took blame for it. Even though I said nothing then, my tolerance level for taking the blame took a serious dent then. From that point on, I have felt it necessary to always try twice as hard, just to escape scrutiny. And still, here we are again. In spite of all my accomplishments. Right where we always seem to end up these days. Every single project. It's becoming unbearable."

c :: String -> String
-- first parameter is delimiter, one
f :: String -> String -> [String] -> Maybe String

c x=maybe x id$f"""DIE IN A GREASE FIRE".words$x
f d[]y=Just$' ':unwords y
f d(' ':j)y=f('\n':d)j y
f d z@(h:j)(x:y)=if head x==h then fmap((d++x)++)(f"\n"j y)else fmap((' ':x)++)(f d z y)
f _ _ _=Nothing
main=interact c
