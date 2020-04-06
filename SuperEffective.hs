-- http://codegolf.stackexchange.com/questions/55823/its-super-effective
{-

              Defending type
               (same order)

   Normal   222221201222222222
   Fighting 421124104222214241
A  Flying   242221421224122222
t  Poison   222111210224222224
t  Ground   220424124421422222
a  Rock     214212421422224222
c  Bug      211122211124242241
k  Ghost    022222242222242212
i  Steel    222224221112124224
n  Fire     222221424114224122
g  Water    222244222411222122
   Grass    221144121141222122
T  Electric 224202222241122122
y  Psychic  242422221222212202
p  Ice      224242221114221422
e  Dragon   222222221222222420
   Dark     212222242222242211
   Fairy    242122221122222442
-}

parse :: String -> (String, [String])
parse x = let (att,_:_:def) = span(/=',')x
              (d1,d2) = span(/='/')def
           in (att,if d2==""then[d1]else[d1,tail d2])

s2i :: String -> Int
