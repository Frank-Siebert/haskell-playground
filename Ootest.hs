import Control.Monad (forM_)
-- tries some object oriented stuff

type Point = (Int,Int)

data Widget = Widget {
         draw :: IO () ,
         translate :: Point -> Widget
    }

(x1,y1) -+- (x2,y2) = (x1+x2,y2+y2)

data Rectangle = Rectangle {
         upperLeft :: Point,
         lowerRight :: Point
         } deriving Show

mkRectangleWidget :: Rectangle -> Widget
mkRectangleWidget rect = Widget {
         draw = print rect,
         translate = \offset ->  mkRectangleWidget (Rectangle {
--         translate offset = mkRectangleWidget (Rectangle {
             upperLeft  = upperLeft  rect -+- offset,
             lowerRight = lowerRight rect -+- offset
             })
        }

data Circle = Circle Point Int deriving Show

mkCircleWidget c@(Circle center radius) = Widget {
          draw = print c,
          translate = \offset -> mkCircleWidget (Circle (center -+- offset) radius)
}

widgets :: [Widget]
widgets = [mkRectangleWidget (Rectangle (0,0) (40,25)), mkCircleWidget (Circle (20,12) 7)]

main = do mapM_ draw widgets
          let widgets' = map (flip translate (1,1)) widgets
          putStrLn "Now the translation:"
          forM_ widgets' draw
