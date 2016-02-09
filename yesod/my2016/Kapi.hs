import Data.Int (Int8)

type Quality = Int8

data Good = Good { singular :: String
                 , plural   :: String
                 , unit     :: String
                 }
apples :: Good
apples = Good { singular = "Apfel", plural = "Äpfel", unit = "Kilogramm" }

data Product = Product Good Quality

type Money = Integer
type Quantity = Integer
data Posten = Posten Product Quantity
-- Stock
type Inventory = [Posten]

type Factory = String
type Shop = String

-- simple for 1 : 1 : 1 relations...
data Production = Production { product :: Good
                             , educt   :: [Good]
                             , place   :: Factory
                             , cost    :: Money
                             }

--
