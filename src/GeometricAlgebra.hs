module GeometricAlgebra where
import qualified Data.Map as Map
import qualified Data.Set as Set

type Value = Double
data Vector = Vector Char deriving (Eq, Ord)
data Blade = Blade (Set.Set Vector) deriving (Eq, Ord)
data Multivector = Multivector (Map.Map Blade Value) deriving (Eq)

unitBlade :: Blade
unitBlade = Blade Set.empty

fromValue :: Value -> Multivector
fromValue value = fromBladeValue unitBlade value

fromBladeValue :: Blade -> Value -> Multivector
fromBladeValue blade value = Multivector $ Map.fromList[(blade, value)]

fromBlade :: Blade -> Multivector
fromBlade blade = fromBladeValue blade 1

fromVector :: Vector -> Multivector
fromVector x = fromBlade $ Blade $ Set.fromList [x]

fromVectors :: [Vector] -> Multivector
fromVectors vectors = foldr (*) 1 $ map fromVector vectors

fromString :: String -> Multivector
fromString vectors = fromVectors $ map Vector vectors

instance Show Vector where
    show (Vector x) = [x]

instance Show Blade where
    show (Blade vectors) =
        foldr (++) "" $ map show $ Set.toList vectors

instance Show Multivector where
    show (Multivector values) =
        let filtered = filter
                (\(_, value) -> value /= 0.0)
                $ Map.toList values
        in case filtered of
            [] -> show "0.0"
            values' -> foldr1
                (\x y -> x ++ " + " ++ y)
                $ map
                    (\(blade, value) -> show value ++ show blade)
                    values'

instance Num Multivector where
    (Multivector values) + (Multivector values') =
        Multivector $ Map.fromList $ map
            (\blade -> (blade,
                (Map.findWithDefault 0 blade values) +
                (Map.findWithDefault 0 blade values')))
            $ Set.toList $ Set.union (Map.keysSet values) (Map.keysSet values')

    (Multivector values) * (Multivector values') =
        let combine :: [Vector] -> [Vector] -> ([Vector], Bool)
            combine [] x     = (x, False)
            combine x []     = (x, False)
            combine [x] (y:ys)
                | x == y = (ys, False)
                | x < y = (x:y:ys, False)
                | x > y =
                    let (combined, reversed) = combine [y, x] ys
                    in (combined, not reversed)
            combine (x:xs) y =
                let (combined, reversed) = combine xs y
                    (combined', reversed') = combine [x] combined
                in (combined', reversed `xor` reversed')

            xor :: Bool -> Bool -> Bool
            xor x y = x && (not y) || (not x) && y

        in foldr (+) 0 $ map
            (\[(Blade vectors, value), (Blade vectors', value')] ->
                let (combined, reversed) =
                        combine (Set.toList vectors) (Set.toList vectors')
                in fromBladeValue
                    (Blade $ Set.fromList combined)
                    $ value * value' * (if reversed then -1 else 1))
            $ sequence [(Map.toList values), (Map.toList values')]

    negate (Multivector values) =
        Multivector $ Map.fromList $ map
            (\(blade, value) -> (blade, negate value))
            $ Map.toList values

    fromInteger i = fromValue $ fromInteger i

    -- Invalid
    abs x = x
    signum _ = 1




