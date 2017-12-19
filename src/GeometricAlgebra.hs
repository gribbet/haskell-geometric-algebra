module GeometricAlgebra where
import qualified Data.Map as Map
import qualified Data.Set as Set

data Vector v = Vector v deriving (Eq, Ord)
data Blade v = Blade (Set.Set (Vector v)) deriving (Eq, Ord)
data Multivector v x = Multivector (Map.Map (Blade v) x)

unitBlade :: Blade v
unitBlade = Blade Set.empty

fromValue :: (Ord v, Num x) => x -> Multivector v x
fromValue value = fromBladeValue unitBlade value

fromBladeValue :: (Ord v, Num x) => Blade v -> x -> Multivector v x
fromBladeValue blade value = Multivector $ Map.fromList[(blade, value)]

fromBlade :: Ord v => Blade v -> Multivector v Int
fromBlade blade = fromBladeValue blade 1

fromVector :: Ord v => Vector v -> Multivector v Int
fromVector x = fromBlade $ Blade $ Set.fromList [x]

fromVectors :: Ord v => [Vector v] -> Multivector v Int
fromVectors vectors = foldr (*) 1 $ map fromVector vectors

fromString :: String -> Multivector Char Int
fromString vectors = fromVectors $ map Vector vectors

instance Show v => Show (Vector v) where
    show (Vector a) = show a

instance Show v => Show (Blade v) where
    show (Blade vectors) =
        foldr (++) "" $ map show $ Set.toList vectors

instance (Show v, Num x, Eq x, Show x) => Show (Multivector v x) where
    show (Multivector values) =
        let filtered = filter
                (\(_, value) -> value /= 0)
                $ Map.toList values
        in case filtered of
            [] -> show (0::Int)
            values' -> foldr1
                (\x y -> x ++ " + " ++ y)
                $ map
                    (\(blade, value) -> show value ++ show blade)
                    values'

instance (Ord v, Num x) => Num (Multivector v x) where
    (Multivector values) + (Multivector values') =
        Multivector $ Map.fromList $ map
            (\blade -> (blade,
                (Map.findWithDefault 0 blade values) +
                (Map.findWithDefault 0 blade values')))
            $ Set.toList $ Set.union (Map.keysSet values) (Map.keysSet values')

    (Multivector values) * (Multivector values') =
        let combine :: Ord v => [Vector v] -> [Vector v] -> ([Vector v], Bool)
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

instance (Show v, Num x, Eq x, Show x) => Eq (Multivector v x) where
    a == b = (show a) == (show b)




