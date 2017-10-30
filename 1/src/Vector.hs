module Vector where


data Vector a = Vector2 a a
              | Vector3 a a a


packVector :: Floating a => Vector a -> [a]
packVector (Vector2 x y)   = [y, x]
packVector (Vector3 x y z) = [z, y, x]

unpackVector :: Floating a => [a] -> Vector a
unpackVector [y, x]    = Vector2 x y
unpackVector [z, y, x] = Vector3 x y z
unpackVector _         = error "Vector is either 2D or 3D!"

applyVectorBifunction :: Floating a => ([a] -> [a] -> b) -> (Vector a, Vector a) -> b
applyVectorBifunction f (x, y) = uncurry f $ eqDimen (packVector x) (packVector y)
  where
    eqDimen :: Floating a => [a] -> [a] -> ([a], [a])
    eqDimen x'@[_, _]    y'@[_, _, _] = (0 : x', y'    )
    eqDimen x'@[_, _, _] y'@[_, _]    = (x',     0 : y')
    eqDimen x'           y'           = (x',     y'    )

vectorLength :: Floating a => Vector a -> a
vectorLength = sum . map (^ (2 :: Integer)) . packVector

vectorSum :: Floating a => Vector a -> Vector a -> Vector a
vectorSum = curry $ unpackVector . applyVectorBifunction (zipWith (+))

vectorProduct :: Floating a => Vector a -> Vector a -> a
vectorProduct = curry $ sum . applyVectorBifunction (zipWith (*))

vectorDistance :: Floating a => Vector a -> Vector a -> a
vectorDistance = curry $ sum . map (^ (2 :: Integer)) . applyVectorBifunction (zipWith (-))

vectorCrossProduct :: Floating a => Vector a -> Vector a -> Vector a
vectorCrossProduct (Vector2 x1 y1)    (Vector2 x2 y2)      = Vector3 0          0          (x1 * y2 - x2 * y1)
vectorCrossProduct (Vector2 x1 y1)    (Vector3 x2 y2 z2)   = Vector3 (y1 * z2)  (-z2 * x1) (x1 * y2 - x2 * y1)
vectorCrossProduct (Vector3 x1 y1 z1) (Vector2 x2 y2)      = Vector3 (-y2 * z1) (z1 * x2)  (x1 * y2 - x2 * y1)
vectorCrossProduct (Vector3 x1 y1 z1) (Vector3 x2 y2 z2)
    = Vector3 (y1 * z2 - y2 * z1) (z1 * x2 - z2 * x1) (x1 * y2 - x2 * y1)
