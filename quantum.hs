newtype Complex re im = Complex { re :: Num, im :: Num }
instance (Num re, Num im) => Num (Complex re im) where
    Complex (re, im) + Complex (ore, oim) = Complex (re + ore, im + oim)
    Complex (re, im) - Complex (ore, oim) = Complex (re - ore, im - oim)
    Complex (re, im) * Complex (ore, oim) = Complex (re * ore - im * oim, re * oim + im * ore)
    --Complex (re, im) / Complex (ore, oim) = Complex (re, im) * Complex (ore / (ore * ore - oim * oim), -oim / (ore * ore - oim * oim))
    --Complex (r, i) ^ n                = iterate ((*) Complex (r, i)) (Complex (r, i)) !! n
    Complex (re, im) :^ n = iterate ((*) Complex (re, im)) (Complex (re, im)) !! n

data Potential =
    InfiniteSquareWell {
        n :: Int,   -- nth frequency
        a :: Float  -- width of well
    } | SimpleHarmonic {
        n :: Int,   -- n states above ground
        w :: Float  -- angular velocity of harmonic oscillator ( sqrt (k / m))
    }

type Wave = Float -> Complex Float

h_ :: Float
h_ = 1.05457173 * 10 ^ (-34)

i :: (Num a) => Complex a
i = Complex 0 1

fact :: (Num a) => Int -> a
fact 0 = 1
fact n = fromIntegral n * fact (n-1)

d_d x state = state -- TODO Differentiate w/ respect to x

solveWave :: Potential -> Float -> Wave

solveWave (InfiniteSquareWell n a) m x = (1 / sqrt a) * sin (sqrt (2 * m * e) / h_ * x)
    where
        e = ((fromIntegral n)^2 * pi^2 * h_**2) / (2 * m * a^2)

solveWave (SimpleHarmonic n w) m x = (a n) * (iterate raiseOp (ground_state x) !! n)
    where
        a n = ((m * w) / (pi * h_)) ^ (1/4) * ((-i) ^ n) / sqrt ((fact n) * (h_ * w) ^ n)
        raiseOp state = (((h_ / i) * (d_d x $ state)) + i * m * w * x) / sqrt(2 * m)
        ground_state x = (a 0) * exp ((-m * w * x^2) / (2 * h_)) :+ 0
