data Complex re im = Complex { re :: Double, im :: Double } deriving Show
instance (Num re, Num im) => Num (Complex re im) where
    Complex re im + Complex ore oim = Complex (re + ore) (im + oim)
    Complex re im - Complex ore oim = Complex (re - ore) (im - oim)
    Complex re im * Complex ore oim = Complex (re * ore - im * oim) (re * oim + im * ore)
    fromInteger x = Complex (fromInteger x) 0

instance (Fractional re, Fractional im) => Fractional (Complex re im) where
    (Complex re im) / (Complex ore oim) = (Complex re im) * Complex (ore / (ore * ore + oim * oim)) (-oim / (ore * ore + oim * oim))
    fromRational x = Complex (fromRational x) 0

data Potential =
    InfiniteSquareWell {
        n :: Int,   -- nth frequency
        a :: Double  -- width of well
    } | SimpleHarmonic {
        n :: Int,   -- n states above ground
        w :: Double  -- angular velocity of harmonic oscillator ( sqrt (k / m))
    }

type Wave = Double -> Complex Double Double

h_ :: (Fractional a) => a
h_ = 1.05457173 / (10 ^ 34)

i :: (Num a) => Complex a a
i = Complex 0 1

fact :: (Num a) => Int -> a
fact 0 = 1
fact n = fromIntegral n * fact (n-1)

d_d x state = state -- TODO Differentiate w/ respect to x
solveWave :: Potential -> Double -> Wave

solveWave (InfiniteSquareWell n a) m x = Complex (a0 * sin (sqrt (2 * m * e) / h_ * x)) 0
    where
        a0 = 1 / sqrt a
        e = ((fromIntegral n)^2 * pi^2 * h_**2) / (2 * m * a^2)
{-|
solveWave (SimpleHarmonic n w) m x = Complex ((a n) * (iterate raiseOp (ground_state x) !! n)) 0
    where
        a n = ((m * w) / (pi * h_)) ^ (1/4) * ((-i) ^ n) / sqrt ((fact n) * (h_ * w) ^ n)
        raiseOp state = (((h_ / i) * (d_d x $ state)) + i * m * w * x) / sqrt(2 * m)
        ground_state x = ((a 0) * exp ((-m * w * x^2) / (2 * h_)))
|-}
