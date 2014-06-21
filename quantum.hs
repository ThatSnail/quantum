-- WARNING: Wave function assumed to be time-independent and separable.  TODO: Append time-dependent factor exp(-iEt/h_)

type Mass = Double

data Particle = Electron | Photon

-- All values in kg
mass :: Particle -> Mass
mass Electron = 9.10938291 / (10 ^ 31)
mass Photon   = 0 -- ?

data Complex re im = Complex { re :: Double, im :: Double } deriving Show
instance (Num re, Num im) => Num (Complex re im) where
    Complex re im + Complex ore oim = Complex (re + ore) (im + oim)
    Complex re im - Complex ore oim = Complex (re - ore) (im - oim)
    Complex re im * Complex ore oim = Complex ((re * ore) - (im * oim)) ((re * oim) + (im * ore))
    fromInteger x = Complex (fromInteger x) 0
    abs (Complex re im) = Complex (abs re) (abs im)
    signum (Complex re im) = Complex (signum re) (signum im)
instance (Fractional re, Fractional im) => Fractional (Complex re im) where
    (Complex re im) / (Complex ore oim) = (Complex re im) * Complex (ore / (ore * ore + oim * oim)) (-oim / (ore * ore + oim * oim))
    fromRational x = Complex (fromRational x) 0
instance (Fractional re, Fractional im) => Floating (Complex re im) where
    pi = Complex pi 0
    log (Complex re im) = Complex (log r) p
        where
            r = sqrt (re * re + im * im)
            p = atan2 im re
    exp (Complex re im) = Complex ((exp re) * (sin im)) ((exp re) * (cos im))
    sin (Complex re im) = Complex ((sin re) * (cosh im)) ((cos re) * (sinh im))
    cos (Complex re im) = Complex ((cos re) * (cosh im)) (-(sin re) * (sinh im))
    sinh (Complex re im) = Complex ((sinh re) * (cos im)) ((cosh re) * (sin im))
    cosh (Complex re im) = Complex ((cosh re) * (cos im)) ((sinh re) * (sin im))

data Potential =
    InfiniteSquareWell {
        n :: Int,   -- nth frequency
        a :: Double  -- width of well
    } | SimpleHarmonic {
        n :: Int,   -- n states above ground
        w :: Double  -- angular velocity of harmonic oscillator ( sqrt (k / m) )
    }

type Wave = Double -> Complex Double Double

h_ :: (Fractional a) => a
h_ = 1.05457173 / (10 ^ 34)

i :: Complex Double Double
i = Complex 0 1

eps :: (Fractional a) => a
eps = 0.00000000001

fact :: (Num a) => Int -> a
fact 0 = 1
fact n = fromIntegral n * fact (n-1)

complexConj :: (Num a) => (a -> Complex b b) -> (a -> Complex b b)
complexConj f x = Complex (re $ f x) (-(im $ f x))

mag :: (Num a, Num b) => (a -> Complex b b) -> (a -> Complex b b)
mag f x = ((complexConj f) x) * (f x)

(<+>) :: (Num a, Num b) => (a -> b) -> (a -> b) -> (a -> b)
f <+> g = \x -> f x + g x

-- Derivative
-- WARNING: Only works with functions that take one argument!
d_d :: (Fractional a, Fractional b) => (a -> b) -> a -> b
d_d f x = ((f (x + eps)) - (f x)) / eps

solveWave :: Potential -> Mass -> Wave
solveWave (InfiniteSquareWell n a) m x
    | -a < x && x < a = Complex (a0 * sin (sqrt (2 * m * e) / h_ * x)) 0
    | otherwise       = 0
    where
        a0 = 1 / sqrt a -- Normalization constant
        e = ((fromIntegral n)^2 * pi^2 * h_**2) / (2 * m * a^2) -- Energy
solveWave (SimpleHarmonic n w) m x = (a n) * (iterate raiseOp ground_state_trans !! n) (Complex x 0)
    where
        a n = (Complex (((m * w) / (pi * h_)) ** (1/4)) 0) * ((-i) ^ n) / (Complex (sqrt ((fact n) * (h_ * w) ^ n)) 0)
        raiseOp state = (((*(h_ / i)) . d_d state) <+> ((*imwx) . state)) . (*recipSqrt2m)
            where
                imwx = Complex 0 (m * w * x)
                recipSqrt2m = 1 / (Complex (sqrt (2 * m)) 0)
        ground_state_trans (Complex x _) = ground_state x
            where
                ground_state x = (a 0) * (Complex (exp ((-m * w * x^2) / (2 * h_))) 0)


