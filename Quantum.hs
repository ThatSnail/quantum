-- WARNING: Wave function assumed to be time-independent and separable.  TODO: Append time-dependent factor exp(-iEt/h_)

import Complex

type Mass = Double
type ProbabilityDistr = Double -> Double -> Double

data Particle = Electron | Photon

-- All values in kg
mass :: Particle -> Mass
mass Electron = 9.10938291 / (10 ^ 31)
mass Photon   = 0 -- ?

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

intEps :: (Fractional a) => a
intEps = 0.001

fact :: (Num a) => Int -> a
fact 0 = 1
fact n = fromIntegral n * fact (n-1)

complexConj :: (Num a) => (a -> Complex b b) -> (a -> Complex b b)
complexConj f x = Complex (re $ f x) (-(im $ f x))

mag :: (Num a, Num b) => (a -> Complex b b) -> (a -> Double)
mag f x = re (((complexConj f) x) * (f x))

(<+>) :: (Num a, Num b) => (a -> b) -> (a -> b) -> (a -> b)
f <+> g = \x -> f x + g x

-- Derivative
-- WARNING: Only works with functions that take one argument!
d_d :: (Fractional a, Fractional b) => (a -> b) -> a -> b
d_d f x = ((f (x + eps)) - (f x)) / eps

integrate :: (Fractional a) => (Double -> a) -> Double -> Double -> a
integrate f a b = sum $ map ((*intEps ) . f) [a, a + intEps..b]

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

waveProb :: Wave -> ProbabilityDistr
waveProb state = integrate (mag state)

waveProbPoint :: Wave -> Double -> Double
waveProbPoint state x = waveProb state x (x + eps)

runTests :: IO ()
runTests = do
    putStr "Running tests...\n\n"
    putStr "Test 1 : Infinite Square Well\n"
    putStr $ (take 50 $ repeat '=') ++ "\n\n"
    putStr "Testing completeness condition P(-a, +a) == 1\n"
    putStr $ testResults $ testCompleteness
    putStr "Done!\n"

testResults :: Bool -> String
testResults True  = "Passed.\n"
testResults False = "Failed!\n"

testCompleteness :: Bool
testCompleteness = foldl1 (&&) $ map ((<0.0001) . abs . (\x -> x-1) . (\(n, a) -> f n a (-a) a)) tests
    where
        f n a = waveProb (solveWave (InfiniteSquareWell n a) (mass Electron))
        tests = do
            n <- [1, 5]
            a <- [0.1, 0.2..3]
            [(n, a)]
