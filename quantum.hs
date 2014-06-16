import Data.Complex

type Wave = Float -> Complex Float

h_ :: Float
h_ = 1.05457173 * 10 ^ (-34)

i :: Complex
i = 0 :+ 1

d_d x state = -- TODO Differentiate w/ respect to x

-- Infinite square well
infinite_square_well :: (Int, Float) -> Float -> Wave
infinite_square_well (n, a) m x = (1 / sqrt a) * sin (sqrt (2 * m * e) / h_ * x) :+ 0
    where
        e = ((fromIntegral n)^2 * pi^2 * h_**2) / (2 * m * a^2)

-- Simple harmonic potential
simple_harmonic_potential :: 
simple_harmonic_potential = (A n) * (iterate raiseOp ground_state !! n)
    where
        A n = ((m * w) / (pi * h_)) ^ (1/4) * ((-i) ^ n) / sqrt ((fact n) * (h_ * w) ^ n)
        raiseOp state = (((h_ / i) * (d_d x $ state)) + i * m * w * x) / sqrt(2 * m)
