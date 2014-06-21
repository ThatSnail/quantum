module Complex where

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
