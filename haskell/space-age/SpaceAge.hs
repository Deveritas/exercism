module SpaceAge where

data Planet = Earth | Mercury | Venus | Mars | Jupiter | Saturn | Uranus | Neptune

earthYear :: Double
earthYear = 1 / 31557600

yearLength :: Planet -> Double
yearLength Earth   = earthYear
yearLength Mercury = earthYear / 0.2408467
yearLength Venus   = earthYear / 0.61519726
yearLength Mars    = earthYear / 1.8808158
yearLength Jupiter = earthYear / 11.862615
yearLength Saturn  = earthYear / 29.447498
yearLength Uranus  = earthYear / 84.016846
yearLength Neptune = earthYear / 164.79132

ageOn :: Planet -> Double -> Double
ageOn planet = (*) $ yearLength planet