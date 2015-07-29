module Noise

type Seed = uint64

type INoise =
    abstract member reseed : Seed -> INoise
    abstract member getValue : float -> float
