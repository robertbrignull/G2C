[assume a (- (poisson 100) 100)]
[assume b (- (poisson 100) 100)]

[observe (normal (+ a b) .00001) 7]

[predict a]
