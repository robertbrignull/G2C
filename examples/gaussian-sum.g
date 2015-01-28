; should become a sum of two samples:
; N(-1, 7) + Po(6)

[assume a (normal 4  2)]
[assume b (normal 2  1)]
[assume c (normal -5 2)]
[assume d (poisson 6)]
[assume e (normal -2 2)]

[predict (+ a b c d e)]
