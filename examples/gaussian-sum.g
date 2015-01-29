; should become a sum of two samples:
; N(-1, 7) + Po(11)

[assume a (normal 4  2)]
[assume b (poisson 4)]
[assume c (normal 2  1)]
[assume d (poisson 1)]
[assume e (normal -5 2)]
[assume f (poisson 6)]
[assume g (normal -2 2)]

[predict (+ a b c d e f g)]
