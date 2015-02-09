[assume a (+ (poisson 2) 1)]
[assume b (+ (poisson 3) 1)]

[assume x (beta a b)]

[observe (geometric x) 3]
[observe (geometric x) 8]
[observe (geometric x) 5]

[predict x]
