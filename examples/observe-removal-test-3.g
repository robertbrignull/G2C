[assume a (+ (poisson 2) 1)]
[assume b (+ (poisson 3) 1)]

[assume p (beta a b)]

[observe (flip p) true]
[observe (flip p) true]
[observe (flip p) true]
[observe (flip p) false]
[observe (flip p) true]
[observe (flip p) false]

[predict p]
