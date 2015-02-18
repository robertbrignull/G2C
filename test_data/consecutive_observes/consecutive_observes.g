[assume m (normal 5 1)]

[assume f (lambda (x : Num) -> Num (+ (* m x) 2))]
[assume g (lambda (x : Num) -> Num (+ (* 4 x) 2))]

[observe (normal (f 0) 0.1) (g 0)]
[observe (normal (f 1) 0.1) (g 1)]
[observe (normal (f 2) 0.1) (g 2)]
[observe (normal (f 3) 0.1) (g 3)]

[predict m]
