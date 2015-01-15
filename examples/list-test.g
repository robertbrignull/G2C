[assume fs (list (lambda (x : Num) -> Num x)
                 (lambda (x : Num) -> Num (* 2 x))
                 (lambda (x : Num) -> Num (* 3 x)))]

[assume n (uniform-discrete 0 3)]

[predict ((nth (Num) -> Num fs n) 42)]
