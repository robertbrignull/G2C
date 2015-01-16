[assume f (lambda () -> Num (uniform-discrete 5 10))]

[assume g (mem f)]

[assume h (mem (lambda (x : Num) -> Num (uniform-discrete 2 5)))]

[predict (* (f) (f))]        ; not square
[predict (* (g) (g))]        ; square
[predict (* (h 1) (h 1))]    ; square
[predict (* (h 2) (h 3))]    ; not square
