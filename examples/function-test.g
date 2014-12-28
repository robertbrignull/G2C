[assume g (lambda (x : num) x)]

[assume f (lambda (x : num) (g (g x)))]

[predict (f 42)]
