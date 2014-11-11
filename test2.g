(let g (lambda (x : num) x)
(let f (lambda (x : num) (g (g x)))
  (f 42)))
