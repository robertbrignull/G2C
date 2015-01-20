[assume fib (lambda (n : Num) -> Num (if (< n 1) 1 (+ (fib (- n 1)) (fib (- n 2)))))]
[predict (fib 24)]
