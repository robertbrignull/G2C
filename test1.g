(let g 
     (lambda (f : (num) -> num, x : num)
         (if (= x 0) 0 (f x)))
     (g (lambda (y : num)
                (* y 2))
        21))
