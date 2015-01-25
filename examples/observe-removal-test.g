[assume three (- (* 5 (+ 0 1)) 2)]
[assume x (normal (* three 1) (- (* 5 (+ 0 1)) three))]
[observe (normal (/ (- (* 3 (+ x 1)) 4) 2) (/ 4 (+ 2 2))) (+ 2 three 1)]
[predict x]