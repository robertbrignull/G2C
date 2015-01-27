; here we're testing the ability to deal with lots of arithmetic
; expressions and assumed ids around the mean.

; should have mean = 4.09, variance = 0.364

;[assume three (- (* 5 (+ 0 1)) 2)]
;[assume x (normal (* three 1) (- (* 5 (+ 0 1)) three))]
;[observe (normal (/ (- (* 3 (+ x 1)) 4) 2) (/ 4 (+ 2 2))) (+ 2 three 1)]
;[predict x]

[assume x (normal 3 2)]
[observe (normal x 1) 6]
[predict x]
