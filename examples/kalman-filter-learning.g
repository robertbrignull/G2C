; measurement noise variance 
[assume R 0.01]					
; state transition model which is applied to the previous state		
[assume A (normal 0 1)] 								
; observation model which maps the true state space into the observed space
[assume H 1]
; process noise variance 											
[assume Q 0.00001]
[assume x (mem (lambda (t : Num) -> Num (if (<= t 1) (normal 0 1) (normal (* A (x (- t 1))) Q))))]
[observe (normal (* H (x 1)) R) 0.38]
[observe (normal (* H (x 2)) R) 0.49]
[observe (normal (* H (x 3)) R) 0.47]
[observe (normal (* H (x 4)) R) 0.28]
[observe (normal (* H (x 5)) R) 0.24]
[observe (normal (* H (x 7)) R) 0.33]
[observe (normal (* H (x 8)) R) 0.47]
[observe (normal (* H (x 9)) R) 0.40]
[observe (normal (* H (x 10)) R) 0.44]
; learn A
[predict A]