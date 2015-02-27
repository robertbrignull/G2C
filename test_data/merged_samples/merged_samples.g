[assume x_1 (normal 2 4)]
[assume x_2 (normal 3 2)]
[assume x_3 (normal 4 1)]
[assume x_4 (normal (- 2) 1)]

[assume y_1 (+ x_1 7)]          ; ~ N(9, 4)
[assume y_3 (* x_3 (- 2))]      ; ~ N(-8, 4)

[assume z (- y_1 x_2 y_3 x_4)]  ; ~ N(16, 11)

;[predict (list y_1 x_2 y_3 x_4)]
[predict z]
