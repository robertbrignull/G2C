; here we're testing having multiple assumed normals

; should have:
; x mean ~ 6.2,   x variance = 0.4
; y mean = 1.875, y variance = 0.625

[assume x (normal 5 2)]
[assume y (normal 3 1)]

[observe (normal x 1) 8]
[observe (normal (+ x y) 1) 7]

[predict x]
[predict y]
