[assume gradient (uniform-continuous 0 1)]

[assume coeff (normal gradient 0.05)]
[assume const (normal 0 0.2)]

[observe (normal (+ (* 88.6 coeff) const) 0.1) 20.0]
[observe (normal (+ (* 71.6 coeff) const) 0.1) 16.0]
[observe (normal (+ (* 93.3 coeff) const) 0.1) 19.8]
[observe (normal (+ (* 84.3 coeff) const) 0.1) 18.4]
[observe (normal (+ (* 80.6 coeff) const) 0.1) 17.1]
[observe (normal (+ (* 75.2 coeff) const) 0.1) 15.5]

[predict gradient]
