[assume marsaglia-normal
	(lambda (mu : num, std : num) -> num
    (let x (uniform-continuous -1.0 1.0)
    (let y (uniform-continuous -1.0 1.0)
    (let s (+ (* x x) (* y y))
      (if (< s 1) 
        (+ mu (* std (* x (sqrt (* -2.0 (/ (log s) s))))))
        (marsaglia-normal mu std))))))]

[assume std (sqrt 2)]
[assume mu (marsaglia-normal 1 (sqrt 5))]

[observe (normal mu std) (+ 8 1)]
[observe (normal mu std) 8]

[predict mu]