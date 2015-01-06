[assume g
  (lambda (x : num) -> num
  	x)]

[assume f
	(lambda (x : num) -> num
		(g (g x)))]

[predict (f 42)]
