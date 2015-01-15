[assume g
  (lambda (x : Num) -> Num
  	x)]

[assume f
	(lambda (x : Num) -> Num
		(g (g x)))]

[predict (f 42)]
