; Prior drill beliefs
[assume state-of-nature (list 0.5 0.3 0.2)] 

; Oil quantity: dry, wet or soaking (0, 1 or 2)
[assume oil-quant (discrete state-of-nature)]

; Sensor reliability conditional distribution
[assume sound-state-dist (lambda (o : Num) -> List
	(cond ((= o 0) (list .6 .3 .1))
		    ((= o 1) (list .3 .4 .3))
		    (else    (list .1 .4 .5))))]

; Utility function 
[assume utility (lambda (drill : Bool) -> Num 
	(if drill
		  (nth Num (list -70 50 200) oil-quant)
		  0))]

; Make a sound observation
[observe (discrete (sound-state-dist oil-quant)) 2]

; Predict the oil quantity in the well
[predict oil-quant]

; What is the utility if we decide to drill?
[predict (utility true)]

; What is the utility if we don't?
[predict (utility false)]

; With what probability should I drill?
[assume should-i-drill (if (> (utility true) (utility false)) 1 0)]
[predict should-i-drill]
