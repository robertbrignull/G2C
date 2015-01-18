; Define initial state distribution
[assume initial-hidden-state      (categorical Num (list (list 1 0.8)  (list 2 0.1)  (list 3 0.1) ))]

; Define next hidden state distributions
[assume next-state-1              (lambda () -> Num (categorical Num (list (list 1   0)  (list 2 0.8)  (list 3 0.2) )))]
[assume next-state-2              (lambda () -> Num (categorical Num (list (list 1 0.2)  (list 2   0)  (list 3 0.8) )))]
[assume next-state-3              (lambda () -> Num (categorical Num (list (list 1 0.8)  (list 2 0.2)  (list 3   0) )))]

; Define state durations distrubutions
[assume state-duration-1          (lambda () -> Num (ceil (normal 10 (sqrt 2))))]
[assume state-duration-2          (lambda () -> Num (ceil (normal 20 (sqrt 2))))]
[assume state-duration-3          (lambda () -> Num (ceil (exponential 1)))]

[assume get-observation-mu-state  (lambda (state : Num) -> Num state)]
[assume get-observation-std-state (lambda (state : Num) -> Num (sqrt 0.2))]

[assume get-observation-mu        (lambda (time : Num) -> Num (get-observation-mu-state  (get-hidden-state time)))]
[assume get-observation-std       (lambda (time : Num) -> Num (get-observation-std-state (get-hidden-state time)))]

; Get the observed state based on the hidden state
[assume get-observed-value        (lambda (time : Num) -> Num (normal (get-observation-mu time) (get-observation-std time)))]

; The EDHMM state is stored as a two element list with the first element
; containing the state id and the second element containing the time remaining
; in the state

; Takes a new state and returns a list containing the state and the duration
; remaining in the state
[assume get-new-state-duration (lambda (state : Num) -> List
    (cond ( (= state 1) (list state (state-duration-1)) )
          ( (= state 2) (list state (state-duration-2)) )
          ( else        (list state (state-duration-3)) ) ) )]

[assume get-new-state (lambda (state : Num) -> List
    (cond ( (= state 1) (get-new-state-duration (next-state-1)) )
          ( (= state 2) (get-new-state-duration (next-state-2)) )
          ( else        (get-new-state-duration (next-state-3)) ) ) )]

; Decrements the time remaining in the current state
[assume decrement-state-duration (lambda (state : List) -> List (list (first Num state) (- (second Num state) 1)) )]

; Returns a list containing the next state id and time remaining. At time 0
; this is deterined by the initial state distribution. Otherwise, if there is
; time remaining in the current state then decrement and return the same state.
; Otherwise transition to the next state and initialize the time rmaining.
[assume get-hidden-state-with-duration 
        (mem (lambda (time : Num) -> List
                (if (= time 0)
                    (get-new-state-duration initial-hidden-state)
                    (if (<= (get-hidden-state-duration-remaining (- time 1)) 0)
                        (get-new-state (get-hidden-state (- time 1)))
                        (decrement-state-duration (get-hidden-state-with-duration (- time 1)))
                    )
                )
        ))]

; Get the state id at specified time
[assume get-hidden-state (lambda (time : Num) -> Num (first Num (get-hidden-state-with-duration time)) )]

; Get time remaining in state at specified time
[assume get-hidden-state-duration-remaining (lambda (time : Num) -> Num (second Num (get-hidden-state-with-duration time)) )]

; Required to run generative model
[assume final-hidden-state-with-duration (get-hidden-state-with-duration 99)]

; Helper functions to return time series data as a list
[assume get-hidden-state-list       (lambda (i : Num, c : Num) -> List
        (if (= i c) (list (get-hidden-state c))
                    (cons (get-hidden-state i)
                          (get-hidden-state-list (+ i 1) c) )))]
[assume get-duration-remaining-list (lambda (i : Num, c : Num) -> List
        (if (= i c) (list (get-hidden-state-duration-remaining c))
                    (cons (get-hidden-state-duration-remaining i)
                          (get-duration-remaining-list (+ i 1) c) )))]
[assume get-observed-value-list     (lambda (i : Num, c : Num) -> List
        (if (= i c) (list (get-observed-value c))
                    (cons (get-observed-value i)
                          (get-observed-value-list (+ i 1) c) )))]

; Predict EDHMM state and duration remaining and observed value
[predict (get-hidden-state-list 0 50)]
[predict (get-duration-remaining-list 0 50)]
[predict (get-observed-value-list 0 50)]