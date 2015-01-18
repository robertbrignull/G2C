; Define expected inputs and outputs as lists
[assume Input  (list (list -1 -1) (list -1  1) (list  1 -1) (list  1  1))]
[assume Output (list           0            1            1            0)]

; Helper functions for accessing expected inputs / outputs
[assume I (lambda (i : Num) -> List (nth List Input  i))]
[assume O (lambda (i : Num) -> Num  (nth Num  Output i))]

; Define weights for layer 1 nodes
[assume w-L1-N1 (list (normal 0 0.5)
                      (normal 0 0.5))]
[assume w-L1-N2 (list (normal 0 0.5)
                      (normal 0 0.5))]
[assume w-L1-N3 (list (normal 0 0.5)
                      (normal 0 0.5))]
[assume w-L1-N4 (list (normal 0 0.5)
                      (normal 0 0.5))]

; Define weights for layer 2 nodes
[assume w-L2-N1 (list (normal 0 0.5)
                      (normal 0 0.5)
                      (normal 0 0.5)
                      (normal 0 0.5))]
[assume w-L2-N2 (list (normal 0 0.5)
                      (normal 0 0.5)
                      (normal 0 0.5)
                      (normal 0 0.5))]

; Define weights for output node
[assume w-O     (list (normal 0 0.5)
                      (normal 0 0.5))]

; Define activation function
[assume activate (lambda (v : Num) -> Num (- (/ 2 (+ 1 (exp (* -10 (+ v -0.5))))) 1))]

; Compute layer 1 node values as a list based on input nodes
[assume L1 (mem (lambda (i : Num) -> List
    (list (activate (+ (* (first Num  (I i)) (first Num  w-L1-N1))
                       (* (second Num (I i)) (second Num w-L1-N1))))
          (activate (+ (* (first Num  (I i)) (first Num  w-L1-N2))
                       (* (second Num (I i)) (second Num w-L1-N2))))
          (activate (+ (* (first Num  (I i)) (first Num  w-L1-N3))
                       (* (second Num (I i)) (second Num w-L1-N3))))
          (activate (+ (* (first Num  (I i)) (first Num  w-L1-N4))
                       (* (second Num (I i)) (second Num w-L1-N4)))))))]

; Compute layer 2 node values as a list based on layer 1 nodes
[assume L2 (mem (lambda (i : Num) -> List
    (list (activate(+ (+ (+ (* (nth Num (L1 i) 0) (nth Num w-L2-N1 0))
                            (* (nth Num (L1 i) 1) (nth Num w-L2-N1 1)))
                            (* (nth Num (L1 i) 2) (nth Num w-L2-N1 2)))
                            (* (nth Num (L1 i) 3) (nth Num w-L2-N1 3))))
          (activate(+ (+ (+ (* (nth Num (L1 i) 0) (nth Num w-L2-N2 0))
                            (* (nth Num (L1 i) 1) (nth Num w-L2-N2 1)))
                            (* (nth Num (L1 i) 2) (nth Num w-L2-N2 2)))
                            (* (nth Num (L1 i) 3) (nth Num w-L2-N2 3)))))))]

; Compute output value based on layer 2 nodes
[assume p-O  (mem (lambda (i : Num) -> Num
    (if (>= (activate (+ (* (first Num  (L2 i)) (first Num  w-O))
                         (* (second Num (L2 i)) (second Num w-O))))
            0) 1 0)))]


; Train the network using our training data
[observe (normal (p-O 0) 0.1) (O 0)]
[observe (normal (p-O 1) 0.1) (O 1)]
[observe (normal (p-O 2) 0.1) (O 2)]
[observe (normal (p-O 3) 0.1) (O 3)]

; Predict response to the four input values
[predict (list (I 0) (p-O 0))]
[predict (list (I 1) (p-O 1))]
[predict (list (I 2) (p-O 2))]
[predict (list (I 3) (p-O 3))]