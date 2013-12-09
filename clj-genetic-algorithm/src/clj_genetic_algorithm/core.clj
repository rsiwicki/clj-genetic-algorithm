(ns clj-genetic-algorithm.core)

(def initial-population-size 4)
(def min-fitness 0)
(def cross-over-probability 0.2)

(defn avg-population-fitness [population] 
  (/ (reduce + (map #(:fitness %) population)) (count population)))

(defn max-population-fitness [population]
  (reduce max (map #(:fitness %) population)))

(defn rnd-int [] (int (* (rand) 10)))

(def gene-pool [:DUP :SWAP :MUL :ADD :OVER :NOP])

(defn get-op [] (rand-nth gene-pool))

(defn build-genome []
  (for [x (range 10)] (get-op)))

(defn build-population [] 
  (for [x (range initial-population-size)] (assoc {:fitness 0} :genome (build-genome))))

; returns new stack post operator 
; e.g. (run-vm [:SWAP :DUP :ADD :ADD :ADD :DUP :MUL :DUP :MUL] [1 2 3])
; => [65536]
(defn cycle-virt-mach [instruction stack]
  (case instruction 
    :DUP (conj stack (peek stack))
    :SWAP (let [l (last stack) n (nth stack 1)] (conj (pop (pop stack)) l n))
    :MUL (conj (pop (pop stack)) (* (last stack) (nth stack 1)))
    :ADD (conj (pop (pop stack)) (+ (last stack) (nth stack 1)))
    :OVER (conj (pop stack) (first stack))
    :NOP stack
    ))

(defn run-virt-mach [instruction-set stack]
  (let [[instruction & residual-instruction-set] instruction-set ]
  	(if (nil? instruction)
     stack
   	(recur residual-instruction-set (cycle-virt-mach instruction stack)))))

; testing x^3 + y^2 + z  
; returns fitness
(defn evaluate-genome [genome]
  (try
  	(let [x (rnd-int) 
        y (rnd-int) 
        z (rnd-int) 
        answer (+ (* x x x) (* y y) z)
        stack (conj [] x y z)
        phenotype (run-virt-mach genome stack)]
     (if (= (count phenotype) 1) 
       (if (= (first phenotype) answer) 100 10) 0))
  (catch Exception e -10) ; your genome broke the virtual machine!
  )) 

(defn splice-genome [genome]
  (split-at (int (* (rand) (count genome))) genome)
  )

; need a probability to make it less destructive
(defn reproduce [parent-xx parent-xy]
  (let [p-x (splice-genome parent-xx)
        p-y (splice-genome parent-xy)
        new-xx (concat (first p-x) (last p-y))
        new-xy (concat (first p-y) (last p-x))
        ]
    	(if (< (rand) cross-over-probability) 
       		(list new-xx new-xy)
         	(list parent-xx parent-xy))))

; need to make fitness additive
(defn evaluate-population [population]
  (map #(assoc {} :genome % :fitness (evaluate-genome %)) (map #(:genome %) (build-population))))

(defn evolve [population] 
  (let [p (evaluate-population population)
        max-fitness (max-population-fitness p)
        avg-fitness (avg-population-fitness p)]
		; retrieve pairs and reproduce them
  		(for [x (range 0 (count population) 2) ]
      		(let [child-1 (nth population x)
              	  child-2 (nth population (+ x 1))
                  parent-1 (select-parent population max-fitness min-fitness 0)
                  parent-2 (select-parent population max-fitness min-fitness 0)
                  progeny (reproduce (:genome parent-1) (:genome parent-2))
                  ]
     			  (list (assoc {:fitness (:fitness child-1)} :genome (first progeny))
               			(assoc {:fitness (:fitness child-1)} :genome (first progeny)))			
     		)	
       )  	
 
 ))

; selectes a parent at random based on the proability related to fitness
; if no parent is found after probing 10% of population
; returns random candidate
(defn select-parent [population max-fitness min-fitness breaker] 
  (let [candidate-genome (rand-nth population)
        candidate-fitness (:fitness candidate-genome)
        selection-probability (if (= max-fitness 0) 
                                0 
                                (/ candidate-fitness max-fitness))
        max-tie-breaker 100]   	
     	(if (= breaker max-tie-breaker)
     		(rand-nth population)
      	(if 
       		(and (> candidate-fitness min-fitness) 
              	 (< (rand) selection-probability))
       		candidate-genome
            (recur population max-fitness min-fitness (inc breaker))
        ))
         
  ))

(defn recombine [] 1)


(defn main [] 1)