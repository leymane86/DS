#|
In this project you will produce three things:

1. A high-level evolutionary computation framework

2. The representation, and breeding functions, and maybe your own evaluation
function for a simple GA for boolean vectors.

3. The representation, and breeding functions, and maybe your own evaluation
function for a simple GA for floating-point vectors.

4. The representation, breeding functions, and evaluations to do GP for
two problems:
A. Symbolic Regression
B. Artificial Ant

The high-level EC system will work as follows:

- Simple generational evolution
- GA-style Tournament selection
- A simple breeding function
- some simple statistics functions

I have provided the approximate function templates I myself used to complete
the task; with permission you can use these or go your own way, but otherwise
please try not to deviate much from this template.

The project is due approximately two and a half weeks from now or so.  Please 
try to get it in on time.

WHAT YOU MUST PROVIDE:

1. Completed code which works and compiles.  As simple as possible would be nice.

2. A short report describing the code you wrote and some experimentation with it.
Some things to try:
   -- different problems (in the vector section)
   -- different settings of mutation and crossover parameters
   -- different population sizes or run lengths
Try to get a handle on how problem difficulty changes when you tweak various
parameters.  Can you get mutation and crossover parameters which seem optimal for
a given problem for example?  (Obviously bigger population sizes are more optimal
but that's kinda cheating).  Note that this is a STOCHASTIC problem so you'll need
to run a number of times and get the mean best result in order to say anything of
consequence.  It'd be nice if the report were in LaTeX and it'd be good practice for
you as well, but it's not necessary.  I do not accept reports in Word.  Only send
me a PDF.

Make sure your code is compiled.  In most cases (such as SBCL), your code will be
automatically compiled.
|#


;;; Useful Functions and Macros

(defmacro swap (elt1 elt2)
  "Swaps elt1 and elt2, using SETF.  Returns nil."
  (let ((temp (gensym)))
    `(let ((,temp ,elt1))
       (setf ,elt1 ,elt2)
       (setf ,elt2 ,temp)
       nil)))

(defmacro while (test return-value &body body)
  "Repeatedly executes body as long as test returns true.
Then evaluates and returns return-value"
  `(progn (loop while ,test do (progn ,@body)) ,return-value))

(defun random-elt (sequence)
  "Returns a random element from sequence"
  (elt sequence (random (length sequence))))

(defun random? (&optional (prob 0.5))
  "Tosses a coin of prob probability of coming up heads,
then returns t if it's heads, else nil."
  (< (random 1.0) prob))

(defun generate-list (num function &optional no-duplicates)
  "Generates a list of size NUM, with each element created by
  (funcall FUNCTION).  If no-duplicates is t, then no duplicates
are permitted (FUNCTION is repeatedly called until a unique
new slot is created).  EQUALP is the test used for duplicates."
  (let (bag)
    (while (< (length bag) num) bag
      (let ((candidate (funcall function)))
	(unless (and no-duplicates
		     (member candidate bag :test #'equalp))
	  (push candidate bag))))))

;; hope this works right
(defun gaussian-random (mean variance)
  "Generates a random number under a gaussian distribution with the
given mean and variance (using the Box-Muller-Marsaglia method)"
  (let ((x 0) (y 0) (w 0))
    (while (not (and (< 0 w) (< w 1)))
	   (setf x (- (random 2.0) 1.0))
	   (setf y (- (random 2.0) 1.0))
	   (setf w (+ (* x x) (* y y))))
    (+ mean (* x (sqrt variance) (sqrt (* -2 (/ (log w) w)))))))

;;;;;; TOP-LEVEL EVOLUTIONARY COMPUTATION FUNCTIONS 


;;; TOURNAMENT SELECTION

;; is this a good setting?  Try tweaking it (any integer >= 2) and see
(defparameter *tournament-size* 7)

(defun tournament-select-one (population fitnesses)
  (let* ((best (random ( length population)))
	 (challenger NIL))
    (dotimes (i *tournament-size*)
      (setf challenger (random (length population)))
      (if (< (elt fitnesses best) (elt fitnesses challenger))
	  (setf best challenger)))
  ;"Does one tournament selection and returns the selected individual."
    (elt population best)))

 ;"Does one tournament selection and returns the selected individual."

  ;;; IMPLEMENT ME
  ;;;
  ;;; See Algorithm 32 of Essentials of Metaheuristics

  ;;; Hints:
  ;;; 1. My implementation is about 7 lines long
  ;;; 2. This would be a reasonable place to judiciously use SETF
  ;;; 3. You might want to do a loop, and keep track of both the
  ;;;    best individual you've found so far and also its fitness




(defun tournament-selector (num population fitnesses)
  (generate-list num (lambda () (tournament-select-one population fitnesses)))

  ;"Does NUM tournament selections, and puts them all in a list, then returns the list"
  ;;; IMPLEMENT ME
  ;;;
  ;;; Hints:
  ;;; 1. This is a very short function.  My version is 1 line long.
  ;;; 2. Maybe one of the utility functions I provided might be of
  ;;;    benefit here

)



;; I'm nice and am providing this for you.  :-)
(defun simple-printer (pop fitnesses)
  "Determines the individual in pop with the best (highest) fitness, then
prints that fitness and individual in a pleasing manner."
  (let (best-ind best-fit)
    (mapcar #'(lambda (ind fit)
		(when (or (not best-ind)
			  (< best-fit fit))
		  (setq best-ind ind)
		  (setq best-fit fit))) pop fitnesses)
    (format t "~%Best Individual of Generation...~%Fitness: ~a~%Individual:~a~%"
	    best-fit best-ind)
    fitnesses))


;(evolve 50 100
; 	:setup #'boolean-vector-sum-setup
;	:creator #'boolean-vector-creator
;	:selector #'tournament-selector
;	:modifier #'boolean-vector-modifier
;        :evaluator #'leading-ones-f
;	:printer #'simple-printer)


(defun evolve (generations pop-size
	       &key setup creator selector modifier evaluator printer)
  "Evolves for some number of GENERATIONS, creating a population of size
POP-SIZE, using various functions"
    ;(funcall setup)
     (let* ((population)
            (fitness)
            (population-integer)
            (Q nil)
            (parenta)
            (parentb)
            (children))
       
       (funcall setup)
       (dotimes (i pop-size)
         (push (funcall creator) population))

       (dotimes (j generations)  
         (setf population-integer NIL)  
         (if (and (> (length (elt population 1)) 1)
		  (typep (elt (elt population 1) 1) 'boolean))
             (dotimes (i (length population))     
               (let ((a NIL))
		 (dotimes (j (length (elt population i))) 
		   (if (eq  (elt (elt population i) j) t) 
		       (setf a (append a '(1)))
		       (setf a (append a '(0)))))
		 (setf a (append a '(0)))
		 (setf population-integer (append population-integer (list a)))))
             (setf population-integer population))
         
         (setf fitness (mapcar evaluator population-integer)) 
         (print "evolve")
         (setf parenta (funcall selector (/ pop-size 2) population fitness))
         (setf parentb (funcall selector (/ pop-size 2) population fitness))
         (dotimes (i (/ pop-size 2))
           (setf children (funcall modifier (elt parenta i) (elt parentb i)))
           (push (elt children 0) q)
           (push (elt children 1) q))
	 (setf population q)
	 (setf q NIL)
         (funcall printer population fitness))
       
       (funcall printer population fitness)))
        
  ;;; IMPLEMENT ME
  ;;;
  ;; The functions passed in are as follows:
  ;;(SETUP)                     called at the beginning of evolution, to set up
  ;;                            global variables as necessary
  ;;(CREATOR)                   creates a random individual
  ;;(SELECTOR num pop fitneses) given a population and a list of corresponding fitnesses,
  ;;                            selects and returns NUM individuals as a list.
  ;;                            An individual may appear more than once in the list.
  ;;(MODIFIER ind1 ind2)        modifies individuals ind1 and ind2 by crossing them
  ;;                            over and mutating them.  Returns the two children
  ;;                            as a list: (child1 child2).  Nondestructive to
  ;;                            ind1 and ind2.
  ;;(PRINTER pop fitnesses)     prints the best individual in the population, plus
  ;;                            its fitness, and any other interesting statistics
  ;;                            you think interesting for that generation.
  ;;(EVALUATOR individual)      evaluates an individual, and returns its fitness.
  ;;Pop will be guaranteed to be a multiple of 2 in size.
  ;;
  ;; HIGHER FITNESSES ARE BETTER

  ;; your function should call PRINTER each generation, and also print out or the
  ;; best individual discovered over the whole run at the end, plus its fitness
  ;; and any other statistics you think might be nifty.

  ;;; HINTS: 
  ;;; 1. You could do this in many ways.  But I implemented it using
  ;;;    the following functions (among others)
  ;;;    FUNCALL FORMAT MAPCAR LAMBDA APPLY
  ;;; 2. My version of this function is about 20 lines long.
  ;;;    This sounds long but it's really pretty straightforward
  ;;;    code with no particular tricks.
  ;;; 3. Pay attention to the keyword arguments


;;;;;; BOOLEAN VECTOR GENETIC ALGORTITHM


;;; Creator, Modifier, Evaluator, and Setup functions for the
;;; boolean vectors Problem.  Assume that you are evolving a bit vector
;;; *vector-length* long.  

;;; The default test function is Max-Ones.
;;;; Max-ones is defined in section 11.2.1 of "Essentials of Metaheuristics"
;;; by yours truly.  In that section are defined several other boolean functions
;;; which you should also try and report on.  Some plausible ones might
;;; include:

;;; :max-ones
;;; :trap
;;; :leading-ones
;;; :leading-ones-blocks



(defparameter *boolean-vector-length* 100)
(defparameter *boolean-problem* :max-ones)
;; perhaps other problems might include... 

(defun boolean-vector-creator ()
  (let* ((individual (make-array *boolean-vector-length*))) 
    (loop for i from 0 to (- *boolean-vector-length* 1)
	  do
          (if (  < (random 1.0) 0.5)
              (setf (aref individual i) t)
              (setf (aref individual i) NIL)))
    individual))

(defparameter *boolean-crossover-probability* 0.02)
(defparameter *boolean-mutation-probability* 0.01)


(defun uniform-crossover (ind1 ind2)
  "Performs uniform crossover on the two individuals, modifying them in place.
*crossover-probability* is the probability that any given allele will crossover.  
The individuals are guaranteed to be the same length.  Returns NIL."
  (dotimes (i *boolean-vector-length*) 
    (if ( < (random 1.0) *boolean-crossover-probability*)
        (rotatef (elt ind1 i) (elt ind2 i)))))

(defun boolean-vector-modifier (ind1 ind2)
    "Copies and modifies ind1 and ind2 by crossing them over with a uniform crossover,
then mutates the children.  *crossover-probability* is the probability that a
given allele will crossover.  *mutation-probability* is the probability that any
given allele in a child will mutate.  Mutation simply flips the bit of the allele."
  (let* ((child1 (copy-seq ind1)) 
	  (child2 (copy-seq ind2)))
     (uniform-crossover child1 child2)
     (dotimes (i *boolean-vector-length*)
       (if ( < (random 1.0) *boolean-mutation-probability*)
           (setf (elt child1 i) (not (elt child1 i))))
       (if ( < (random 1.0) *boolean-mutation-probability*)
           (setf (elt child2 i) (not (elt child2 i)))))
     (list child1 child2))) 


;; to impement BOOLEAN-VECTOR-MODIFIER (and FLOAT-VECTOR-MODIFIER below) the
;; following function is strongly reccommended.


(defun boolean-vector-evaluator (ind1)
  "Evaluates an individual, which must be a boolean-vector, and returns
its fitness."
  (let* ((fitness 0))
    (dotimes (i *boolean-vector-length*) 
      (if (elt ind1 i)
          (incf fitness)))
    fitness))

(defun boolean-vector-sum-setup ()
  "Does nothing.  Perhaps you might use this to set up various
(ahem) global variables to define the problem being evaluated, I dunno."
  )


;;; FITNESS EVALUATION FUNCTIONS

;;; I'm providing you with some classic boolean objective functions you could  See
;;; section 11.2.1 of Essentials of Metaheuristics for details on these functions.


(defun max-ones-f (ind)
  "Sums the vector."
  (reduce #'+ ind))

(defun leading-ones-f (ind)
  "Counts the number of ones in your vector, starting at the beginning, until
a zero is encountered."
  (position 0 ind))

(defparameter *leading-ones-block-size* 3)
(defun leading-ones-blocks (ind)
  "Counts the number of groups of *leading-ones-block-size* ones in your vector,
until a zero is encountered.  For example, if *leading-ones-block-size* is 3,
then 1110000 is 1, 1111000 is 1, 1111100 is 1, and 1111110 is 2."
  (floor (leading-ones-f ind) *leading-ones-block-size*))



;;; an example way to fire up the GA.  It should easily discover
;;; a 100% correct solution.
#|
(evolve 50 100
 	:setup #'boolean-vector-sum-setup
	:creator #'boolean-vector-creator
	:selector #'tournament-selector
	:modifier #'boolean-vector-modifier
        :evaluator #'max-ones-f
	:printer #'simple-printer)
|#







;;;;;; FLOATING-POINT VECTOR GENETIC ALGORTITHM



;;; The default test function is Rastrigin.
;;;; Rastrigin is defined in section 11.2.2 of "Essentials of Metaheuristics"
;;; by yours truly.  In that section are defined several other floating-point functions
;;; which you should also try and report on.  Some plausible ones might
;;; include:

;;; :rastrigin
;;; :rosenbrock
;;; :griewank
;;; :schwefel

;;; If you were really into this, you might also try testing on
;;; rotated problems -- some of the problems above are linearly
;;; separable and rotation makes them harder and more interesting.
;;; See the end of Section 11.2.2.

;;; I have defined the minimum and maximum gene values for rastrigin
;;; as [-5.12, 5.12].  Other problems have other traditional min/max
;;; values, consult Section 11.2.2.



(defparameter *float-vector-length* 100)

(defparameter *float-problem* :rastrigin)
(defparameter *float-min* -5.12)  ;; these will change based on the problem
(defparameter *float-max* 5.12)   ;; likewise


(defun float-vector-creator ()
  "Creates a floating-point-vector *float-vector-length* in size, filled with
random numbers in the range appropriate to the given problem"
  (let* ((individual (make-array *float-vector-length*))) 
    (loop for i from 0 to (- *float-vector-length* 1)
	  do
          (setf (aref individual i)  (+ (random (- *float-max* *float-min*) ) *float-min*)))
    individual))



(defparameter *float-crossover-probability* 0.2)
(defparameter *float-mutation-probability* 0.1)   ;; I just made up this number
(defparameter *float-mutation-variance* 0.01) ;; I just made up this number


(defun float-vector-modifier (ind1 ind2)
  "Copies and modifies ind1 and ind2 by crossing them over with a uniform crossover,
then mutates the children.  *crossover-probability* is the probability that any
given allele will crossover.  *mutation-probability* is the probability that any
given allele in a child will mutate.  Mutation does gaussian convolution on the allele."

   (let* ((child1 (copy-seq ind1)) 
	 (child2 (copy-seq ind2)))
    
    (dotimes (i *float-vector-length*) 
      (if ( < (random 1.0) *float-crossover-probability*)
          (rotatef (elt child1 i) (elt child2 i))))
            
    (let* ((v 0)
	   (n 0))
      (dolist (x (list child1 child2))
	(dotimes (i *float-vector-length*)
	  (setf v (elt x i))
          (if (>= *float-mutation-probability* (random 1.0))
	      (progn  
		(setf n (gaussian-random 0.0 *float-mutation-variance*) )
		(loop while (or (> *float-min* (+ v n)) (< *float-max* (+ v n)))
		      do 
		      (setf n (gaussian-random 0.0 *float-mutation-variance*)))
		(setf (elt x i) (+ v n)))))
      (list child1 child2)))))
  

;;; To implement FLOAT-VECTOR-MODIFIER, the following function is strongly recommended

(defun sum-f (ind)
  "Performs the Sum objective function.  Assumes that ind is a list of floats"
  (reduce #'+ ind))


(defun float-vector-sum-evaluator (ind1)
  "Evaluates an individual, which must be a floating point vector, and returns
its fitness."
    (sum-f ind1))


(defun float-vector-sum-setup ()
  "Does nothing.  Perhaps you might use this function to set
(ahem) various global variables which define the problem being evaluated
and the floating-point ranges involved, etc.  I dunno."
  )


;;; FITNESS EVALUATION FUNCTIONS

;;; I'm providing you with some classic objective functions.  See section 11.2.2 of
;;; Essentials of Metaheuristics for details on these functions.
;;;
;;; Many of these functions (sphere, rosenbrock, rastrigin, schwefel) are
;;; traditionally minimized rather than maximized.  We're assuming that higher
;;; values are "fitter" in this class, so I have taken the liberty of converting
;;; all the minimization functions into maximization functions by negating their
;;; outputs.  This means that you'll see a lot of negative values and that's fine;
;;; just remember that higher is always better.
;;; 
;;; These functions also traditionally operate with different bounds on the
;;; minimum and maximum values of the numbers in the individuals' vectors.  
;;; Let's assume that for all of these functions, these values can legally
;;; range from -5.12 to 5.12 inclusive.  One function (schwefel) normally goes from
;;; about -511 to +512, so if you look at the code you can see I'm multiplying
;;; the values by 100 to properly scale it so it now uses -5.12 to 5.12.



(defun step-f (ind)
  "Performs the Step objective function.  Assumes that ind is a list of floats"
  (+ (* 6 (length ind))
     (reduce #'+ (mapcar #'floor ind))))

(defun sphere-f (ind)
  "Performs the Sphere objective function.  Assumes that ind is a list of floats"
  (- (reduce #'+ (mapcar (lambda (x) (* x x)) ind))))

(defun rosenbrock-f (ind)
  "Performs the Rosenbrock objective function.  Assumes that ind is a list of floats"
  (- (reduce #'+ (mapcar (lambda (x x1)
			   (+ (* (- 1 x) (- 1 x))
			      (* 100 (- x1 (* x x)) (- x1 (* x x)))))
			 ind (rest ind)))))

(defun rastrigin-f (ind)
  "Performs the Rastrigin objective function.  Assumes that ind is a list of floats"
  (- (+ (* 10 (length ind))
	(reduce #'+ (mapcar (lambda (x) (- (* x x) (* 10 (cos (* 2 pi x)))))
			    ind)))))

(defun schwefel-f (ind)
  "Performs the Schwefel objective function.  Assumes that ind is a list of floats"
  (- (reduce #'+ (mapcar (lambda (x) (* (- x) (sin (sqrt (abs x)))))	

			 (mapcar (lambda (x) (* x 100)) ind)))))




;;; an example way to fire up the GA.  If you've got it tuned right, it should quickly
;;; find individuals which are all very close to +5.12

#|
(evolve 50 2000
 	:setup #'float-vector-sum-setup
	:creator #'float-vector-creator
	:selector #'tournament-selector
	:modifier #'float-vector-modifier
        :evaluator #'sum-f
	:printer #'simple-printer)
|#




;;;; GP TREE CREATION CODE

;;; GP's tree individuals are considerably more complex to modify than
;;; simple vectors.  Get the GA system working right before tackling
;;; this part, trust me.



;; set up in the gp setup function -- for example, see
;; the code for gp-symbolic-regression-setup
(defparameter *nonterminal-set* nil)
(defparameter *terminal-set* nil)



;;; important hint: to use SETF to change the position of an element in a list
;;; or a tree, you need to pass into SETF an expression starting with the
;;; parent.  For example (setf (car parent) val) .... thus you HAVE to have
;;; access to the parent, not just the child.  This means that to store a
;;; "position", you have to store away the parent and the arg position of
;;; child, not the child itself.

(defun make-queue ()
  "Makes a random-queue"
  (make-array '(0) :adjustable t :fill-pointer t))
(defun enqueue (elt queue)
  "Enqueues an element in the random-queue"
  (progn (vector-push-extend elt queue) queue))
(defun queue-empty-p (queue)
  "Returns t if random-queue is empty"
  (= (length queue) 0))
(defun random-dequeue (queue)
  "Picks a random element in queue and removes and returns it.
Error generated if the queue is empty."
  (let ((index (random (length queue))))
    (swap (elt queue index) (elt queue (1- (length queue))))
    (vector-pop queue)))

(defun ptc2 (size)
  "If size=1, just returns a random terminal.  Else builds and
returns a tree by repeatedly extending the tree horizon with
nonterminals until the total number of nonterminals in the tree,
plus the number of unfilled slots in the horizon, is >= size.
Then fills the remaining slots in the horizon with terminals.
Terminals like X should be added to the tree
in function form (X) rather than just X."

(let ((root))
  (if (= size 1)
      (setf root  (list (elt *terminal-set* (random (length *terminal-set*)))))
      (progn 
	(let* ((count 1)
	       (queue (make-queue)))
	  (setf root (list NIL)) 	
	  (enqueue root queue)
	  (loop while(> size (- (+ count (length queue)) 1))
		do
		   (let* ((a (copy-seq (elt *nonterminal-set* (random (length *nonterminal-set*)))))
			  (s (random-dequeue queue))
			  (args (elt a 1)))
		     (setf (elt a 1) (list NIL))
      		(enqueue (elt a 1) queue)
		     (if (= args 2)
			 (progn
			   (setf a (append a (list (list NIL))))
			   (enqueue (elt a 2) queue)))
		     (if (= args 3)
			 (progn
			   (setf a (append a (list (list NIL) (list NIL))))
			   (enqueue (elt a 2) queue)
			   (enqueue (elt a 3) queue)))
		     (setf (car s) (car a))
		     (setf (cdr s) (cdr a))
		     (incf count)))
	
	  (loop while(< 0 (length queue))
		do
		   (let* ((s (random-dequeue queue))
			  (a (elt *terminal-set* (random (length *terminal-set*)))))  
		
		     (setf (car s) NIL)
		     (setf (car s) a))))))
  root))
  #|
  The simple version of PTC2 you will implement is as follows:

  PTC2(size):
  if size = 1, return a random terminal
  else
     q <- make-queue
     root <- random nonterminal
     count <- 1
     enqueue into q each child argument slot of root
     Loop until count + size_of_q >= size
        remove a random argument slot s from q
        a <- random nonterminal
        count <- count + 1
        fill the slot s with a
        enqueue into q each child argument slot of a
     Loop until size_of_q = 0
        remove a random argument slot s from q
        a <- random terminal
        fill the slot s with a
     return root


  Note that "terminals" will all be considered to be
  zero-argument functions.  Thus PTC might generate the
  s-expression tree:

  (cos (+ (x) (- (x) (x)) (sin (x))))

  but it should NOT generate the tree:

  (cos (+ x (- x x) (sin x)))


  Note that this has some gotchas: the big gotcha is that you have to keep track of
  argument slots in lisp s-expressions, and not just pointers to the values presently
  in those slots.  If you are totally lost as to how to implement that, I can provide
  some hints, but you should try to figure it out on your own if you can.
  |#

  ;;; IMPLEMENT ME

;  )


(defparameter *size-limit* 20)

(defun gp-creator ()
  "Picks a random number from 1 to 20, then uses ptc2 to create
a tree of that size"
  (ptc2 (+ 1 (random 20))))


;;; GP TREE MODIFICATION CODE

(defvar *example* '(a (b c) (d e (f (g h i j)) k)))

(defun num-nodes (tree)
  "Returns the number of nodes in tree, including the root"

  (let* ((i 0)
	 (total 0))
    (loop while (< i (length tree))
	  do  
          (if (listp (elt tree i))
	      (progn 
		(setf total (+ total (num-nodes (elt tree i)))))
	      (incf total 1))
	  (incf i))
  total))




(defun nth-subtree-parent (tree n)
    "Given a tree, finds the nth node by depth-first search though
the tree, not including the root node of the tree (0-indexed). If the
nth node is NODE, let the parent node of NODE is PARENT,
and NODE is the ith child of PARENT (starting with 0),
then return a list of the form (PARENT i).  For example, in
the tree (a (b c d) (f (g (h) i) j)), let's say that (g (h) i)
is the chosen node.  Then we return ((f (g (h) i) j) 0).

If n is bigger than the number of nodes in the tree
 (not including the root), then we return n - nodes_in_tree
 (except for root)."

  (let* ((queue (make-queue))
	 (tmp NIL)
	 (index-queue (make-queue))
	 (i 0)
	 (nth 0)
	 (parent NIL))
	 
    (enqueue tree queue)
    (enqueue i index-queue)

    (loop while (and (> (length queue) 0) (/= nth n))
	  do (setf tmp (vector-pop queue))
	     (setf i (vector-pop index-queue)) 
	     (setf parent tmp)
	     (loop while (and (< i (- (length tmp) 1)) (/= nth n))
		   do
		      (incf nth)
  		      (incf i)
		      (if (= nth n)
			  (return-from nth-subtree-parent (list parent i)))
		      (if (listp (elt tmp i))
			  (progn 
			    (enqueue tmp queue)
			    (enqueue i index-queue)
			    (setf parent (elt tmp i))
			    (setf tmp (elt tmp i))
			    (setf i 0)))))))


  ;;; this is best described with an example:
  ;    (dotimes (x 12)
  ;           (print (nth-subtree-parent
  ;                       '(a (b c) (d e (f (g h i j)) k))
  ;                        x)))
  ;;; result:
  ;((A (B C) (D E (F (G H I J)) K)) 0) 
  ;((B C) 0) 
  ;((A (B C) (D E (F (G H I J)) K)) 1) 
  ;((D E (F (G H I J)) K) 0) 
  ;((D E (F (G H I J)) K) 1) 
  ;((F (G H I J)) 0) 
  ;((G H I J) 0) 
  ;((G H I J) 1) 
  ;((G H I J) 2) 
  ;((D E (F (G H I J)) K) 2) 
  ;0 
  ;1 
  ;NIL

    ;;; IMPLEMENT ME



(defun node-type (choice node)
(if (and (< .1 choice) (listp node))
    T
    NIL))

(defparameter *mutation-size-limit* 10)

(defun gp-modifier (ind1-input ind2-input)
  "Flips a coin.  If it's heads, then ind1 and ind2 are
crossed over using subtree crossover.  If it's tails, then
ind1 and ind2 are each mutated using subtree mutation, where
the size of the newly-generated subtrees is picked at random
from 1 to 10 inclusive.  Doesn't damage ind1 or ind2.  Returns
the two modified versions as a list."

  (let* ((ind1 (copy-tree ind1-input))
	 (ind2 (copy-tree ind2-input))
	 (ind1-num (num-nodes ind1))
	 (ind2-num  (num-nodes ind2))
	 (ind1-index (random ind1-num))
	 (ind2-index (random ind2-num))
	 (chose (random 1.0))
	 (ind1-nth)
	 (ind1-parent)
	 (ind2-nth)
	 (ind2-parent)
	 (crossover-prob 0.9)
	 (results nil))
    
	 (setf results (nth-subtree-parent ind1 ind1-index))
  
	(if (eq results NIL)
	    (progn 
	      (setf ind1-nth 0)
	      (setf ind1-parent (list ind1)))
	    (progn
	      (setf ind1-nth (elt results 1))
	      (setf ind1-parent (elt results 0))))
	(setf results (nth-subtree-parent ind2 ind2-index))

	(if (eq results NIL)
	    (progn 
	      (setf ind2-nth 0)
	      (setf ind2-parent (list ind2)))
	    (progn
	      (setf ind2-nth (elt results 1))
	      (setf ind2-parent (elt results 0))))
	(if (< chose crossover-prob)
	    (progn
	      (rotatef (elt ind1-parent ind1-nth) (elt ind2-parent ind2-nth)))
	    (progn
	      (setf (elt ind1-parent ind1-nth) (ptc2 (+ 1 (random 10))))
	      (setf (elt ind2-parent ind2-nth) (ptc2 (+ 1 (random 10))))))

	(if (eq (nth-subtree-parent ind1-input ind1-index) NIL) 
            (setf ind1 (elt ind1-parent 0)))
	(if (eq (nth-subtree-parent ind2-input ind2-index) NIL) 
            (setf ind2 (elt ind2-parent 0)))

	(list ind1 ind2)))
 
 


;;; SYMBOLIC REGRESSION
;;; This problem domain is similar, more or less, to the GP example in
;;; the lecture notes.  Your goal is to make a symbolic expression which
;;; represents a mathematical function consisting of SIN COS, EXP,
;;; +, -, *, and % (a version of / which doesn't give an error on
;;; divide-by-zero).  And also the function X, which returns a current
;;; value for the X variable.
;;;
;;; In symbolic regression, we generate 20 (x,y) pairs produced at
;;; random which fit the expression y = x^4 + x^3 + x^2 + x.  You can
;;; make up another function is you like, but that's the one we're going
;;; with here.  Using just these data pairs, the GP system will attempt
;;; to ascertain the function.  It will generate possible expressions
;;; as its individuals, and the fitness of an expression is how closely,
;;; for each X value, it gives the correct corresponding Y value.
;;;
;;; This is called SYMBOLIC regression because we're actually learning
;;; the mathematical expression itself, including transcendental functions
;;; like SIN and COS and E^.  As opposed to statistical linear or
;;; quadratic curve-fitting regressions which just try to learn the
;;; linear or quadratic parameters etc.
;;;
;;; An example 100% optimal solution:
;;;
;;; (+ (* (x) (* (+ (x) (* (x) (x))) (x))) (* (+ (x) (cos (- (x) (x)))) (x)))



;;; GP SYMBOLIC REGRESSION SETUP
;;; (I provide this for you)

(defparameter *num-vals* 20)
(defparameter *vals* nil) ;; gets set in gp-setup

(defun gp-symbolic-regression-setup ()
  "Defines the function sets, and sets up vals"

  (setq *nonterminal-set* '((+ 2) (- 2) (* 2) (% 2) (sin 1) (cos 1) (exp 1)))
  (setq *terminal-set* '(x))

  (setq *vals* nil)
  (dotimes (v *num-vals*)
    (push (1- (random 2.0)) *vals*)))

(defun poly-to-learn (x) (+ (* x x x x) (* x x x) (* x x) x))

;; define the function set
(defparameter *x* nil) ;; to be set in gp-evaluator
(defun x () *x*)
(defun % (x y) (if (= y 0) 0 (/ x y)))  ;; "protected division"
;;; the rest of the functions are standard Lisp functions




;;; GP SYMBOLIC REGRESSION EVALUATION

(defun gp-symbolic-regression-evaluator (ind)
  "Evaluates an individual by setting *x* to each of the
elements in *vals* in turn, then running the individual and
get the output minus (poly-to-learn *x*).  Take the
absolute value of the this difference.  The sum of all such
absolute values over all *vals* is the 'raw-fitness' Z.  From
this we compute the individual's fitness as 1 / (1 + z) -- thusl
large values of Z are low fitness.  Return the final
individual's fitness.  During evaluation, the expressions
evaluated may overflow or underflow, or produce NaN.  Handle all
such math errors by
returning most-positive-fixnum as the output of that expression."

;(setf example '(+ x x))
;(setf x 2)
;(setf y (lambda (x) (eval example)))
;(funcall y 2)

(let* ((sum 0)
       (i 0)
       (result 0))
  (if (and (listp ind) (listp (car ind)))
      (setf ind (car ind)))
  (loop while (< i (length *vals*))
	do
	(setf *x* (elt *vals* i))
	(setf result (handler-case (eval ind) (error () most-positive-fixnum) ))
	(setf sum (+ sum (abs (- result (poly-to-learn *x*)))))
	(incf i))
  (/ 1.0 (+ 1.0 sum))))

  ;;; hint:
  ;;; (handler-case
  ;;;  ....
  ;;;  (error (condition)
  ;;;     (format t "~%Warning, ~a" condition) most-positive-fixnum))



;;; Example run
#|
(evolve 50 500
 	:setup #'gp-symbolic-regression-setup
	:creator #'gp-creator
	:selector #'tournament-selector
	:modifier #'gp-modifier
        :evaluator #'gp-symbolic-regression-evaluator
	:printer #'simple-printer)
|#





;;; GP ARTIFICIAL ANT CODE
;;; for this part you'll need to implement both the evaluator AND
;;; make up the function that form the function set.

;;; In the artificial ant problem domain, you'll be evolving an s-expression
;;; which moves an ant around a toroidal map shown below.  The ant starts out
;;; at (0,0), facing east (to the right).  The functions in
;;; the expression are:
;;; (if-food-ahead --then-- --else--)   If food is directly ahead of the ant,
;;;                                     then evaluate the THEN expression, else
;;;                                     evaluate the ELSE expression
;;; (progn2 --item1-- --item2--)        Evaluate item1, then evaluate item 2
;;; (progn3 --item1-- --item2-- --item3--)  Evaluate item1, then item2, then item3
;;; (move)                              Move forward one unit
;;;                                     If you pass over a food pellet, it is eaten
;;;                                     and removed from the map.
;;; (left)                              Turn left
;;; (right)                             Turn right
;;;
;;;
;;; the way a tree is evaluated is as follows.  The whole tree is evaluated,
;;; and the functions move the ant about.  If the ant has not made a total of
;;; 600 MOVE, LEFT, and RIGHT operations yet, then the tree is evaluated AGAIN
;;; moving the ant some more from its current position.  This goes on until
;;; 600 operations have been completed, at which time the ant will not move
;;; any more.  If 600 operations are completed in the middle of the evaluation
;;; of a tree, the simplest approach is to have the MOVE command simply
;;; "stop working" so the ant doesn't gobble up any more pellets accidentally.

;;; The fitness of the artificial ant is the number of pellets it ate in the
;;; 600-operation period.  Maps are reset between evaluations of different
;;; individuals of course.

;;; Here's an optimal ant program (there are many such):
;;  (progn3 (if-food-ahead (move) (progn2 (left) (progn2 (left)
;;             (if-food-ahead (move) (right))))) (move) (right)))

;;; This is a hard problem for GP and you may need to run many times before you
;;; get a 100% perfect answer.

;;; Note that in my thesis it says 400 moves and not 600.  We're going with
;;; 600 here.  It's easier.



;;; our ant's food trail map
(defparameter *map-strs* '(
".###............................"
"...#............................"
"...#.....................###...."
"...#....................#....#.."
"...#....................#....#.."
"...####.#####........##........."
"............#................#.."
"............#.......#..........."
"............#.......#..........."
"............#.......#........#.."
"....................#..........."
"............#..................."
"............#................#.."
"............#.......#..........."
"............#.......#.....###..."
".................#.....#........"
"................................"
"............#..................."
"............#...#.......#......."
"............#...#..........#...."
"............#...#..............."
"............#...#..............."
"............#.............#....."
"............#..........#........"
"...##..#####....#..............."
".#..............#..............."
".#..............#..............."
".#......#######................."
".#.....#........................"
".......#........................"
"..####.........................."
"................................"))

(defparameter *map-height* 32)
(defparameter *map-width* 32)


;;; some useful functions for you

(defun make-map (lis)
  "Makes a map out of a string-list such as *MAP-STRS*"
  (let ((map (make-array (list (length (first lis)) (length lis)))))
    (dotimes (y (length lis) map)
      (dotimes (x (length (elt lis y)))
	(setf (aref map x y)
	      (cond ((equalp #\# (elt (elt lis y) x)) nil)
		    (t t)))))))

;; The four directions.  For relative direction, you might
;; assume that the ant always PERCEIVES things as if it were
;; facing north.
(defconstant *n* 0)
(defconstant *e* 1)
(defconstant *s* 2)
(defconstant *w* 3)

(defun direction-to-arrow (dir)
  "Returns a character which represents a given direction -- might
be useful for showing the movement along a path perhaps..."
  (cond ((= dir *n*) #\^)
	((= dir *s*) #\v)
	((= dir *e*) #\>)
	(t #\<)))

(defun maparray (function array &optional (same-type nil))
  "Maps function over array in its major order.  If SAME-TYPE,
then the new array will have the same element type as the old
array; but if a function returns an invalid element then an error
may occur.  If SAME-TYPE is NIL, then the array will accommodate
any type."
  (let ((new-array (apply #'make-array (array-dimensions array)
			  :element-type (if same-type
					    (array-element-type array)
					  t)
			  :adjustable (adjustable-array-p array)
			  (if (vectorp array)
			      `(:fill-pointer ,(fill-pointer array))
			    nil))))
    (dotimes (x (array-total-size array) new-array)
      (setf (row-major-aref new-array x)
	    (funcall function (row-major-aref array x))))))

(defun print-map (map)
  "Prints a map, which must be a 2D array.  If a value in the map
is T (indicating a space), then a '.' is printed.  If a value in the map
is NIL (indicating a food pellet), then a '#' is printed.  If a value in
the map is anything else, then it is simply PRINTed.  This allows you to
consider spaces to be all sorts of stuff in case you'd like to print a
trail of spaces on the map for example.  Returns NIL."
  (let ((dim (array-dimensions map)))
    (dotimes (y (second dim) nil)
      (format t "~%")
      (dotimes (x (first dim))
	(format t "~a"
		(let ((v (aref map x y)))
		  (cond ((equal v t) #\.)
			((null v) #\#)
			(t v))))))))



(defmacro absolute-direction (relative-dir ant-dir)
  "If the ant is facing ANT-DIR, then converts the perceived
RELATIVE-DIR direction into an absolute ('true') direction
and returns that."
  `(mod (+ ,relative-dir ,ant-dir) 4))

(defmacro x-pos-at (x-pos absolute-dir &optional (steps 1))
  "Returns the new x position if one moved STEPS steps the absolute-dir
direction from the given x position.  Toroidal."
  `(mod (cond ((= (mod ,absolute-dir 2) *n*) ,x-pos)         ;; n or s
	      ((= ,absolute-dir *e*) (+ ,x-pos ,steps))     ;; e
	      (t (+ ,x-pos (- ,steps) *map-width*)))         ;; w
	*map-width*))

(defmacro y-pos-at (y-pos absolute-dir &optional (steps 1))
  "Returns the new y position if onee moved STEPS steps in the absolute-dir
direction from the given y position.  Toroidal."
  `(mod (cond ((= (mod ,absolute-dir 2) *e*) ,y-pos)        ;; e or w
	      ((= ,absolute-dir *s*) (+ ,y-pos ,steps))     ;; s
	      (t (+ ,y-pos (- ,steps) *map-height*)))       ;; n
	*map-height*))


(defparameter *current-move* 0 "The move # that the ant is at right now")
(defparameter *num-moves* 600 "How many moves the ant may make")
(defparameter *current-x-pos* 0 "The current X position of the ant")
(defparameter *current-y-pos* 0 "The current Y position of the ant")
(defparameter *current-ant-dir* *e* "The current direction the ant is facing")
(defparameter *eaten-pellets* 0 "How many pellets the ant has eaten so far")
(defparameter *map* (make-map *map-strs*) "The ant's map")




;;; the function set you have to implement

(defun neighborhood ()
    (list
     (list *current-x-pos* (y-pos-at *current-y-pos* *n*))
     (list (x-pos-at *current-x-pos* *e*) *current-y-pos*)
     (list *current-x-pos* (y-pos-at *current-y-pos* *s*))
     (list (x-pos-at *current-x-pos* *w*) *current-y-pos*)))

(defun food-here-p ()
  (not (aref *map* *current-x-pos* *current-y-pos*)))

(defun food-ahead-p ()
  (let* ((ahead-coordinates (elt (neighborhood) *current-ant-dir*))
	 (x-ahead (first ahead-coordinates))
	 (y-ahead (second ahead-coordinates)))
    (not (aref *map* x-ahead y-ahead))))

(defmacro if-food-ahead (then else)
  "If there is food directly ahead of the ant, then THEN is evaluated,
else ELSE is evaluated"
  ;; because this is an if/then statement, it MUST be implemented as a macro.
  `(if (food-ahead-p)
      ,then
      ,else))


(defun progn2 (arg1 arg2)
    "Evaluates arg1 and arg2 in succession, then returns the value of arg2"
    (declare (ignore arg1))
    arg2)  ;; ...though in artificial ant, the return value isn't used ... 

(defun progn3 (arg1 arg2 arg3)
  "Evaluates arg1, arg2, and arg3 in succession, then returns the value of arg3"
  (declare (ignore arg1 arg2))
  arg3)  ;; ...though in artificial ant, the return value isn't used ...

(defun increment-move-count ()
  (setf *current-move* (+ 1 *current-move*)))

(defun have-move-budget-p ()
  (< *current-move* *num-moves*))

(defun move ()
  "If the move count does not exceed *num-moves*, increments the move count
and moves the ant forward, consuming any pellet under the new square where the
ant is now.  Perhaps it might be nice to leave a little trail in the map showing
where the ant had gone."
  (if (have-move-budget-p)
      (progn
	(increment-move-count)
	(setf *current-x-pos* (x-pos-at *current-x-pos* *current-ant-dir*))
	(setf *current-y-pos* (y-pos-at *current-y-pos* *current-ant-dir*))
	(if (food-here-p)
	    (progn
	     (setf *eaten-pellets* (+ 1 *eaten-pellets*))
	     (setf (aref *map* *current-x-pos* *current-y-pos*) T))))))


(defun left ()
  "Increments the move count, and turns the ant left"
  (if (have-move-budget-p)
      (progn
	(increment-move-count)
	(setf *current-ant-dir* (mod (- *current-ant-dir* 1) 4)))))

(defun right ()
  "Increments the move count, and turns the ant right"
  (if (have-move-budget-p)
      (progn
	(increment-move-count)
	(setf *current-ant-dir* (mod (+ 1 *current-ant-dir*) 4)))))

(defparameter *nonterminal-set* nil)
(defparameter *terminal-set* nil)

;; I provide this for you
(defun gp-artificial-ant-setup ()
  "Sets up vals"
  (setq *nonterminal-set* '((if-food-ahead 2) (progn2 2) (progn3 3)))
  (setq *terminal-set* '(left right move))
  (setq *map* (make-map *map-strs*))
  (setq *current-move* 0)
  (setq *eaten-pellets* 0))


;; you'll need to implement this as well

(defun gp-artificial-ant-evaluator (ind)
  "Evaluates an individual by putting it in a fresh map and letting it run
for *num-moves* moves.  The fitness is the number of pellets eaten -- thus
more pellets, higher (better) fitness."
  (progn
    (setf *map* (make-map *map-strs*))
    (setf *current-x-pos* 0)
    (setf *current-y-pos* 0)
    (setf *current-ant-dir* *e*)
    (setf *current-move* 0)
    (setf *eaten-pellets* 0)
    (if (and (listp ind) (listp (car ind)))
	(setf ind (car ind)))
    (loop while (< *current-move* *num-moves*)
	  do
	  (eval ind))
    *eaten-pellets*))

;; you might choose to write your own printer, which prints out the best
;; individual's map.  But it's not required.

#|
(evolve 50 500
 	:setup #'gp-artificial-ant-setup
	:creator #'gp-creator
	:selector #'tournament-selector
	:modifier #'gp-modifier
        :evaluator #'gp-artificial-ant-evaluator
	:printer #'simple-printer)
|#
