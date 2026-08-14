;;;;;;;; SIMPLE-PLAN
;;;;;;;; The following planner is a brute-force planning partial-order (POP) system.  
;;;;;;;; It doesn't use a heuristic, but instead just uses iterative-deepening search.  
;;;;;;;; If you want to see a high-quality, free POP planner, check out UCPOP at
;;;;;;;; https://www.cs.washington.edu/ai/ucpop.html
;;;;;;;; Note that there are many better planners still at this point (POP is getting old).
;;;;;;;;
;;;;;;;; The following planner does not use variables -- it's propositional and not
;;;;;;;; predicate-logic based.  Because it doesn't use
;;;;;;;; variables, we have to write more code to describe a problem (see the Blocks World
;;;;;;;; example at end) but it is a much easier algorithm to for you to write (you don't have to
;;;;;;;; be involved in unification.  Happy!).  
;;;;;;;;
;;;;;;;; I am giving you some basic code which defines predicates, links,
;;;;;;;; orderings, and strips operators and plans.  Then I'm giving you
;;;;;;;; the entry code which creates a minimal plan.  Lastly, you get a
;;;;;;;; simple two-block blocks world problem to play with, plus a three-
;;;;;;;; block blocks world problem to play with after you get the two-blocks
;;;;;;;; version working properly.
;;;;;;;;
;;;;;;;; SOME HINTS:
;;;;;;;; 1. Read up on DOTTED PAIRS
;;;;;;;; 2. Read up on ASSOCIATION LISTS
;;;;;;;; 3. Read up on STRUCTURES
;;;;;;;; 4. Read up on RETURN-FROM, RETURN, CATCH, and THROW
;;;;;;;; 5. MAPC and other mapping functions might be useful to you.
;;;;;;;; 6. So might HASH TABLES
;;;;;;;;
;;;;;;;; This is a tough assignment.  If you have any questions, feel free to ask me.
;;;;;;;; If you get really stuck, I might be willing to "release" a function or two
;;;;;;;; to you, but at a cost in project grade of course.  Still, it'd be better than
;;;;;;;; not getting the project running.
;;;;;;;;
;;;;;;;; The big challenge in this project is getting the code running fast.  The class
;;;;;;;; will likely have a *huge* variance in running speed.  I can implement this code
;;;;;;;; to run very fast indeed, just a few milliseconds, so if your code is taking
;;;;;;;; many seconds to run, it's time to think about optimization and algorithmic
;;;;;;;; improvements.  
;;;;;;;;
;;;;;;;; Once you get the code working you might then think about how to implement some
;;;;;;;; other problems.  Check out ones in the book and think about how to propositionalize
;;;;;;;; them.  Or invent some fun problems.  You'll discover quickly that variables help
;;;;;;;; a lot!  A fun project might be to write a bit of code which converts a variable-style
;;;;;;;; problems to propositionalized ones so you don't have to write a million operators.
;;;;;;;; Or you might test how the system scales with more complex problems.  At any rate,
;;;;;;;; do some interesting extension of it.
;;;;;;;;
;;;;;;;; Provide the code for the project and a report as usual describing what you did and
;;;;;;;; what extensions or experiments you performed.  Please do not deviate far from the
;;;;;;;; code proper -- it's hard to grade otherwise.


;;;;; PREDICATES

;;; A predicate is a list of the form (t-or-nil name)
;;; where the t-or-nil indicates if it's negated,
;;; and NAME is a symbol indicating the predicate name.
;;; Example: (NIL B-ON-A)  "B is not on A"

(defun negate (predicate)
  "Negates a predicate.  Pretty simple!"
  (cons (not (first predicate)) (rest predicate)))


;;;;;; STRIPS OPERATORS
;;;;;; A strips operator has is described below.  Note that
;;;;;; UNIQ allows two operators with the same name, preconds
;;;;;; and effects to be in the plan at once.

(defstruct operator
  "Defines a strips operator consisting of
a NAME (a symbol or string),
a UNIQ gensym symbol assigned when the operator is added to the plan,
a list of PRECONDITIONS (predicates)
a list of EFFECTS ( predicates),

The resultant function MAKE-OPERATOR creates a *template*,
which is different from an instantiated operator actually in a plan.
Use instantiate-operator to create an operator from a template."
  name uniq preconditions effects)


;; Expect a possible warning here about redefinition
(defun copy-operator (operator)
  "Copies the operator and assigns it a new unique gensym symbol to make
it unique as far as EQUALP is concerned.  Returns the copy."
  ;; I suggest you use this code to guarantee that the operator is properly
  ;; copied out of the templates.
  (let ((op (copy-structure operator)))
    (setf (operator-uniq op) (gensym))
    op))



;;;;;;; LINKS
;;;;;;; A link is a structure that specifies a causal link with a from-operator,
;;;;;;; a to-operator, and precondition of the to-operator involved
;;;;;;; (which is the SAME as the effect of the from-operator!)

(defstruct (link
             (:print-function print-link))
  "FROM and TO are operators in the plan.
  PRECOND is the predicate that FROM's effect makes true in TO's precondition."
  from precond to)

(defun print-link (p stream depth)
  "Helper function to print link in a pretty way"
  (declare (ignorable depth))
  (format stream "#< (~a)~a -> (~a)~a : ~a >"
          (when (link-from p) (operator-uniq (link-from p)))
          (when (link-from p) (operator-name (link-from p)))
          (when (link-to p) (operator-uniq (link-to p)))
          (when (link-to p) (operator-name (link-to p)))
          (link-precond p)))


;;;;;;; ORDERINGS
;;;;;;; An ordering is just a dotted pair of the form (before-op . after-op)
;;;;;;; where before-op and after-op are strips operators (instances of
;;;;;;; the OPERATOR structure).  The ordering specifies
;;;;;;; that before-op must come before after-op.


(defun print-ordering (p stream depth)
  "Helper function to print link in a pretty way"
  (declare (ignorable depth))
  (format stream "#[ (~a)~a -> (~a)~a ]"
          (operator-uniq (first p))
          (operator-name (first p))
          (operator-uniq (rest p))
          (operator-name (rest p))))


;;;;;;; PLANS
;;;;;;; A plan is a list of operators, a list of orderings, and a list of
;;;;;;; links, plus the goal and start operator (which are also in the operator
;;;;;;; list).

(defstruct (plan (:print-function print-plan))
  "A collection of lists of operators, orderings, and links,
plus a pointer to the start operator and to the goal operator."
  operators orderings links start goal)

(defun print-plan (p stream depth)
  "Helper function print plan in a pretty way"
  (declare (ignorable depth))
  (format stream "#< PLAN operators: ~{~%~a~} ~%links: ~{~%~a~} ~%orderings: ~{~%~a~}~%>"
          (plan-operators p) (plan-links p)
          (mapcar #'(lambda (ordering)
                      (print-ordering ordering nil 0))
                  (plan-orderings p))))

;; Expect a possible warning here about redefinition
(defun copy-plan (plan)
  ;; I suggest you use this code to guarantee that the plan is copied
  ;; before you do any destructive coding on it.
  "Deep-copies the plan, and copies the operators, orderings, and links."
  (let ((p (copy-structure plan)))
    (setf (plan-operators p) (copy-tree (plan-operators p)))
    (setf (plan-orderings p) (copy-tree (plan-orderings p)))
    (setf (plan-links p) (copy-tree (plan-links p)))
    p))




;;;;;;;;; UTILITY FUNCTIONS
;;;;;;;;; I predict you will find these functions useful.

;;;; Reachable takes an association list and determines if you can reach
;;;; an item in the list from another item.  For example:
;;;;
;;;; (reachable '((a . b) (c . d) (b . c) (b . e) (e . a)) 'e 'd)
;;;; --> T   ;; e->a, a->b, b->c, c->d

(defun reachable (assoc-list from to)
  "Returns t if to is reachable from from in the association list."
  ;; expensive!

;;; SPEED HINT.  You might rewrite this function to be more efficient.
;;; You could try dividing the list into two lists, one consisting of association pairs
;;; known to be reachable from FROM and ones not know to be reachable, then
;;; using the property of transitivity, move pairs from the second list to the first
;;; list, until either you discover it's reachable, or nothing else is moving.

  (dolist (x assoc-list nil)
    (when (and (equalp (car x) from)
               (or (equalp (cdr x) to)
                   (reachable (remove x assoc-list) (cdr x) to)))
      (return t))))


;;;; Cyclic-assoc-list takes an association list and determines if it
;;;; contains a cycle (two objects can reach each other)
;;;;
;;;; (cyclic-assoc-list '((a . b) (c . d) (b . c) (b . e) (e . a)))
;;;; --> T   ;; a->b, b->e, e->a
;;;;
;;;; (cyclic-assoc-list '((a . a)))
;;;; --> T   ;; a->a

(defun cyclic-assoc-list (assoc-list)
  (dolist (x assoc-list nil)
    (when (reachable assoc-list (cdr x) (car x))
      (return t))))

;;;; Binary-combinations returns all N^2 combinations of T and NIL.
;;;; 
;;;; (binary-combinations 4)
;;;; -->
;;;; ((NIL T NIL T) (T T NIL T)
;;;;  (NIL NIL NIL T) (T NIL NIL T)
;;;;  (NIL T T T) (T T T T)
;;;;  (NIL NIL T T) (T NIL T T)
;;;;  (NIL T NIL NIL) (T T NIL NIL)
;;;;  (NIL NIL NIL NIL) (T NIL NIL NIL)
;;;;  (NIL T T NIL) (T T T NIL)
;;;;  (NIL NIL T NIL) (T NIL T NIL))

(defun binary-combinations (n)
  "Gives all combinations of n t's and nils"
  (let ((bag '(())))
    (dotimes (x n bag)
      (let (bag2)
        (dolist (b bag)
          (push (cons t b) bag2)
          (push (cons nil b) bag2))
        (setf bag bag2)))))



;;;;;; PLANNING CODE TEMPLATES
;;;;;;
;;;;;; The following are the functions I used to implement my planner.
;;;;;; You have been given all interfaces I used, but no implementation.
;;;;;; This is how I went about doing it -- if you want to you can of course
;;;;;; do the code differently, but indicate that you are doing so.
;;;;;;
;;;;;; The recursion looks like this:
;;;;;; SELECT-SUBGOAL calls CHOOSE-OPERATOR, which calls HOOK-UP-OPERATOR,
;;;;;; which calls RESOLVE-THREATS, which in turn calls SELECT-SUBGOAL.
;;;;;; All of these functions return a either plan
;;;;;; (the solution) or NIL (failure to find a solution).
;;;;;; 
;;;;;; In SELECT-SUBGOAL I test to see if the maximum depth is exceeded
;;;;;; and fail immediately if it is.  That's the only place I check for
;;;;;; depth.  You are free to test other places if you want.  Then I
;;;;;; I increment the current depth before passing it to CHOOSE-OPERATOR.
;;;;;;
;;;;;; Some hints: I also used PUSH and SETF and DOLIST a lot,
;;;;;; as well as a healthy dose of MAPCAR, MAPC, and MAPL.  Also you might
;;;;;; want to read up on CONSes of the form (a . b) which I use a lot,
;;;;;; and also ASSOCIATION LISTS which are very convenient in certain spots


(defun before-p (operator1 operator2 plan)
  "Operator1 is ordered before operator2 in plan?"
  (reachable plan operator1 operator2))

(defun link-exists-for-precondition-p (precond operator plan)
  "T if there's a link for the precond for a given operator, else nil.
precond is a predicate."
  (let* ((name (operator-name operator))
	(plan-length (length (plan-links plan)))
	(i 0))
   (loop while (< i plan-length)
	 do
	 (if (and (equal precond (link-precond (elt (plan-links plan) i))) (eq operator (link-to (elt (plan-links plan) i))))
	     (progn
	       (return-from link-exists-for-precondition-p T)))
	 (incf i))
   NIL))
  


(defun operator-threatens-link-p (operator link plan)
  "T if operator threatens link in plan, because it's not ordered after
or before the link, and it's got an effect which counters the link's effect."
  (let* ((start (link-from link))
	 (end (link-to link))
	 (orders (plan-orderings plan))
	 (not-precond (negate (link-precond link))))
    (if (or (eq operator start) (eq operator end)) (return-from operator-threatens-link-p nil))
    (if (or (before-p operator start orders) (before-p end operator orders)) (return-from operator-threatens-link-p nil))
    (loop for effect in (operator-effects operator)
	  do
	  (if (equal effect not-precond) (return-from operator-threatens-link-p T)))))




(defun inconsistent-p (plan)
  "Plan orderings are inconsistent"
  (cyclic-assoc-list (plan-orderings plan)))


(defun pick-precond (plan)
  "Return ONE (operator . precondition) pair in the plan that has not been met yet.
If there is no such pair, return nil"
  (dolist (operators (plan-operators plan))
    (dolist (preconditions (operator-preconditions operators))
      (if (not (link-exists-for-precondition-p preconditions operators plan))
	  (progn
	    (return-from pick-precond (cons operators preconditions))))))
  NIL)
;;; SPEED HINT.  Any precondition will work.  But this is an opportunity
;;; to pick a smart one.  Perhaps you might select the precondition
;;; which has the fewest possible operators which solve it, so it fails
;;; the fastest if it's wrong. 
  

(defun op-for-p (op precond)
  (member precond (operator-effects op) :test #'equal))

(defun all-effects (precondition plan)
  "Given a precondition, returns a list of ALL operators presently IN THE PLAN which have
effects which can achieve this precondition."
  (remove-if-not (lambda (op)
		   (op-for-p op precondition))
		 (plan-operators plan)))
;; hint: there's short, efficient way to do this, and a long,
  ;; grotesquely inefficient way.  Don't do the inefficient way.


(defun all-operators (precondition)
  "Given a precondition, returns all list of ALL operator templates which have
an effect that can achieve this precondition."
  (gethash precondition *operators-for-precond*))
  ;; hint: there's short, efficient way to do this, and a long,
  ;; grotesquely inefficient way.  Don't do the inefficient way.
  

(defun select-subgoal (plan current-depth max-depth)
  "For all possible subgoals, recursively calls choose-operator
on those subgoals.  Returns a solved plan, else nil if not solved."
  (if (>= current-depth max-depth)
      (return-from select-subgoal nil))
  (let ((op-precond-pair (pick-precond plan)))
    (if op-precond-pair
	(choose-operator op-precond-pair plan (+ 1 current-depth) max-depth)
	plan)))
          ;;; an enterprising student noted that the book says you DON'T have
          ;;; to nondeterministically choose from among all preconditions --
          ;;; you just pick one arbitrarily and that's all.  Note that the
          ;;; algorithm says "pick a plan step...", rather than "CHOOSE a
          ;;; plan step....".  This makes the algorithm much faster.  


(defun choose-operator (op-precond-pair plan current-depth max-depth)
  "For a given (operator . precondition) pair, recursively call
hook-up-operator for all possible operators in the plan.  If that
doesn't work, recursively call add operators and call hook-up-operators
on them.  Returns a solved plan, else nil if not solved."
  (let ((op (car op-precond-pair))
	(precond (cdr op-precond-pair)))
    (loop for op-solver in (all-effects precond plan)
	  do
	  (let ((hook-up-result (hook-up-operator op-solver op precond (copy-plan plan) current-depth max-depth nil)))
	    (if hook-up-result
		(return-from choose-operator hook-up-result))))
    (loop for new-op-solver in (all-operators precond)
	  do
	  (let* ((new-op (copy-operator new-op-solver))
		 (holder-plan (add-operator new-op plan))
		 (hook-up-result (hook-up-operator new-op op precond holder-plan current-depth max-depth t)))
	    (if hook-up-result
		(return-from choose-operator hook-up-result))))
    nil))
  
(defun add-operator (operator plan)
  "Given an OPERATOR and a PLAN makes a copy of the plan [the
operator should have already been copied out of its template at this point].
Then adds that copied operator
the copied plan, and hooks up the orderings so that the new operator is
after start and before goal.  Returns the modified copy of the plan."
  (let* ((holder-plan (copy-plan plan))
	 (start (plan-start holder-plan))
	 (goal (plan-goal holder-plan)))
    (pushnew operator (plan-operators holder-plan))
    (pushnew (cons start operator) (plan-orderings holder-plan))
    (pushnew (cons operator goal) (plan-orderings holder-plan))
    holder-plan))
  
  ;;; hint: make sure you copy the plan!
  ;;; also hint: use PUSHNEW to add stuff but not duplicates
  ;;; Don't use PUSHNEW everywhere instead of PUSH, just where it
  ;;; makes specific sense.
  

(defun hook-up-operator (from to precondition plan
                         current-depth max-depth
                         new-operator-was-added)
  "Hooks up an operator called FROM, adding the links and orderings to the operator
TO for the given PRECONDITION that FROM achieves for TO.  Then
recursively  calls resolve-threats to fix any problems.  Presumes that
PLAN is a copy that can be modified at will by HOOK-UP-OPERATOR. Returns a solved
plan, else nil if not solved."
  (if (before-p to from (plan-orderings plan))
      (return-from hook-up-operator nil))
  (let ((link (make-link :from from :precond precondition :to to)))
    (pushnew link (plan-links plan))
    (if (not (before-p from to (plan-orderings plan)))
	(pushnew (cons from to) (plan-orderings plan)))
    (if (inconsistent-p plan)
	(return-from hook-up-operator nil))
    (resolve-threats plan
		     (threats plan
			      (if new-operator-was-added from)
			      link)
		     current-depth max-depth)))
        
  ;;; hint: want to go fast?  The first thing you should do is
  ;;; test to see if TO is already ordered before FROM and thus
  ;;; hooking them up would make the plan inconsistent from the get-go
  ;;; also hint: use PUSHNEW to add stuff but not duplicates  
  ;;; Don't use PUSHNEW everywhere instead of PUSH, just where it
  ;;; makes specific sense.


(defun threats (plan maybe-threatening-operator maybe-threatened-link)
  "After hooking up an operator, we have two places that we need to check for threats.
First, we need to see if the link we just created is threatened by some operator.
Second, IF we just added in an operator, then we need to check to see if it threatens
any links.

This function should return a list of (op . link) pairs (called ''threats'') which
indicate all the situations where some operator OP threatens a link LINK.  The only
situations you need to check are the ones described in the previous paragraph.

This function should assume that if MAYBE-THREATENING-OPERATOR is NIL, then no
operator was added and we don't have to check for its threats.  However, we must
always check for any operators which threaten MAYBE-THREATENED-LINK."
  (let ((threats)
	(operators (plan-operators plan))
	(links (plan-links plan)))
    (loop for op in operators
	  do
	  (if (operator-threatens-link-p op maybe-threatened-link plan) (push (cons op maybe-threatened-link) threats)))
    (if maybe-threatening-operator
	(loop for link in links
	      do
	      (if (operator-threatens-link-p maybe-threatening-operator link plan) (pushnew (cons maybe-threatening-operator link) threats))))
    threats))



(defun all-promotion-demotion-plans (plan threats)
  "Returns plans for each combination of promotions and demotions
of the given threats, except  for the inconsistent plans.  These plans
are copies of the original plan."
  ;;; Hint: binary-combinations could be useful to you.
  ;;; Also check out MAPC
  ;;; SPEED HINT.  You might handle the one-threat case specially.
  ;;; In that case you could also check for inconsistency right then and there too.
  (if (= 1 (length threats))
      (let ((plans)
	    (threat (first threats))
	    (promote-copy (copy-plan plan))
	    (demote-copy (copy-plan plan)))
	(promote (car threat) (cdr threat) promote-copy)
	(demote (car threat) (cdr threat) demote-copy)
	(if (not (inconsistent-p promote-copy)) (push promote-copy plans))
	(if (not (inconsistent-p demote-copy)) (push demote-copy plans))
	plans)
      (if (= 0 (length threats))
	  (return-from all-promotion-demotion-plans (list (copy-plan plan)))
	  (let ((plans)
		(holder-plan)
		(assignments (binary-combinations (length threats))))
	    (loop for assignment in assignments
		  do
		     (setq holder-plan (copy-plan plan))
		     (loop for do-promote in assignment
			   for threat in threats
			   do
			      (if do-promote
				  (promote (car threat) (cdr threat) holder-plan)
				  (demote (car threat) (cdr threat) holder-plan)))
		     (if (not (inconsistent-p holder-plan)) (push holder-plan plans)))
	    plans))))

		    

(defun promote (operator link plan)
  "Promotes an operator relative to a link.  Doesn't copy the plan."
  (push (cons (link-to link) operator) (plan-orderings plan)))

(defun demote (operator link plan)
  "Demotes an operator relative to a link.  Doesn't copy the plan."
  (push (cons operator (link-from link)) (plan-orderings plan)))

(defun resolve-threats (plan threats current-depth max-depth)
  "Tries all combinations of solutions to all the threats in the plan,
then recursively calls SELECT-SUBGOAL on them until one returns a
solved plan.  Returns the solved plan, else nil if no solved plan."
  (if (not threats)
      (return-from resolve-threats (select-subgoal plan current-depth max-depth)))
  (let ((possible-plans (all-promotion-demotion-plans plan threats)))
    (loop for plan-fix in possible-plans
	  do
	  (let ((selection (select-subgoal plan-fix current-depth max-depth)))
	    (if selection (return-from resolve-threats selection))))
  nil))

;;;;;;;;; WORLD-SELECT
;;;;
;;;;   Chooses what world we're planning in.
;;;;


(defun world-select (world)
  (cond
    ((string= world "two-block")
     (progn
       (setf *operators* *two-block-operators*)
       (setf *start-effects* *two-block-start-effects*)
       (setf *goal-preconditions* *two-block-goal-preconditions*)))
    ((string= world "three-block-regular")
     (progn
       (setf *operators* *three-block-operators*)
       (setf *start-effects* *three-block-start-effects*)
       (setf *goal-preconditions* *three-block-goal-preconditions*)))
  ((string= world "three-block-sussman")
     (progn
       (setf *operators* *three-block-operators*)
       (setf *start-effects* *sussman-start-effects*)
       (setf *goal-preconditions* *three-block-goal-preconditions*)))
  ((string= world "simple-spare-tire")
     (progn
       (setf *operators* *simple-spare-tire-operators*)
       (setf *start-effects* *simple-spare-tire-start-effects*)
       (setf *goal-preconditions* *simple-spare-tire-goal-preconditions*)))
  ((string= world "complex-spare-tire")
     (progn
       (setf *operators* *complex-spare-tire-operators*)
       (setf *start-effects* *complex-spare-tire-start-effects*)
       (setf *goal-preconditions* *complex-spare-tire-goal-preconditions*)))
  (t (print "Error: Didn't recognize that world, consult the report for usage.")))
  (build-operators-for-precond))




;;;;;;; DO-POP
;;;;;;; This is the high-level code.  Note it creates a goal and a start
;;;;;;; operator, then creates a plan with those operators and an ordering
;;;;;;; between them.  Then it does iterative-deepening, calling
;;;;;;; SELECT-SUBGOAL with ever-larger maximum depths.  If the solution is
;;;;;;; non-null, it breaks out of the loop and returns the solution.
;;;;;;;
;;;;;;; One thing you should note is that DO-POP also builds a little hash
;;;;;;; table called *operators-for-precond*.  I think you will find this
;;;;;;; useful in one of your functions.


(defparameter *depth-increment* 1
  "The depth to increment in iterative deepening search")

;;; This is used to cache the operators by precond.  You might find this
;;; useful to make your ALL-OPERATORS code much much much faster than the
;;; obvious dorky way to do it.
(defparameter *operators-for-precond* nil
  "Hash table.  Will yield a list of operators which can achieve a given precondition")

(defun build-operators-for-precond ()
  "Buils the hash table"
  (setf *operators-for-precond* (make-hash-table :test #'equalp))
  (dolist (operator *operators*)
    (dolist (effect (operator-effects operator))
      (push operator (gethash effect *operators-for-precond*)))))

(defun do-pop-test ()
  (let* ((start (make-operator
                 :name 'start
                 :uniq (gensym)
                 :preconditions nil
                 :effects *start-effects*))
         (goal (make-operator
                :name 'goal
                :uniq (gensym)
                :preconditions *goal-preconditions*
                :effects nil))
         (plan (make-plan
                :operators (list start goal)
                :orderings (list (cons start goal))
                :links nil
                :start start
                :goal goal)))
    (build-operators-for-precond)
    (select-subgoal plan 1 1)))

(defun do-pop ()
  (let* ((start (make-operator
                 :name 'start
                 :uniq (gensym)
                 :preconditions nil
                 :effects *start-effects*))
         (goal (make-operator
                :name 'goal
                :uniq (gensym)
                :preconditions *goal-preconditions*
                :effects nil))
         (plan (make-plan
                :operators (list start goal)
                :orderings (list (cons start goal))
                :links nil
                :start start
                :goal goal))
         (depth *depth-increment*)
         solution)
    (build-operators-for-precond)
    ;; Do iterative deepening search on this sucker
    (loop
       (format t "~%Search Depth: ~d" depth)
       (setf solution (select-subgoal plan 0 depth))
       (when solution (return)) ;; break from loop, we're done!
       (incf depth *depth-increment*))
    ;; found the answer if we got here
    (format t "~%Solution Discovered:~%~%")
    solution))



;;;;; TWO-BLOCK-WORLD
;;;;; You have two blocks on the table, A and B.   Pretty simple, no?
(defparameter *two-block-operators*
  (list
   ;; move from table operators
   (make-operator :name 'a-table-to-b
                  :preconditions '((t a-on-table) (t b-clear) (t a-clear))
                  :effects '((nil a-on-table) (nil b-clear) (t a-on-b)))
   (make-operator :name 'b-table-to-a
                  :preconditions '((t b-on-table) (t a-clear) (t b-clear))
                  :effects '((nil b-on-table) (nil a-clear) (t b-on-a)))
   ;; move to table operators
   (make-operator :name 'a-b-to-table
                  :preconditions '((t a-on-b) (t a-clear))
                  :effects '((t a-on-table) (nil a-on-b) (t b-clear)))
   (make-operator :name 'b-a-to-table
                  :preconditions '((t b-on-a) (t b-clear))
                  :effects '((t b-on-table) (nil b-on-a) (t a-clear))))
  "A list of strips operators without their uniq gensyms set yet -- 
doesn't matter really -- but NOT including a goal or start operator")


;;; b is on top of a
(defparameter *two-block-start-effects*
  '((t a-on-table) (t b-on-a) (t b-clear)))

;;; a is on top of b
(defparameter *two-block-goal-preconditions*
  ;; somewhat redundant, is doable with just ((t a-on-b))
  '((t a-on-b) (t b-on-table) (t a-clear)))

;;;;; Solution to 2-BLOCK-WORLD on SBCL: 

;; * (do-pop)
;;
;; Search Depth: 1
;; Search Depth: 2
;; Search Depth: 3
;; Search Depth: 4
;; Search Depth: 5
;; Search Depth: 6
;; Search Depth: 7
;; Search Depth: 8
;; Search Depth: 9
;; Solution Discovered:
;;
;; #< PLAN operators: 
;; #S(OPERATOR
;;    :NAME A-TABLE-TO-B
;;    :UNIQ G251529
;;    :PRECONDITIONS ((T A-ON-TABLE) (T B-CLEAR) (T A-CLEAR))
;;    :EFFECTS ((NIL A-ON-TABLE) (NIL B-CLEAR) (T A-ON-B)))
;; #S(OPERATOR
;;    :NAME B-A-TO-TABLE
;;    :UNIQ G251527
;;    :PRECONDITIONS ((T B-ON-A) (T B-CLEAR))
;;    :EFFECTS ((T B-ON-TABLE) (NIL B-ON-A) (T A-CLEAR)))
;; #S(OPERATOR
;;    :NAME SpTART
;;    :UNIQ G251258
;;    :PRECONDITIONS NIL
;;    :EFFECTS ((T A-ON-TABLE) (T B-ON-A) (T B-CLEAR)))
;; #S(OPERATOR
;;    :NAME GOAL
;;    :UNIQ G251259
;;    :PRECONDITIONS ((T A-ON-B) (T B-ON-TABLE) (T A-CLEAR))
;;    :EFFECTS NIL) 
;; links: 
;; #< (G251258)START -> (G251529)A-TABLE-TO-B : (T A-ON-TABLE) >
;; #< (G251258)START -> (G251529)A-TABLE-TO-B : (T B-CLEAR) >
;; #< (G251527)B-A-TO-TABLE -> (G251529)A-TABLE-TO-B : (T A-CLEAR) >
;; #< (G251258)START -> (G251527)B-A-TO-TABLE : (T B-ON-A) >
;; #< (G251258)START -> (G251527)B-A-TO-TABLE : (T B-CLEAR) >
;; #< (G251529)A-TABLE-TO-B -> (G251259)GOAL : (T A-ON-B) >
;; #< (G251527)B-A-TO-TABLE -> (G251259)GOAL : (T B-ON-TABLE) >
;; #< (G251527)B-A-TO-TABLE -> (G251259)GOAL : (T A-CLEAR) > 
;; orderings: 
;; #[ (G251527)B-A-TO-TABLE -> (G251529)A-TABLE-TO-B ]
;; #[ (G251529)A-TABLE-TO-B -> (G251259)GOAL ]
;; #[ (G251258)START -> (G251529)A-TABLE-TO-B ]
;; #[ (G251527)B-A-TO-TABLE -> (G251259)GOAL ]
;; #[ (G251258)START -> (G251527)B-A-TO-TABLE ]
;; #[ (G251258)START -> (G251259)GOAL ]
;; >








;;;;;; THREE-BLOCK-WORLD
;;;;;; You have three blocks on the table, A, B, and C.
;;;;;;
;;;;;;
;;;
;;; Why so many operators?  Because we don't have a variable facility.
;;; We can't say MOVE(x,y,z) -- we can only say MOVE(A,TABLE,B).  To
;;; add in a variable facility is a lot more coding, and I figured I'd
;;; save you the hassle of unification.  If you want to give it a shot,
;;; I have written up some unification code which might help you out.
;;; Another consequence of not having a variable facility is that you
;;; can't rely on the least-commitment heuristic of not immediately
;;; binding variables to constants.  For us, we must *immediately*
;;; commit to constants.  That makes our search space much nastier.
;;; C'est la vie!
;;;
(defparameter *three-block-operators*
   (list
    ;; move from table operators
    (make-operator :name 'a-table-to-b
                :preconditions '((t a-on-table) (t b-clear) (t a-clear))
                :effects '((nil a-on-table) (nil b-clear) (t a-on-b)))
    (make-operator :name 'a-table-to-c
                :preconditions '((t a-on-table) (t c-clear) (t a-clear))
                :effects '((nil a-on-table) (nil c-clear) (t a-on-c)))
    (make-operator :name 'b-table-to-a
                :preconditions '((t b-on-table) (t a-clear) (t b-clear))
                :effects '((nil b-on-table) (nil a-clear) (t b-on-a)))
    (make-operator :name 'b-table-to-c
                :preconditions '((t b-on-table) (t c-clear) (t b-clear))
                :effects '((nil b-on-table) (nil c-clear) (t b-on-c)))
    (make-operator :name 'c-table-to-a
                :preconditions '((t c-on-table) (t a-clear) (t c-clear))
                :effects '((nil c-on-table) (nil a-clear) (t c-on-a)))
    (make-operator :name 'c-table-to-b
                :preconditions '((t c-on-table) (t b-clear) (t c-clear))
                :effects '((nil c-on-table) (nil b-clear) (t c-on-b)))
    ;; move to table operators
    (make-operator :name 'a-b-to-table
                :preconditions '((t a-on-b) (t a-clear))
                :effects '((t a-on-table) (nil a-on-b) (t b-clear)))
    (make-operator :name 'a-c-to-table
                :preconditions '((t a-on-c) (t a-clear))
                :effects '((t a-on-table) (nil a-on-c) (t c-clear)))
    (make-operator :name 'b-a-to-table
                :preconditions '((t b-on-a) (t b-clear))
                :effects '((t b-on-table) (nil b-on-a) (t a-clear)))
    (make-operator :name 'b-c-to-table
                :preconditions '((t b-on-c) (t b-clear))
                :effects '((t b-on-table) (nil b-on-c) (t c-clear)))
    (make-operator :name 'c-a-to-table
                :preconditions '((t c-on-a) (t c-clear))
                :effects '((t c-on-table) (nil c-on-a) (t a-clear)))
    (make-operator :name 'c-b-to-table
                :preconditions '((t c-on-b) (t c-clear))
                :effects '((t c-on-table) (nil c-on-b) (t b-clear)))
    ;; block-to-block operators
    (make-operator :name 'a-b-to-c
                :preconditions '((t a-on-b) (t a-clear) (t c-clear))
                :effects '((nil a-on-b) (t a-on-c) (nil c-clear) (t b-clear)))
    (make-operator :name 'a-c-to-b
                :preconditions '((t a-on-c) (t a-clear) (t b-clear))
                :effects '((nil a-on-c) (t a-on-b) (nil b-clear) (t c-clear)))
    (make-operator :name 'b-a-to-c
                :preconditions '((t b-on-a) (t b-clear) (t c-clear))
                :effects '((nil b-on-a) (t b-on-c) (nil c-clear) (t a-clear)))
    (make-operator :name 'b-c-to-a
                :preconditions '((t b-on-c) (t b-clear) (t a-clear))
                :effects '((nil b-on-c) (t b-on-a) (nil a-clear) (t c-clear)))
    (make-operator :name 'c-a-to-b
                :preconditions '((t c-on-a) (t c-clear) (t b-clear))
                :effects '((nil c-on-a) (t c-on-b) (nil b-clear) (t a-clear)))
    (make-operator :name 'c-b-to-a
                :preconditions '((t c-on-b) (t c-clear) (t a-clear))
                :effects '((nil c-on-b) (t c-on-a) (nil a-clear) (t b-clear))))
   "A list of strips operators without their uniq gensyms set yet -- 
 doesn't matter really -- but NOT including a goal or start operator")

(defparameter *sussman-start-effects*
   ;; Sussman Anomaly
   '((t a-on-table) (t b-on-table) (t c-on-a) (t b-clear) (t c-clear))
   "A list of predicates which specify the initial state")

(defparameter *three-block-start-effects*
   ;; another simple situation: all on table
   '((t a-on-table) (t a-clear)
     (t b-on-table) (t b-clear)
     (t c-on-table) (t c-clear))) 

 (defparameter *three-block-goal-preconditions*
   '((t a-on-b) (t b-on-c) (t c-on-table) (t a-clear)))




;;;; An Solution to 3-BLOCK-WORLD, Sussman Anomaly, in SBCL:


;; * (do-pop)
;;
;; Search Depth: 1
;; Search Depth: 2
;; Search Depth: 3
;; Search Depth: 4
;; Search Depth: 5
;; Search Depth: 6
;; Search Depth: 7
;; Search Depth: 8
;; Search Depth: 9
;; Search Depth: 10
;; Search Depth: 11
;; Search Depth: 12
;; Search Depth: 13
;; Solution Discovered:
;;
;; #< PLAN operators: 
;; #S(OPERATOR
;;    :NAME B-TABLE-TO-C
;;    :UNIQ G251257
;;    :PRECONDITIONS ((T B-ON-TABLE) (T C-CLEAR) (T B-CLEAR))
;;    :EFFECTS ((NIL B-ON-TABLE) (NIL C-CLEAR) (T B-ON-C)))
;; #S(OPERATOR
;;    :NAME A-TABLE-TO-B
;;    :UNIQ G225453
;;    :PRECONDITIONS ((T A-ON-TABLE) (T B-CLEAR) (T A-CLEAR))
;;    :EFFECTS ((NIL A-ON-TABLE) (NIL B-CLEAR) (T A-ON-B)))
;; #S(OPERATOR
;;    :NAME C-A-TO-TABLE
;;    :UNIQ G182393
;;    :PRECONDITIONS ((T C-ON-A) (T C-CLEAR))
;;    :EFFECTS ((T C-ON-TABLE) (NIL C-ON-A) (T A-CLEAR)))
;; #S(OPERATOR
;;    :NAME START
;;    :UNIQ G616
;;    :PRECONDITIONS NIL
;;    :EFFECTS ((T A-ON-TABLE) (T B-ON-TABLE) (T C-ON-A) (T B-CLEAR) (T C-CLEAR)))
;; #S(OPERATOR
;;    :NAME GOAL
;;    :UNIQ G617
;;    :PRECONDITIONS ((T B-ON-C) (T A-ON-B) (T A-CLEAR) (T C-ON-TABLE))
;;    :EFFECTS NIL) 
;; links: 
;; #< (G616)START -> (G251257)B-TABLE-TO-C : (T C-CLEAR) >
;; #< (G616)START -> (G251257)B-TABLE-TO-C : (T B-CLEAR) >
;; #< (G616)START -> (G225453)A-TABLE-TO-B : (T B-CLEAR) >
;; #< (G182393)C-A-TO-TABLE -> (G225453)A-TABLE-TO-B : (T A-CLEAR) >
;; #< (G616)START -> (G182393)C-A-TO-TABLE : (T C-CLEAR) >
;; #< (G182393)C-A-TO-TABLE -> (G617)GOAL : (T A-CLEAR) >
;; #< (G616)START -> (G251257)B-TABLE-TO-C : (T B-ON-TABLE) >
;; #< (G616)START -> (G225453)A-TABLE-TO-B : (T A-ON-TABLE) >
;; #< (G616)START -> (G182393)C-A-TO-TABLE : (T C-ON-A) >
;; #< (G251257)B-TABLE-TO-C -> (G617)GOAL : (T B-ON-C) >
;; #< (G225453)A-TABLE-TO-B -> (G617)GOAL : (T A-ON-B) >
;; #< (G182393)C-A-TO-TABLE -> (G617)GOAL : (T C-ON-TABLE) > 
;; orderings: 
;; #[ (G251257)B-TABLE-TO-C -> (G225453)A-TABLE-TO-B ]
;; #[ (G182393)C-A-TO-TABLE -> (G225453)A-TABLE-TO-B ]
;; #[ (G182393)C-A-TO-TABLE -> (G251257)B-TABLE-TO-C ]
;; #[ (G251257)B-TABLE-TO-C -> (G617)GOAL ]
;; #[ (G616)START -> (G251257)B-TABLE-TO-C ]
;; #[ (G225453)A-TABLE-TO-B -> (G617)GOAL ]
;; #[ (G616)START -> (G225453)A-TABLE-TO-B ]
;; #[ (G182393)C-A-TO-TABLE -> (G617)GOAL ]
;; #[ (G616)START -> (G182393)C-A-TO-TABLE ]
;; #[ (G616)START -> (G617)GOAL ]
;; >


;;;;; SIMPLE-SPARE-TIRE-WORLD
;;;;; Simple spare tire problem, lifted from 11.1 in the posted book

(defparameter *simple-spare-tire-operators*
  (list
   (make-operator :name 'spare-off-trunk
		  :uniq (gensym)
		  :preconditions '((t spare-at-trunk))
		  :effects '((nil spare-at-trunk)
			     (t spare-at-ground)))
   (make-operator :name 'flat-off-axle
		  :uniq (gensym)
		  :preconditions '((t flat-at-axle))
		  :effects '((nil flat-at-axle)
			     (t flat-at-ground)))
   (make-operator :name 'put-spare-on-axle
		  :uniq (gensym)
		  :preconditions '((t spare-at-ground)
				   (nil flat-at-axle))
		  :effects '((nil spare-at-ground)
			     (t spare-at-axle)))
   (make-operator :name 'leave-overnight
		  :uniq (gensym)
		  :preconditions nil
		  :effects '((nil spare-at-ground)
			     (nil spare-at-axle)
			     (nil spare-at-trunk)
			     (nil flat-at-ground)
			     (nil flat-at-axle)))))

(defparameter *simple-spare-tire-start-effects*
  '((t flat-at-axle)
    (t spare-at-trunk)))

(defparameter *simple-spare-tire-goal-preconditions*
  '((t spare-at-axle)))
     
		  
;;;;; COMPLEX-SPARE-TIRE-WORLD
;;;;; An extension of the simple spare tire problem from the book.
;;;;; More operations are allowed, like putting the flat in the trunk,
;;;;; putting the flat back on the axel, putting the spare in the trunk, etc.

(defparameter *complex-spare-tire-operators*
  (list
   (make-operator :name 'spare-off-trunk
		  :uniq (gensym)
		  :preconditions '((t spare-at-trunk))
		  :effects '((nil spare-at-trunk)
			     (t spare-at-ground)))
   (make-operator :name 'put-spare-on-trunk
		  :uniq (gensym)
		  :preconditions '((t spare-at-ground)
				   (nil flat-at-trunk))
		  :effects '((nil spare-at-ground)
			     (t spare-at-trunk)))
   (make-operator :name 'spare-off-axle
		  :uniq (gensym)
		  :preconditions '((t spare-at-axle))
		  :effects '((nil spare-at-axle)
			     (t spare-at-ground)))
   (make-operator :name 'put-spare-on-axle
		  :uniq (gensym)
		  :preconditions '((t spare-at-ground)
				   (nil flat-at-axle))
		  :effects '((nil spare-at-ground)
			     (t spare-at-axle)))
   (make-operator :name 'flat-off-trunk
		  :uniq (gensym)
		  :preconditions '((t flat-at-trunk))
		  :effects '((nil flat-at-trunk)
			     (t flat-at-ground)))
   (make-operator :name 'put-flat-on-trunk
		  :uniq (gensym)
		  :preconditions '((t flat-at-ground)
				   (nil spare-at-trunk))
		  :effects '((t flat-at-trunk)
			     (nil flat-at-ground)))
   (make-operator :name 'flat-off-axle
		  :uniq (gensym)
		  :preconditions '((t flat-at-axle))
		  :effects '((nil flat-at-axle)
			     (t flat-at-ground)))
   (make-operator :name 'put-flat-on-axle
		  :uniq (gensym)
		  :preconditions '((t flat-at-ground)
				   (nil spare-at-axle))
		  :effects '((t flat-at-axle)
			     (nil flat-at-ground)))
   (make-operator :name 'leave-overnight
		  :uniq (gensym)
		  :preconditions nil
		  :effects '((nil spare-at-ground)
			     (nil spare-at-axle)
			     (nil spare-at-trunk)
			     (nil flat-at-ground)
			     (nil flat-at-axle)
			     (nil flat-at-trunk)))))

(defparameter *complex-spare-tire-start-effects*
  '((t flat-at-axle)
    (nil flat-at-ground)
    (nil flat-at-trunk)
    (t spare-at-trunk)
    (nil spare-at-ground)
    (nil spare-at-axle)))

(defparameter *complex-spare-tire-goal-preconditions*
  '((t spare-at-axle)
    (t flat-at-trunk)))
     

  



;;;;; COMPILATION WARNINGS

;; You should have few warnings from compilation, all of them spurious.  These
;; are due entirely to missing functions (because they're defined later in the file)
;; and undeclared global variables (because they're defined later in the file).
;; SBCL complains about these:

;; in: DEFSTRUCT LINK
;; caught STYLE-WARNING:
;;   undefined function: PRINT-LINK
;; 
;; in: DEFSTRUCT PLAN
;; caught STYLE-WARNING:
;;   undefined function: PRINT-PLAN
;; 
;; in: DEFUN PICK-PRECOND
;; caught STYLE-WARNING:
;;   undefined function: NUM-ALL-OPERATORS
;; 
;; in: DEFUN ALL-OPERATORS
;; caught WARNING:
;;   undefined variable: *OPERATORS-FOR-PRECOND*
;; 
;; in: DEFUN NUM-ALL-OPERATORS
;; caught WARNING:
;;   undefined variable: *NUM-OPERATORS-FOR-PRECOND*
;; 
;; in: DEFUN SELECT-SUBGOAL
;; caught STYLE-WARNING:
;;   undefined function: CHOOSE-OPERATOR
;; 
;; in: DEFUN CHOOSE-OPERATOR
;; caught STYLE-WARNING:
;;   undefined function: ADD-OPERATOR
;; caught STYLE-WARNING:
;;   undefined function: HOOK-UP-OPERATOR
;; 
;; in: DEFUN HOOK-UP-OPERATOR
;; caught STYLE-WARNING:
;;   undefined function: RESOLVE-THREATS
;; caught STYLE-WARNING:
;;   undefined function: THREATS
;; 
;; in: DEFUN ALL-PROMOTION-DEMOTION-PLANS
;; caught STYLE-WARNING:
;;   undefined function: DEMOTE
;; caught STYLE-WARNING:
;;   undefined function: PROMOTE
;; 
;; in: DEFUN BUILD-OPERATORS-FOR-PRECOND
;; caught WARNING:
;;   undefined variable: *OPERATORS*
;; 
;; in: DEFUN DO-POP
;; caught WARNING:
;;   undefined variable: *GOAL-PRECONDITIONS*
;; caught WARNING:
;;   undefined variable: *START-EFFECTS*

;; ... I really oughta rearrange the code so these go away.  :-)
