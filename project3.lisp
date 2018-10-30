;;;;;DO NOT TURN IN UNTIL FUNCTION NAMES ARE RIGHT!!!!!!!!

;;;List Functions

(defun append (a b)
  (if (null a)
	b
	(cons (car a) (append (cdr a) b))))

(defun reverse (a)
  (if (null a)
	'()
	(append (reverse (cdr a)) (list (car a)))))

(defun map (f l)
  (if (null l) '()
	(append (list (funcall f (car l))) (map f (cdr l)))))

(defun nub (l)
  (cond 
	((null l) '())
	((member (car l) (cdr l))
			  (nub (cdr l)))
	(T (cons (car l) (nub (cdr l))))))

(defun fold (i f l)
  (cond
	((null l) i)
	(T(funcall f (fold i f (cdr l)) (car l)))))


;;;Set Functions

(defun member (x s)
  (cond
	((null s) nil)
	((= (car s) x) t)
	(t (member x (cdr s)))))

(defun insert (x s)
  (cond
	((member x s) s)
	(T (cons x s))))

(defun intersection (a b)
  (cond
	((null a) nil)
	((null b) nil)
	((member (car a) b) (cons (car a) (intersection (cdr a) b)))
	(T (intersection (cdr a) b))))

(defun union (a b)
  (cond 
	((and (null a) (null b)) nil)
	((null a) b)
	(T (nub (cons (car a) (union (cdr a) b))))))

;;;Math Functions

(defun factorial (n) 
  (if (or (= n 1) (= n 0)) 
	1 
	(* n (factorial (- n 1)))))

(defun abs (n)
  (if (<= 0 n)
	n
	(* -1 n)))

(defun right-tri (a b c)
  (= (square c) (+ (square a) (square b))))

(defun square (x)
  (* x x))

(defun lcm (a b)
  (lcm_help a b 1))

(defun lcm_help (a b x)
  (if (and (= 0 (mod x a)) (= 0 (mod x b)))
  x
  (lcm_help a b (+ x 1))
  ))

(defun mod (a b)
  (if (or (= 0 a) (= 0 b))
	0
  	(if (< a b)
		a
		(mod (- a b) b))))

(defun gcd (a b)
  (if (= 0 b)
	a
	(gcd b (mod a b))))

;return a list of factors. 
(defun factor (x n l)
  (cond
	((= x n) l)
	((= (mod x n) 0) (factor x (+ n 1) (cons n l)))
	((/= (mod x n) 0) (factor x (+ n 1) l))
	(t l)))


;;;Required Functions

(defun perfectp (x)
  (= x (fold 0 '+ (factor x 1 nil))))

(defun abundantp (x)
  (< x (fold 0 '+ (factor x 1 nil))))

(defun deficientp (x)
  (> x (fold 0 '+ (factor x 1 nil))))
