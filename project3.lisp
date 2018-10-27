;;;;;DO NOT TURN IN UNTIL FUNCTION NAMES ARE RIGHT!!!!!!!!

;;;List Functions

(defun mappend (a b)
  (if (null a)
	b
	(cons (car a) (mappend (cdr a) b))))

(defun mreverse (a)
  (if (null a)
	'()
	(mappend (mreverse (cdr a)) (list (car a)))))

(defun mmap (f l)
  (if (null l) '()
	(mappend (list (funcall f (car l))) (mmap f (cdr l)))))

(defun nub (l)
  (cond 
	((null l) '())
	((mmember (car l) (cdr l))
			  (nub (cdr l)))
	(T (cons (car l) (nub (cdr l))))))

(defun fold (i f l)
  (cond
	((null l) i)
	(T(funcall f (fold i f (cdr l)) (car l)))))

;;;Set Functions

(defun mmember (x s)
  (cond
	((null s) nil)
	((= (car s) x) t)
	(t (mmember x (cdr s)))))

;;;Math Functions

(defun factorial (n) 
  (if (or (= n 1) (= n 0)) 
	1 
	(* n (factorial (- n 1)))))

(defun mabs (n)
  (if (<= 0 n)
	n
	(* -1 n)))

(defun right-tri (a b c)
  (= (square c) (+ (square a) (square b))))

(defun square (x)
  (* x x))

(defun mlcm (a b)
  (lcm_help a b 1))

(defun lcm_help (a b x)
  (if (and (= 0 (mmod x a)) (= 0 (mmod x b)))
  x
  (lcm_help a b (+ x 1))
  ))

(defun mmod (a b)
  (if (or (= 0 a) (= 0 b))
	0
  	(if (< a b)
		a
		(mmod (- a b) b))))

(defun mgcd (a b)
  (if (= 0 b)
	a
	(mgcd b (mmod a b))))

(defun primep (n)
  (if (= n 2)
	(= 0 0)
  	(relprimep n (- n 1))))

(defun relprimep (n d)
  (if (= d 1)
	(= 1 1)
	(if (mmod n d)
	  (= 0 1)
	  (relprimep n (- d 1)))))

;;;Required Functions

(defun perfectp (n)
  (= n ()))
