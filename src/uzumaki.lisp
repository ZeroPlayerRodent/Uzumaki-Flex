#!/usr/bin/clisp

; UZUMAKI IN COMMON LISP BY ZEROPLAYERRODENT
; Documentation is found here: https://esolangs.org/wiki/Uzumaki

(defun get-file-contents (filename) ; Function that gets contents of a file as a string
  (with-open-file (stream filename)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      contents)
    )
)

(defparameter *codestring* (get-file-contents (elt *args* 0))) ; Define initial string of code
(defparameter *array* (make-array 0 :fill-pointer 0 :adjustable t)) ; Define array that holds spiral

(let ((i -1)(subarray (make-array 0 :fill-pointer 0 :adjustable t))) ; Process code into 2D array
  (loop
    (loop
      (setf i (+ 1 i))
      (if(char= (char *codestring* i) #\Newline)(return)())
      (vector-push-extend (char *codestring* i) subarray)
    )
    (vector-push-extend subarray *array*)
    (setf subarray (make-array 0 :fill-pointer 0 :adjustable t))
    (if (= i (- (length *codestring*) 1))(return)())
  )
)

(defun uh-oh(message) ; Error function
  (format t message)
  (exit)
)

(let ((i 0)) ; Check if spiral is perfect
  (loop
    (if (not(= (length *array*) (length (elt *array* 0))))(uh-oh "ERROR: NOT A PERFECT SPIRAL.")())
    (setf i (+ i 1))
    (if (> i (length *array*))(return)())
  )
)

(defmacro curchar() ; Macro for current character the IP is on
  '(elt(elt *array* *y*)*x*)
)

(defmacro curdir() ; Macro for the current direction the IP should move
  '(aref *movespiral* *y* *x*)
)

(defparameter *x* 0) ; Horizontal position of IP
(defparameter *y* 0) ; Vertical position of IP

(defparameter *dir* 6) ; Variable that stores the current direction the IP should move

(defun rotate(amount) ; Function that rotates the IP's direction 90 degrees
  (cond ((= *dir* 6)(setf *dir* 8))
        ((= *dir* 8)(setf *dir* 4))
        ((= *dir* 4)(setf *dir* 2))
        ((= *dir* 2)(setf *dir* 6))
  )
  (setf *times* (+ amount *times*))
)

(defun move(amount) ; Function that moves the IP in its current direction
  (if (= *dir* 6)(setf *x* (+ *x* amount))())
  (if (= *dir* 8)(setf *y* (+ *y* amount))())
  (if (= *dir* 4)(setf *x* (- *x* amount))())
  (if (= *dir* 2)(setf *y* (- *y* amount))())
)

(defun bigjumpout() ; Function that sets the layer of the IP to 1
  (setf *jumping* 1)
  (cond ((= *dir* 2)(setf *x* 0))
        ((= *dir* 8)(setf *x* *edge*))
        ((= *dir* 4)(setf *y* *edge*))
        ((= *dir* 6)(setf *y* 0))
  )
)

(defun jumpout() ; Function that makes the IP jump outward
  (setf *jumping* 1)
  (rotate 0)
  (rotate 0)
  (rotate 0)
  (move 1)
  (loop
    (if(not(= 0 (curdir)))(return)())
    (move 1)
  )
  (setf *dir* (curdir))
)

(defun jumpin() ; Function that makes the IP jump inward
  (setf *jumping* 1)
  (rotate 0)
  (move 1)
  (loop
    (if(not(= 0 (curdir)))(return)())
    (move 1)
    (if(= *x* -1)(uh-oh "ERROR: THE IP FLEW AWAY, NEVER TO BE SEEN AGAIN.")())
  )
  (setf *dir* (curdir))
)

(defun skip() ; Function for skipping a command
  (move 1)
  (if(not(= (curdir) 9))(setf *dir* (curdir))())
)

(defparameter *queue* (make-array 1 :fill-pointer 1 :adjustable t :initial-element 0)) ; Uzumaki memory queue
(defparameter *acc* 0) ; Uzumaki accumulator

(defparameter *printmode* 0)

(defun interpret() ; Function that interprets 1 byte of code from the spiral
  (let ((interp (curchar)))
  (cond ((char= interp #\H)(jumpout))
        ((char= interp #\B)(jumpin))
        ((char= interp #\W)(bigjumpout))
        ((char= interp #\#)(setf *printmode* 1))
        ((char= interp #\E)(print *queue*))
        ((char= interp #\O)(format t "~d" (elt *queue* 0)))
        ((char= interp #\Q)(enqueue 0))
        ((char= interp #\Z)(enqueue (elt *queue* 0)))
        ((char= interp #\I)(inc 1))
        ((char= interp #\D)(inc -1))
        ((char= interp #\P)(inc 10))
        ((char= interp #\M)(inc -10))
        ((char= interp #\X)(popqueue))
        ((char= interp #\C)(format t "~c" (code-char (elt *queue* 0))))
        ((char= interp #\R)(setf *queue* (nreverse *queue*)))
        ((char= interp #\A)(setf *acc* (elt *queue* 0)))
        ((char= interp #\V)(inc *acc*))
        ((char= interp #\G)(get-int))
        ((char= interp #\S)(get-char))
        ((char= interp #\J)(if(= *acc* (elt *queue* 0))(skip)()))
        ((char= interp #\K)(if(not(= *acc* (elt *queue* 0)))(skip)()))
        (1(uh-oh "ERROR: UNRECOGNIZED CHARACTER IN SPIRAL."))
  ))

  (loop ; This prints the characters between each '#' command
    (if (= *printmode* 0)( (lambda() (setf *dir* (curdir)) (return)) )())
    (setf *dir* (curdir))
    (move 1)
    (if (char= (curchar) #\#)(setf *printmode* 0)(put))
  )
)

(defun popqueue() ; Pops the memory queue
  (setf *queue* (nreverse *queue*))
  (vector-pop *queue*)
  (setf *queue* (nreverse *queue*))
)

(defun enqueue(value) ; Enqueues a value to the memory queue
  (vector-push-extend value *queue*)
)

(defun inc(value) ; Increments the memory queue by a given amount
  (setf (elt *queue* 0) (+(elt *queue* 0) value))
)

(defun put() ; Outputs a character from the spiral
  (format t "~c" (curchar))
)

(defun get-int() ; Gets uesr input as integer
  (let ((gotcha (parse-integer (read-line t))))
    (if gotcha(enqueue gotcha)(enqueue 0))
  )
)

(defun get-char() ; Gets byte of user input
  (let ((gotcha (read-char t)))
    (if gotcha(enqueue (char-code gotcha))(enqueue 0))
  )
)

(defparameter *times* 0)
(defparameter *offset* 0)
(defparameter *edge* (- (length *array*) 1))
(defparameter *jumping* 0) ; Keeps track of when the IP is jumping

(defparameter *movespiral* (make-array `(,(length *array*),(length *array*)) :initial-element 0)) ; 2D array that stores the directions the IP should move in

(loop ; Fills *movespiral* with directions
  (if (= *times* (+ *edge* 1))(return)())
  (cond ((and (= *x* *offset*) (= *y* (+ 2 *offset*)))((lambda () (rotate 1) (setf *offset* (+ *offset* 2)))))
        ((and (= *x* (- *edge* *offset*))(= *y* *offset*))(rotate 1))
        ((and (= *x* *offset*)(= *y* (- *edge* *offset*)))(rotate 1))
        ((and (= *x* (- *edge* *offset*)) (= *y* (- *edge* *offset*)))(rotate 1))
  )
  (setf (curdir) *dir*)
  (move 1)
)

(move -1)
(setf (curdir) 9) ; Set the final command direction to 9

(setf *x* 0)
(setf *y* 0)

(loop ; Main loop
  (if(not(= (curdir) 9))(setf *dir* (curdir))()) ; Terminate when on final command
  (setf *jumping* 0)
  (interpret)
  (if (= (curdir) 9)(return)())
  (if (= *jumping* 0)(move 1)())
)
