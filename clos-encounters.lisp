(defpackage :clos-encounters  ; Create our package
  (:use :common-lisp :clog)   ; Use :common-lisp and :clog
  (:export start-game))       ; Export from package our initialization
                              ; function by exporting it's symbol.
(in-package :clos-encounters) ; CL is not about files, but logical partitions
                              ; called packages with internal and exported
                              ; symbols.

;; Once you have loaded clos-enounters with:
;;     (ql:quickload :clos-encounters)
;; in the REPL, the clos-encounters system is loaded and
;; compiled. That means you can use some Slime magic in
;; emacs. Clicking on any symbol (even in comments) with
;; M-. (meta/alt-period) will bring you right to it, even
;; if in other packages like CLOG. You get back with M-,
;; (meta/alt-comma) CLOG is very well documented both
;; in code and its manual. So when you don't understand
;; something use the M-. trick.

;; defun defines a new function, START-GAME and in this case it contains
;; no parameters.
(defun start-game ()                    ; Our exported symbol START-GAME
  "Initialize CLOG and open a browser"  ; Lisp is tolerant about compilation order
  (initialize 'title-screen)            ; even though the TITLE-SCREEN symbol is
  (open-browser))                       ; defined later we can refer to it in code now.

;; TITLE-SCREEN has one parameter, the name of the parameter does not matter.
;; only its order is significant. Named "keyword" parameters we will see that 
;; later.
;;
;; In case wondering:
;; We know to use one parameter because the documentation for CLOG framework
;; tells us that when a browser connects to the server, it will serve a new page
;; and call the default route which we established in START-GAME with INITIALIZE
;; with one parameter, the root element of the browser window that is a CLOG-BODY
;; object.
;;
;; There are nicer ways to spice up a CLOG app then html in divs and
;; spans, but we are working on lisp right now not CLOG.
(defun title-screen (body)
  "Setup screen layout and splash"
  ;; let* is used instead of let since let* allows SPLASH to use LAYOUT
  ;; try removing the * and M-X-c and see the error.
  (let* ((layout (create-panel-box-layout body))          ; CLOG's panel box layout
	 (splash (create-div (center-panel layout)        ; Some Content in a CLOG-div object
			     :content "<center>They Are Coming!!!</center>")))       
    (center-children (center-panel layout))               ; center-panel is the accessor
                                                          ; for a slot/element in the layout object
    (set-border splash "2px" :solid :black)               ; set-border is a method of CLOG-OBJects.
    (loop for N from 100 to 200 by 10                     ; The extended loop macro Tutorial 8
	  do
	     (set-geometry splash :width N :height N)
	     (sleep .1))                                  ; Each instance of our game runs in its own
                                                          ; thread. We sleep our thread but not others.
    (create-div splash :content "<p><center><h3>CLOS-ENCOUNTERS<h3><h4>of the Lisp kind</h4></center>")
    (set-on-click
     (create-div splash
		 :content (format nil "<center><button>CLICK TO START</button>~
                                       <p>Click/Touch to move and shoot</center>"))
     (lambda (obj)             ; lambda expression is an a function with no
       (declare (ignore obj))  ; name. In Lisp functions are data too and
       (run-game body)))))     ; here is an argument to set-on-click.

(defparameter *evil-dude* "--🛸--" "Them")
(defparameter *good-dude* "--⛄--" "Us")
(defparameter *evil-bomb* "💩" "Their bombs")
(defparameter *good-bomb* "🪨" "Our  bombs")
(defvar *high-score* 0 "Global High Score") ; Global means for every thread, every browser.

(defun run-game (body)
  "Run the game"
  (setf (inner-html body) "")                   ; setf is used for assignment, here setf assigns
					        ; "" a blank string to the inner-html property of the
                                                ; object body passed to run-game. This clears the screen.
  ;; Setup the board
  (let* ((layout      (create-panel-box-layout body))
	 (score-panel (create-panel-box-layout (top-panel layout)
					       :left-width 100
					       :right-width 100
					       :top-height 0
					       :bottom-height 0))
	 (points      0)
	 (evil-speed  200)
	 (score       (create-div (left-panel score-panel) :content "SCORE: 0"))
	 (high-score  (create-div (center-panel score-panel)
				  :content (format nil "HIGH SCORE: ~A" *high-score*)))
	 (speed       (create-div (right-panel score-panel)
				  :content (format nil "SPEED: ~A" evil-speed)))
	 (arena      (center-panel layout))
	 (arena-w    (width arena))
	 (arena-h    (height arena))
	 (evil-avatar (create-span arena :content *evil-dude*))
	 (evil-w      (width evil-avatar))
	 (evil-h      (height evil-avatar))
	 (evil-dir    'forward)
	 (good-avatar (create-span arena :content *good-dude*))
	 (good-w      (width good-avatar))
	 (good-h      (height good-avatar))
	 (done        nil))
    ;; setup colors
    (setf (background-color body) :navy)
    (setf (background-color arena) :lightblue)
    (setf (color score) :yellow)
    (setf (color high-score) :pink)
    (setf (color speed) :white)
    ;; setup text
    (setf (text-alignment high-score) :center)
    ;; setup avatars
    (setf (style body "user-select") :none)
    (setf (positioning evil-avatar) :absolute)
    (setf (positioning good-avatar) :absolute)
    (setf (left evil-avatar) (unit :px (random 100)))
    (setf (left good-avatar) (unit :px 10))
    (setf (bottom good-avatar) (unit :px 0))
    ;; setup move and fire of good guy
    (set-on-mouse-click arena
			(lambda (obj data)
			  (declare (ignore obj))
			  (let* ((bomb (create-span arena :content *good-bomb*)))
			    (setf (positioning bomb) :absolute)
			    (setf (left good-avatar) (unit :px (getf data :x)))
			    (setf (left bomb) (unit :px (getf data :x)))
			    (loop for loc from (- arena-h good-h) downto 0 do
			      (sleep .001)
			      (setf (top bomb) (unit :px loc)))
			    ;; Collision detection
			    (when (and (>= (getf data :x) (position-left evil-avatar))
				       (<= (getf data :x) (+ evil-w (position-left evil-avatar))))
			      (incf points)
			      (decf evil-speed 10)
			      (when (< evil-speed 5)
				(setf evil-speed 5))
			      (setf (text speed) (format nil "SPEED: ~A" evil-speed))
			      (setf (text score) (format nil "SCORE: ~A" points))
			      (when (> points *high-score*)
				(setf *high-score* points)
				(setf (text high-score)
				      (format nil "HIGH SCORE: ~A" *high-score*))))
			    (destroy bomb))))
    (set-on-character body
		      (lambda (obj data)
			(declare (ignore obj))
			(when (equalp data #\s)
			  (setf done t))))
    (loop
      (sleep .001)
      (when (or done
		(not (validp body)))
	(setf (background-color body) :white)
	(setf (inner-html body)
	      (format nil "GAME OVER - SCORE : ~A" points))
	(return))
      (let ((x (position-left evil-avatar)))
	(cond ((> (+ x evil-w) arena-w)
	       (setf evil-dir 'reverse))
	      ((< x 0)
	       (setf evil-dir 'forward)))
	(if (equal evil-dir 'forward)
	    (incf x)
	    (decf x))
	(when (equal (random evil-speed) 1)
	  (bordeaux-threads:make-thread
	   (lambda ()
	     (let* ((bomb (create-span arena :content *evil-bomb*)))
	       (setf (positioning bomb) :absolute)
	       (setf (left bomb) (unit :px x))
	       (loop for loc from evil-h to (- arena-h good-h) do
		 (sleep .01)
		 (setf (top bomb) (unit :px loc)))
	       ;; Collision detection
	       (when (and (>= x (position-left good-avatar))
			  (<= x (+ good-w (position-left good-avatar))))
		 (setf done t))
	       (destroy bomb)))))
	(setf (left evil-avatar) (unit :px x))))))
