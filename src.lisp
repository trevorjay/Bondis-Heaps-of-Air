(defpackage "ACC" (:use "COMMON-LISP" "PARENSCRIPT"))
(in-package "ACC")

(defparameter *b64*
  (with-open-file
      (instream "spinner.b64")
    (let ((string (make-string (file-length instream))))
      (read-sequence string instream)
      string)))

(defmacro code-src (&rest body)
  `(define-symbol-macro code (ps ,@body)))

(code-src
 (setf json -j-s-o-n)
 
 (defparameter *session* nil)

 (defparameter *ctx* (chain cnv (get-context "2d")))
 (defparameter *skip* nil)
 (defparameter *story* nil)
 (defparameter *arcade* nil)
 
 (defmacro log- (msg)
   `(chain console (log ,msg)))

 (defun ng-post (call- call-back)
   (chain
    (fetch
     "https://www.newgrounds.io/gateway_v3.php/"
     (create
      headers (create "Content-Type" "application/x-www-form-urlencoded")
      method "POST"
      body
      (concatenate 'string "input="
		   (encode-u-r-i-component
		    (chain json (stringify (create
					    app_id "XXX" 
					    session_id *session*
					    call call-)))))))
    (then #'(lambda (rsp)
	      (setf (@ window rsp) rsp)
	      (when (@ rsp ok)
		(chain (chain rsp (json)) (then call-back)))))))
 
 (defmacro ng-call (call &rest body)
   `(ng-post ,call #'(lambda (data) ,@body)))

  (defun ping ()
   (symbol-macrolet ((ms-five-minutes (lisp (* 1000 60 5))))
     (set-timeout #'ping ms-five-minutes)
     (ng-call (create component "Gateway.ping" parameters (create)))))

 (defun unlock-trophy (trophy)
   (ng-call (create component "Medal.unlock" parameters (create id trophy))))

 (defun post-score (score)
   (ng-call (create component "ScoreBoard.postScore" parameters (create id 0 value score)) (setf (@ window data) data)))

 (set-timeout #'ping 0)

 (defparameter *resources-loaded* 0)

 (define-symbol-macro g (* 0.1568 0.6))

 (define-symbol-macro game "game")
 (define-symbol-macro segue "segue")
 (define-symbol-macro notice "notice")
 (define-symbol-macro check "check")
 (define-symbol-macro err "err")
 (define-symbol-macro menu "menu")
 (define-symbol-macro opening "opening")
 (define-symbol-macro loading "loading")
 (define-symbol-macro fade "fade")

 (defparameter *stop* f)
 
 (defparameter *resize* f)

 (defparameter *ms* 0)
 (defparameter *t-1* 0)

 (defparameter *transition* t)
 (defun transition-to (state)
   (setf *state* state)
   (setf *transition* t)
   (setf *ms* 0))
 
 (defparameter *state* loading)
 
 (defparameter *hang-time* 0)
 (defparameter *level* 0)
 (defparameter *next-level* 0)
 (defparameter *next-wave* 0)
 (defparameter *waves* null)
 (defun speed (lvl)
   (when (> lvl 180) (setf lvl 180))
   (let* ((v (sin (* (/ lvl 180) (/ pi 2))))
	  (spd (+ (* 24 v) (* 4 (- 1 v)))))
     (when (< spd 3) (return-from speed 3))
     (when (> spd 24) (return-from speed 24))
     spd))
 (defun gap (lvl)
   (when (> lvl 180) (setf lvl 180))
   (let* ((v (sin (* (/ lvl 180) (/ pi 2))))
	  (gp (+ (* 240 v) (* 1440 (- 1 v)))))
     (when (< gp 0) (return-from gap 0))
     (when (> gp 1920) (return-from gap 1920))
     gp))
 
 (defun form-cloud (cld)
   (let* ((idx (floor (* (random) 4)))
	  (x (* (% idx 2) -940))
	  (y (* (floor (/ idx 2)) -600)))
     (setf (@ cld first-child style transform) (concatenate 'string "translate(" x "px," y "px)"))))
 (defparameter *clouds* (chain -array (from (chain document (get-elements-by-class-name "cloud")))))
 (setf (@ (aref *clouds* 0) x) 970)
 (setf (@ (aref *clouds* 1) x) 2410)
 (loop
   for cloud in *clouds*
   do (form-cloud cloud))

 (defun tiles (idx)
   (let ((wave (chain proto-wave content first-element-child (clone-node t))))
     (setf (@ wave x) 0)
     (chain vscreen (append-child wave))
     (case idx
       (0
        (setf (@ wave base) "translate(0px,0px)")
	(chain *waves* (add (create x1 2555 y1 (+ 620 0) x2 2626 y2 (+ 554 5) static f display wave)))
	(chain *waves* (add (create x1 2626 y1 (+ 554 5) x2 2671 y2 (+ 554 5) static f)))
	(chain *waves* (add (create x1 2671 y1 (+ 554 5) x2 2747 y2 (+ 620 0) static f))))
       (1
        (setf (@ wave base) "translate(-500px,0px)")
	(chain *waves* (add (create x1 2508 y1 (+ 620 0) x2 2619 y2 (+ 514 5) static f display wave)))
	(chain *waves* (add (create x1 2619 y1 (+ 514 5) x2 2680 y2 (+ 514 5) static f)))
	(chain *waves* (add (create x1 2680 y1 (+ 514 5) x2 2792 y2 (+ 620 0) static f))))
       (2
        (setf (@ wave base) "translate(-1000px,0px)")
	(chain *waves* (add (create x1 2447 y1 (+ 620 0) x2 2622 y2 (+ 440 5) static f display wave)))
	(chain *waves* (add (create x1 2622 y1 (+ 440 5) x2 2677 y2 (+ 440 5) static f)))
	(chain *waves* (add (create x1 2677 y1 (+ 440 5) x2 2853 y2 (+ 620 0) static f))))
       (3
        (setf (@ wave base) "translate(-1500px,0px)")
	(chain *waves* (add (create x1 2427 y1 (+ 620 0) x2 2632 y2 (+ 523 5) static f display wave)))
	(chain *waves* (add (create x1 2632 y1 (+ 523 5) x2 2669 y2 (+ 523 5) static f)))
	(chain *waves* (add (create x1 2669 y1 (+ 523 5) x2 2875 y2 (+ 620 0) static f))))
       (4
	(setf (@ wave base) "translate(0px,-800px)")
	(chain *waves* (add (create x1 2540 y1 (+ 620 0) x2 2620 y2 (+ 437 5) static f display wave)))
	(chain *waves* (add (create x1 2620 y1 (+ 437 5) x2 2679 y2 (+ 437 5) static f)))
	(chain *waves* (add (create x1 2679 y1 (+ 437 5) x2 2759 y2 (+ 620 0) static f))))
       (5
        (setf (@ wave base) "translate(-500px,-800px)")
	(chain *waves* (add (create x1 2450 y1 (+ 620 0) x2 2708 y2 (+ 447 5) static f display wave)))
	(chain *waves* (add (create x1 2708 y1 (+ 447 5) x2 2758 y2 (+ 447 5) static f)))
	(chain *waves* (add (create x1 2758 y1 (+ 447 5) x2 2847 y2 (+ 620 0) static f))))
       (6
        (setf (@ wave base) "translate(-1000px,-800px)")
	(chain *waves* (add (create x1 2449 y1 (+ 620 0) x2 2545 y2 (+ 446 5) static f display wave)))
	(chain *waves* (add (create x1 2545 y1 (+ 446 5) x2 2599 y2 (+ 446 5) static f)))
	(chain *waves* (add (create x1 2599 y1 (+ 446 5) x2 2853 y2 (+ 620 0) static f))))
       (7
        (setf (@ wave base) "translate(-1500px,-800px)")
	(chain *waves* (add (create x1 2576 y1 (+ 620 0) x2 2686 y2 (+ 523 5) static f display wave)))
	(chain *waves* (add (create x1 2686 y1 (+ 523 5) x2 2711 y2 (+ 523 5) static f)))
	(chain *waves* (add (create x1 2711 y1 (+ 523 5) x2 2722 y2 (+ 620 0) static f)))))
     )
   )

 (defparameter *x* 0)
 (defparameter *y* 620)
 (defparameter *gv* 0)
 (defparameter *ang* 0)
 (defparameter *accel* f)

 (defparameter *flap* 0)
 (defparameter *undulate* 0)
 (defparameter *hit* 0)

 (defparameter *segue-frame* 0)
 (defparameter *swim-xd* 0)
 (defparameter *swim-x* 0)
 (defparameter *swim-y* 0)
 (defparameter *splash-x* 0)
 (defparameter *splash-y* 0)

 (defun start (e)
   (chain e (prevent-default))
   
   (when (not (= self top))
     (open (@ document location) "_blank" "noopener")
     (return-from start))

   (chain intro-song (play))
   (chain main-song (play))
   (setf (@ main-song muted) t)
   
   (chain btn parent-element (remove-child btn))
   
   (when (= (typeof -device-orientation-event) "undefined")
     (transition-to err)
     (return-from start))
   
   (when (@ -device-orientation-event request-permission)
     (chain -device-orientation-event (request-permission)))
   
   (add-event-listener
    "deviceorientation"
    #'(lambda (e)
	(with-slots (beta) e
	  (setf *ang* (* (asin (sin (* beta 2.25 (/ pi 180.0)))) (/ 180.0 pi)))
	  (when (< *ang* -90)
	    (setf *ang* -90))
	  (when (> *ang* 90)
	    (setf *ang* 90)))
	(when (not (= *accel* 0))
	  (setf *accel* t))))

   (transition-to check))
 
 (defun resize (e)
   (setf *resize* t))
 (add-event-listener "resize" #'resize)
 (resize)

 (defun press (e)
   (case (@ e key)
     ("g" (transition-to game))
     ("p" (setf *stop* t))))
 (add-event-listener "keydown" #'press)

 (defun frame (t-0)
   (unless *stop*
     (request-animation-frame #'frame))
   
   (when *resize*
     (let* ((rect (chain body (get-bounding-client-rect)))
	    (bw (@ rect width))
	    (bh (@ rect height))
	    (w 1920)
	    (h 720)
	    (sf 1)
	    (rot f))
       (when (< bw bh)
	 (setf w 720)
	 (setf h 1920)
	 (setf rot t))
       (setf sf (/ bw w))
       (when (> (* sf h) bh)
	 (setf sf (/ bh h)))
       (setf sf (* sf 100))
       (setf (@ vscreen style transform)
	     (concatenate 'string "translate(-50%, -50%) scale(" sf "%)" (if rot " rotate(90deg)" "")))
       (setf *resize* f)))

   (let ((delta (- t-0 *t-1)))
     (setf *ms* (+ *ms* (if (< delta 67) delta 67)))
     (setf *t-1* t-0))

   (case *state*
     (loading
      (when *transition*
	(flet ((bank ()
		 (setf *resources-loaded* (+ *resources-loaded* 1))))
	  (setf *arcade* (new -image))
	  (setf *story* (new -image))
	  (setf *skip* (new -image))
	  (setf (@ *arcade* onload) #'bank)
	  (setf (@ *story* onload) #'bank)
	  (setf (@ *skip* onload) #'bank)
	  (setf (@ *arcade* src) "resources/pngs/arcade.png")
	  (setf (@ *story* src) "resources/pngs/story.png")
	  (setf (@ *skip* src) "resources/pngs/skip.png")
	  (let ((resources
		  (list
		   "resources/jpgs/error.jpg"
		   "resources/pngs/warning.png"
		   "resources/pngs/clouds.png"
		   "resources/pngs/splashes.png"
		   "resources/pngs/waves.png"
		   "resources/pngs/surf_strip.png"
		   "resources/pngs/swim.png")))
	    (loop
	      for resource in resources
	      do (let ((img (new -image)))
		   (setf (@ img onload) #'bank)
		   (setf (@ img src) resource)))))
	(chain menu-vid (play))
	(chain intro-vid (play))
	(setf *transition* f))
      (when (and (>= *resources-loaded* 10)
		 (> (@ menu-vid ready-state) 2))
	(chain menu-vid (pause))
	(chain intro-vid (pause))
	(setf (@ menu-vid currentTime) 0)
	(setf (@ intro-vid currentTime) 0)
	(transition-to notice)))
     
     (notice
      (when *transition*
	(chain (@ loader parent-element) (remove-child loader))
	(setf *transition* f)))

     (menu
      (when *transition*
	(flet ((play ()
		 (transition-to fade))
	       (bore ()
		 (transition-to opening)))
	  (setf (@ story onclick) #'bore)
	  (setf (@ arcade onclick) #'play))
	(setf (@ arcade style visibility) "visible")
	(setf (@ story style visibility) "visible")
	(chain menu-vid (play))
	(setf *transition* f))

      (chain *ctx* (draw-image menu-vid 0 0 1920 720 0 0 1920 720))
      (chain *ctx* (draw-image *arcade* 1270 440 416 107))
      (chain *ctx* (draw-image *story* 800 440 416 107))
      
      )

     (opening
      (when *transition*
	(chain menu-vid (pause))
	(chain arcade parent-element (remove-child arcade))
	(chain story parent-element (remove-child story))
	(flet ((play ()
		 (transition-to fade)))
	  (setf (@ skip onclick) #'play))
	
	(setf (@ skip style visibility) "visible")
	(chain intro-vid (play))
	(setf *transition* f))

      (if (@ intro-vid ended)
	  (chain skip (click))
	  (progn
	    (chain *ctx* (draw-image intro-vid 0 0 1920 720 0 0 1920 720))
	    (chain *ctx* (draw-image *skip* 1740 650 161 55)))))

     (fade
      (setf (@ intro-song volume) (min (max (- 1.0 (/ *ms* 500)) 0) 1))
      (when (> *ms* 500)
	(chain intro parent-element (remove-child intro))
	(chain intro-song (pause))
	(setf (@ main-song current-time) 0)
	(setf (@ main-song muted) f)
	(setf (@ surfer style transform) "translate(50%,50%) translate(-300px,620px) rotate(0deg)")
	(transition-to game)))

     (check
      (when *accel*
	(chain warning parent-element (remove-child warning))
	(transition-to menu))
      (when (> *ms* 8000)
	(transition-to err)))

     (err
      (when *transition*
	(chain intro-song (pause))
	(chain main-song (pause))
	(let ((card (chain proto-err content first-element-child (clone-node t))))
	  (chain vscreen (append-child card))
	  (setf *transition* f))))

     (segue
      (when *transition*
	(setf *segue-frame* 0)
	(setf *transition* f))

      (loop
	while (>= *ms* 83)
	do (progn

	     (when (= *segue-frame* 1)
	       (let ((waves (chain vscreen (query-selector-all ".wave"))))
		 (loop for wave in waves
		       do (chain vscreen (remove-child wave))))
	       (setf (@ surfer style transform) "translate(50%,50%) translate(-300px,620px) rotate(0deg)"))

	     (when (= *segue-frame* 6)
	       (setf (@ splash style transform) "")
	       (setf *swim-xd* 470))

	     (cond
	       ((< *segue-frame* 6)
		(setf *splash-x* (* (% *segue-frame* 3) -420))
		(setf *splash-y* (* (floor (/ *segue-frame* 3)) -420)))

	       ((< *segue-frame* 14)
		(setf *swim-x* (* (% (- *segue-frame* 6) 3) -420))
		(setf *swim-y* (* (floor (/ (- *segue-frame* 6) 3)) -220)))

	       ((and (> *segue-frame* 15) (< *segue-frame* 23))
		(setf *swim-x* (* (% (- *segue-frame* 8) 3) -420))
		(setf *swim-y* (* (floor (/ (- *segue-frame* 8) 3)) -220)))

	       ((> *segue-frame* 22)
		(setf *swim-x* (* (% (+ (% (- *segue-frame* 23) 12) 15) 3) -420))
		(setf *swim-y* (* (floor (/ (+ (% (- *segue-frame* 23) 12) 15) 3)) -220))
		(setf *swim-xd* (+ *swim-xd* -12))
		(when (<= *swim-xd* 0)
		  (transition-to game)))
	       
	       )

	     (setf *segue-frame* (+ *segue-frame* 1))
	     (setf *ms* (- *ms* 83))))

      (setf (@ swim first-child style transform) (concatenate 'string "translate(" *swim-x* "px," *swim-y* "px)"))
      (setf (@ swim style transform) (concatenate 'string "translate(" *swim-xd* "px,0px)"))
      (setf (@ splash first-child style transform) (concatenate 'string "translate(" *splash-x* "px," *splash-y* "px)"))

      )
     
     (game
      
      (when *transition*
	(setf *level* 1)
	(when (> *hang-time* 0)
	  (setf *session* (chain (new (-u-r-l-Search-Params (@ document location search))) (get "ngio_session_id")))
	  (post-score *hang-time*))
	(setf *hang-time* 0)
	(setf *next-level* 125)
	(setf *next-wave* 0)
	(setf *gv* 0)
	(setf *x* -300)
	(setf *y* 620)
	(setf *flap* 0)
	(setf *undulate* 0)
	(setf *hit* 0)
	(setf *waves* (new (-set)))
	(chain *waves* (add (create x1 0 y1 620 x2 1920 y2 620 static t)))
	(setf *transition* f))

      (loop
	while (>= *ms* 8)
	do (let* ((yv (* *ang* 0.011)))

	     (setf *hit* (- *hit* 1))

	     (setf *flap* (+ *flap* 1))
	     (when (> *flap* 19) (setf *flap* 0))

	     (setf *undulate* (+ *undulate* 1))
	     (when (> *undulate* 39) (setf *undulate* 0))

	     (setf *next-level* (- *next-level* 1))
	     (when (<= *next-level* 0)
	       (setf *next-level* 125))
	     (setf *level* (min 999 (+ *level* 0.008)))

	     (setf yv (* yv (* (speed *level*) 2)))
	     (when (> yv 24) (setf yv 24))

	     (when (< *x* 0) (setf *x* (+ *x* (* (speed *level*) 0.32))))
	     (when (> *x* 0) (setf *x* 0))

	     (setf *next-wave* (+ *next-wave* (speed *level*)))
	     (when (>= *next-wave* (+ 260 (gap *level*)))
	       (tiles (floor (* (random) 8)))

	       (setf *next-wave* 0))

	     (let ((waves (chain -array (from *waves*))))
	       (loop for wave in waves
		     do (with-slots (x1 y1 x2 y2 static display) wave
			  (unless static
			    (setf x1 (- x1 (speed *level*)))
			    (setf x2 (- x2 (speed *level*)))
			    (when display
			      (setf (@ display x) (- (@ display x) (speed *level*)))))
			  (when (< x1 -480)
			    (when display
			      (chain display parent-element (remove-child display)))
			    (chain *waves* (delete wave))))))

	     (let ((waves (chain (chain -array (from *waves*)) (reverse))))
	       (loop
		 for wave in waves
		 do (with-slots (x1 x2 y1 y2) wave
		      (when (and
			     (>= 175 x1)
			     (<= 175 x2))
			(let* ((m (/ (- y2 y1) (- x2 x1)))
			       (b (- y1 (* m x1)))
			       (y- (+ (* m 175) b)))
			  (if (< *y* y-)
			      (progn
				(setf *gv* (+ *gv* 1))
				(setf *hang-time* (+ *hang-time* 8))
				(setf *y* (+ *y* yv (* *gv* g))))
			      (progn
				(when (and
				       (> (abs (- *ang* (/ (* (atan m) 180) pi))) 25)
				       (or
					(> *gv* 0)
					(not (= m 0))))
				  (setf *hit* 20)
				  (setf (@ splash style transform) "translate(380px,0px)")
				  (setf (@ splash first-child style transform) "translate(0px,0px)")
				  (transition-to segue))
				(when (> *gv* 0)
				  (setf *hit* 20))
				(setf *y* y-)
				(setf yv 0)
				(setf *gv* 0))))
			(break)))))

	     (loop
	       for cloud in *clouds*
	       do (progn
		    (setf (@ cloud x) (- (@ cloud x) 0.125))
		    (when (< (@ cloud x) -940)
		      (setf (@ cloud x) 2410)
		      (form-cloud cloud))))

	     (setf *ms* (- *ms* 8))))

      (let ((z 0) (a 0) (b 0) (c 0) (d 0) (e 0) (f 0))
	(setf z (% *hang-time* 3600000))
	(setf a (floor (/ z 600000)))
	(setf z (% z 600000))
	(setf b (floor (/ z 60000)))
	(setf z (% z 60000))
	(setf c (floor (/ z 10000)))
	(setf z (% z 10000))
	(setf d (floor (/ z 1000)))
	(setf z (% z 1000))
	(setf e (floor (/ z 100)))
	(setf z (% z 100))
	(setf f (floor (/ z 10)))
	(setf (@ clock text-content) (concatenate 'string "" a b ":" c d "." e f)))
      
      (setf (@ surfer style transform) (concatenate 'string "translate(-50%,-50%) translate(" *x* "px," (- *y* 620) "px) rotate(" *ang* "deg)"))
      
      (let ((flap 0)
	    (offset 0))
	(when (> *flap* 9) (setf flap -250))

	(cond
	  ((> *hit* 0)
	   (setf offset -1000))
	  ((< *ang* (- 5)) (setf offset 500))
	  ((> *ang* 5) (setf offset -500)))
	
	(setf (@ surf-strip style transform) (concatenate 'string "translate(" (+ -500 offset flap) "px,0px)")))

      (let ((waves (chain -array (from *waves*)))
	    (undulate 0))
	(cond
	  ((> *undulate* 29) (setf undulate -600))
	  ((> *undulate* 19) (setf undulate -400))
	  ((> *undulate* 9) (setf undulate -200)))
	(loop for wave in waves
	      do (with-slots (x1 y1 x2 y2 static display) wave
		   (when display
		     (setf (@ display style transform) (concatenate 'string "translate(" (@ display x) "px,0px)"))
		     (setf (@ display first-child style transform) (concatenate 'string "" (@ display base) " translate(0px," undulate "px)"))))))

      (loop
	for cloud in *clouds*
	do (setf (@ cloud style transform) (concatenate 'string "translate(" (@ cloud x) "px,0px)")))

      ))

   )

 (setf (@ btn disabled) f)
 (request-animation-frame #'frame))

(with-open-file (idx
		 "index.html"
		 :direction :output
		 :if-does-not-exist :create
		 :if-exists :supersede)
  (format
   idx
   "<!DOCTYPE html>~%~a~%"
   (ps-html
    ((:html :lang "en")
     (:head
      ((:meta :charset "utf-8"))
      ((:meta :name "viewport" :content "width=device-width,maximum-scale=1,initial-scale=1"))
      (:title "Bondi's Heaps of Air")
      (:style "
html, body {
  font-family: monospace;
  margin: 0;
  padding: 0;
  width: 100%;
  height: 100%;
  overflow: hidden;
  background: white;
}

body {
  background: black;
  width: 100%;
  min-height: 100%;
  touch-action: none;
}

#btn {
  text-align: center;
  color: black;
  position: absolute;
  font-size: 60px;
  width: 250px;
  /*height: 80px;*/
  top: 72%;
  left: 50%;
  transform: translate(-50%, -50%);
}

#vscreen {
  position: absolute;
  top: 50%;
  left: 50%;
  transform: translate(-50%, -50%) scale(0%) rotate(0deg);
  background: linear-gradient(to bottom, #7bcbf6, #a9d8f1 calc(100% / 3), #ffffff);
  /*background: white;*/
  overflow: hidden;
  width: 1920px;
  height: 720px;
}

.hidden {
  visibility: hidden;
}

#surfer {
  position: absolute;
  top: 486px;
  left: 175px;
  width: 250px;
  height: 400px;
  overflow: hidden;
  transform-origin: 50% 334px;
  /*border: solid 1px black;*/
  transform: translate(-50%, -50%);
}

#surfer img {
  display: block;
  position: absolute;
  top: 0px;
  left: 0px;
  /*visibility: hidden;*/
}

#cnv {
  position: absolute;
  top: 0;
  left: 0;
}

.wave {
  position: absolute;
  top: 430px;
  left: 2400px;
  width: 500px;
  height: 200px;
  /*border: solid 1px black;*/
  overflow: hidden;
  transform-origin: 0% 100%;
}

.wave img {
  position: absolute;
  top: -5px;
  left: 0px;  
}

#stats {
  font-size: 32px;
  position: absolute;
  top: 0;
  left: 0;
}

#splash {
  position: absolute;
  top: 230px;
  /*left: -40px;*/
  left: -420px;
  width: 420px;
  height: 420px;
  overflow: hidden;
  /*border: solid 1px black;*/
  /*transform: translate(-50%, -50%);*/
}

#splash img {
  position: absolute;
  top: 0px;
  left: 0px;
}

#swim {
  position: absolute;
  top: 483px;
  /*left: 50px;*/
  left: -420px;
  width: 420px;
  height: 220px;
  overflow: hidden;
  /*border: solid 1px black;*/
}

#swim img {
  position: absolute;
  top: 0px;
  left: 0px;
}

#warning {
  background-color: black;
  background-image: url(\"resources/pngs/warning.png\");
  width: 100%;
  height: 100%;
  position: absolute;
  top: 0px;
  left: 0px;
}

#err {
  background-color: #22534d;
  background-image: url(\"resources/jpgs/error.jpg\");
  width: 100%;
  height: 100%;
  position: absolute;
  top: 0px;
  left: 0px;
}

.cloud {
  position: absolute;
  top: 25px;
  left: 0px;
  width: 940px;
  height: 595px;
  overflow: hidden;
}

.cloud img {
  position: absolute;
  top: -5px;
  left: 0px;  
}

@font-face {
  font-family: 'original_surfer';
  src: url('resources/woff2/original-surfer.woff2') format('woff2');
  font-weight: normal;
  font-style: normal;
}

#score {
  position: absolute;
  top: 0.5em;
  right: 0px;
  text-align: right;
  font-size: 60px;
  color: #034eb7;
  font-family: 'original_surfer';
  height: 50px;
}

#clock {
  font-family: monospace;
}

#loader {
  position: absolute;
  top: 0px;
  left: 0px;
  width: 1920px;
  height: 720px;
  background: black;
}

#loader img {
  position: absolute;
  top: 0px;
  left: 50%;
  height: 720px;
  transform: translate(-50%,0%);
}

#ocean {
  position: absolute;
  bottom: 0px;
  left: 0px;
  background: #2d8591ff;
  width: 1920px;
  height: 100px;
}

#intro {
  background: black;
  position: absolute;
  top: 0px;
  left: 0px;
  width: 1920px;
  height: 720px;
  overflow: hidden;
}

video {
  position: absolute;
  width: 0px;
  height: 0px;
  top: 0px;
  left: 0px;
}

canvas {
  position: absolute;
  top: 0px;
  left: 0px;
}

#skip {
  position: absolute;
  top: 650px;
  left: 1740px;
  width: 161px;
  height: 55px;
  visibility: hidden;
}

#story {
  position: absolute;
  top: 440px;
  left: 800px;
  width: 416px;
  height: 107px;
  visibility: hidden;
}

#arcade {
  position: absolute;
  top: 440px;
  left: 1270px;
  width: 416px;
  height: 107px;
  visibility: hidden;
}
")
      ((:body :id "body")
       ((:audio :id "introSong" :loop t :preload "auto" :src "resources/mp3s/intro.mp3"))
       ((:audio :id "mainSong" :loop t :preload "auto" :src "resources/mp3s/main.mp3"))
       ((:div :id "vscreen")
	((:div :class "cloud" :style "transform: translate(970px,0px)") ((:img :src "resources/pngs/clouds.png")))
	((:div :class "cloud" :style "transform: translate(2410px,0px)") ((:img :src "resources/pngs/clouds.png")))
	((:div :id "surfer" :class "") ((:img :id "surfStrip" :src "resources/pngs/surf_strip.png")))
	((:div :id "splash" :class "") ((:img :src "resources/pngs/splashes.png")))
	((:div :id "swim" :class "") ((:img :src "resources/pngs/swim.png")))
	((:div :id "score") (:span "Hangtime:&nbsp;") ((:span :id "clock") "00:00.00") (:span "&nbsp;"))
	((:div :id "ocean"))
	((:div :id "intro")
	 ((:video :id "introVid" :src "resources/mp4s/intro.mp4" :crossorigin "anonymous" :preload "auto" :muted t :playsinline t))
	 ((:video :id "menuVid" :src "resources/mp4s/menu.mp4" :crossorigin "anonymous" :preload "auto" :muted t :playsinline t :loop t ))
	 ((:canvas :id "cnv" :width 1920 :height 720))
	 ((:div :id "skip"))
	 ((:div :id "story"))
	 ((:div :id "arcade")))
	((:div :id "warning")
	 ((:button :id "btn" :disabled t :onclick (lisp (ps-inline (start event)))) "OK"))
	((:div :id "loader")
	 ((:img :src (lisp (concatenate 'string "data:image/svg+xml;base64," *b64*))))))
       ((:template :id "protoWave")
	((:div :class "wave")
	 ((:img :src "resources/pngs/waves.png"))))
       ((:template :id "protoErr")
	((:div :id "err")))
       ((:script :type "text/javascript") code)))))))
