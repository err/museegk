(ns hack
  (:use	[rosado.processing]
        [rosado.processing.applet])
  (:require [overtone.live :as tone]))




;;Atoms
(def *mouse-position* (atom [0 0]))
(def *mouse-button* (atom -1))
; -1 - not-pressed
;  0 - not-pressed
;  1 - pressed

;;Refs
(def *time* (atom 0))

;;Structs
(defrecord button [idx x-pos y-pos edge-length color snd active?])

;;Global Values
(def *framerate*       60)
(def *screen-width*   1060)
(def *screen-height*  1060)
(def *bg-color*        51)
(def *hl-active-color* 255)
(def *hl-inactive-color* 220)
(def *active-color*    185)
(def *inactive-color*  80)
(def *intensified-color* 220)
(def *matrix-rows*      8)
(def *matrix-cols*      8)
(def *button-height*   100)
(def *button-width*    100)
(def *border-width*    40)
(def *dx*              25)
(def *dy*              25)



;;(defrecord button [x-pos y-pos edge-length color snd active?])
					;
(defn create-button [idx x y len color sample active?]
  (button. idx x y len color sample active?))

;(def *matrix* (ref (create-matrix 8 8)))

(defn nth-row  
  "returns indices of the nth row of an x-by-y matrix.
   n - desired row
   x - number of rows in matrix
   y - number of cols in matrix"
  [n x y]			
  (range  (* n y) (+ y (* n y))))



(defn nth-col
  "returns indices of the nth col of an x-by-y matrix.
   n - desired col 
   x - number of rows in matrix
   y - number of cols in matrix"  
  [n x y]
  (filter #(zero? (mod (- % n) y)) (range (* x y))))
  
  
(defn activate [vec indices turn-on?]
  (let [vec-len (dec (.size vec))
	row-len *matrix-cols*
	col-len *matrix-rows*
	active-b  *active-color*
	inactive-b *inactive-color*
	active-hl   *hl-active-color*
	inactive-hl  *hl-inactive-color*]
    (loop [res [] idx 0]
      (let [cur-b (nth vec idx)
	    new-b (when (some #(== idx %) indices)
		    (button. idx (:x-pos cur-b) (:y-pos cur-b) (:edge-length cur-b)
			     (if turn-on? active-b inactive-b); (:active? cur-b) inactive-b active-b)
			     (:snd cur-b) (if turn-on? true false)))]

	(if (= idx vec-len)
	  (conj res (or new-b cur-b))
	  (recur (conj res (or new-b cur-b)) (inc idx)))))))

(defn hl [vec indices]
  (let [vec-len (dec (.size vec))
	row-len *matrix-cols*
	col-len *matrix-rows*
	active-b  *active-color*
	inactive-b *inactive-color*
	active-hl   *hl-active-color*
	inactive-hl  *hl-inactive-color*]
    (loop [res [] idx 0]
      (let [cur-b (nth vec idx)
	    new-b (if (some #(== idx %) indices)
					;highlight  
		    (button. idx (:x-pos cur-b) (:y-pos cur-b) (:edge-length cur-b)
			     (if (:active? cur-b) active-hl inactive-hl)
			     (:snd cur-b) (:active? cur-b))
					;de-highlight
		    (button. idx  (:x-pos cur-b) (:y-pos cur-b) (:edge-length cur-b)
			     (if (:active? cur-b) active-b inactive-b)
			     (:snd cur-b) (:active? cur-b)))]

	(if (= idx vec-len)
	  (conj res new-b)
	  (recur (conj res new-b) (inc idx)))))))

(defn hl-row [vec n]
  (let [members (nth-row vec n *matrix-rows* *matrix-cols* :idx)
 	row-len  *matrix-cols*
	vec-len  (dec (* *matrix-rows* *matrix-cols*))
	active-b  *active-color*
	inactive-b *inactive-color*
	active-hl   *hl-active-color*
	inactive-hl  *hl-inactive-color*]
    (loop [res [] idx 0]
      (let [cur-b (nth vec idx)
	    new-b (if (some #(== idx %) members)
		;highlight  
		(button. idx (:x-pos cur-b) (:y-pos cur-b) (:edge-length cur-b)
			 (if (:active? cur-b) active-hl inactive-hl)
			 (:snd cur-b) (:active? cur-b))
		;de-highlight
		(button. idx  (:x-pos cur-b) (:y-pos cur-b) (:edge-length cur-b)
			 (if (:active? cur-b) active-b inactive-b)
			 (:snd cur-b) (:active? cur-b)))]
	(if (= idx vec-len)
	  (conj res new-b)
	  (recur (conj res new-b) (inc idx)))))))


(defn hl-col [vec n]
  (let [members (nth-col vec n *matrix-rows* *matrix-cols* :idx)
	vec-len (dec (.size vec))
	row-len  *matrix-cols*
	active-b  *active-color*
	inactive-b *inactive-color*
	active-hl   *hl-active-color*
	inactive-hl  *hl-inactive-color*
	col (nth-col n *matrix-rows* *matrix-cols*)]
    (loop [res [] idx 0]
      (let [cur-b (nth vec idx)
	    new-b (if (some #(== idx %) members)
		;highlight  
		(button. idx (:x-pos cur-b) (:y-pos cur-b) (:edge-length cur-b)
			 (if (:active? cur-b) active-hl inactive-hl)
			 (:snd cur-b) (:active? cur-b))
		;de-highlight
		(button. idx  (:x-pos cur-b) (:y-pos cur-b) (:edge-length cur-b)
			 (if (:active? cur-b) active-b inactive-b)
			 (:snd cur-b) (:active? cur-b)))]
	(if (= idx vec-len)
	  (conj res new-b)
	  (recur (conj res new-b) (inc idx)))))))


;; hm.. could use some refactoring
(let [rows (atom (apply concat (iterate #(shuffle %) (shuffle (range *matrix-rows*)))))
      cols (atom (apply concat (iterate #(shuffle %) (shuffle (range *matrix-cols*)))))]
  (defn rand-row []
    (let [h (first @rows)]
      (swap! rows next)
      h))
  
  (defn rand-col []
    (let [h (first @cols)]
      (swap! cols next)
      h)))

(def *rand-cols* (ref (shuffle (range *matrix-rows*))))

(defn shffl [n] (shuffle (range n)))


(let [rc-toggle (atom 1)]
  (defn update []
    (when (zero? (mod @*time* 4))
      (if (pos? @rc-toggle)
      	(dosync
      	 (let [r (rand-row)]
      	   ;(println "\n ROW: " r)
      	   (alter *matrix* (fn [vec n] (hl vec (nth-row n *matrix-rows* *matrix-cols*))) r ))) ;(rand-row)
      	(dosync
      	 (let [c (rand-col)]
      	   ;(println "\n col: " c)
      	   (alter *matrix* (fn [vec n] (hl vec (nth-col n *matrix-rows* *matrix-cols*))) c )))  ; (rand-col)
      	)
      (swap! rc-toggle #(* % -1)))))

(defn draw-button [button]
  (let [ x (:x-pos button)
	 y (:y-pos button)
	 c (:color button)
	 l (:edge-length button) ]
    
    (rect-mode CORNER)
    (no-stroke)				;(stroke-weight 3)
					;(stroke 255)

    (fill c 128 180)
    ;; (if (= c  *hl-inactive-color*)
    ;;   (fill 60 128 180)
    ;;   (fill c 128 180))
					;pretty colors    (fill (mod x 256)  128 180)
    (rect x y l l)))

(defn draw-matrix [m]
  (dorun (map draw-button m)))

(defn draw []
;; void draw() {
;;   pg.beginDraw();
;;   pg.background(102);
;;   pg.stroke(255);
;;   pg.line(40, 40, mouseX, mouseY);
;;   pg.endDraw();
;;   image(pg, 10, 10); 
;; }
  (.beginDraw peg)
  (background 51 0)
  (stroke 255)
  (.line peg 40 40 100 100)
  (.endDraw peg)
  (.image peg 10 10))



;;;; INPUT EVENTS
(defn mouse-moved [evt]
  (let [x (.getX evt)
	y (.getY evt)]
    (reset! *mouse-position* [x y])))


(defn collision? [vec x y]
  (let [res (filter (fn [button]
		      (let [xb  (:x-pos button)
			    yb  (:y-pos button)
			    len (:edge-length button)]
			(and (< xb x) (< x (+ xb len))
			     (< yb y) (< y (+ yb len)))))
		    vec)]
    (if (not (zero? (count res)))
      [(first res) (:idx (first res))]
      [nil -1])))

(def *toggle-on* (atom false))
(def *starting-block* (atom -1))
(def *visited* (atom nil))

(defn mouse-pressed [evt]
  (let [x (.getX evt)
	y (.getY evt)
	b @*mouse-button*]
    (let [[selection? idx] (collision? @*matrix* x y)]
      (println selection? "\n" idx)
      (when selection?
	(dosync
	 (if (:active? (nth @*matrix* idx))
	   (reset! *toggle-on* false)
	   (reset! *toggle-on* true))
	 (reset! *starting-block* idx)
	 (swap!  *visited* conj idx)
	 (alter *matrix* (fn [vec ind] (activate vec [ind] @*toggle-on*)) idx))))
    (reset! *mouse-button*  (* -1 b))))

(defn mouse-released [evt]
  (let [x (.getX evt)
	y (.getY evt)
	b @*mouse-button*]
    ;; (dosync (alter *matrix*
    ;; 		   (fn [vec n0 n1] (hl-col (hl-row vec n0) n1))
    ;; 		   (int (rand *matrix-rows*))
    ;; 		   (int (rand *matrix-cols*))))
    (reset! *visited* nil)
    (reset! *toggle-on* false)
    (reset! *mouse-button* (* -1 b))))

(defn mouse-dragged [evt]
  (let [x (.getX evt)
	y (.getY evt)]
    (let [[selection? idx] (collision? @*matrix* x y)]
      (println "mouse-dragged:  " selection? "\n" idx "\nvisited: " @*visited* "\ntoggle: " @*toggle-on*  "\n")
      (when selection? 
	(dosync
	 (when (not (some #(== idx %) @*visited*))
	   (alter *matrix* (fn [vec ind] (activate vec [ind] @*toggle-on*)) idx)))
	 (swap! *visited* #(distinct (conj % idx)))))
    (reset! *mouse-position* [x y])))


(def m-agent (agent nil))

(def metro (tone/metronome 120))

;; (def tones
;;      (map #([(:idx %) (:snd %) (:active? %)])
;; 	  @*matrix*))

(defn get-button [idx]
 (nth @*matrix* idx)
  ;; (nth @tones idx)
  )




(defn setup []
  
  (def peg (create-graphics 80 80 P2D))
  (smooth)
)  





(defapplet museegk
  :title "Enter the MatrEEGx!"
  :setup setup
  :draw draw
  :size [400 400]
  ;:mouse-moved mouse-moved
  ;:mouse-pressed mouse-pressed
  ;:mouse-released mouse-released
  ;:mouse-dragged mouse-dragged
)






;(run museegk :interactive)
; 
;; (stop museegk)