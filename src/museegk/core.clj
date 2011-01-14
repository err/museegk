(ns museegk.core
  (:use [rosado.processing]
        [rosado.processing.applet])
  (:require [overtone.live :as tone]
  	    [clojure.contrib.combinatorics :as combo]))


;; the following were all recorded for 16X16 matricies
(def contemplative [82 105 114 119 133 137 162 165 169 197])

(def dino-dna [27 29 44 79 86 87 101 105 110
	       122 125 130 139 151 166 168
	       171 173 183 190 211 219 226
	       229 234 238 240 246 248 255])


(def snow-day-2 [34 55 65 106 134 141 155 159
		 177 179 182 184 187 189 191
		 209 211 214 216 219 221 223])


(def snow-day [4 8 22 27 34 41 45 65 66
	       68 72 74 77 98 100 102 104
	       106 109 130 132 134 138 162
	       166 170 208 209 212 215 218
	       220 221 223 226 229 232 235]) ; c major


(def brian-rossetti [35 36 37 38 42 43 44 50 61 68 75 88 104
		     105 120 121 122 131 136 137 138 139 148
		     149 152 153 154 155 165 166 174 183 184 189 190 200 201 202 203 204])


(def jason-eric [32 41 44 57 58 61 63 74 75 78 94
		 110 126 134 135 164 165 166 168
		 176 178 195 198 208 212 216 227
		 240 241 242 244 249])


(def zelda-esque [1 21 33 36 38 43 53 73
		  75 77 88 103 105 107 109
		  111 137 141 143 173 175 207])

(def chris-lee-foreal [134 135 164 165 166 168
		       176 178 195 198 208 212 
		       216 227 240 241 242 244 249])


(def chris-lee-11 [42 57 58 74 134 135 164 165
		   166 168 176 178 195 198 208
		   212 216 227 240 241 242 244 249])



;;;; beast-trix
(def suspenz [18 19 20 21 22 39 40 41 58 75 76 84 85
	      101 102 109 115 119 120 130 137 145 150
	      159 167 171 172 174 184 189 194 217 225
	      228 229 232 235 237 240 242 243 244 246
	      247 250 252 254])


;;;; Data (because i'm lousy with loading files/requiring namespaces!)

;; (def note-name-matrix ["c0" "c1" "c2" "c3" "c4" "c5" "c6" "c7"
;; 		       "b0" "b1" "b2" "b3" "b4" "b5" "b6" "b7"
;; 		       "a0" "a1" "a2" "a3" "a4" "a5" "a6" "a7"
;; 		       "g0" "g1" "g2" "g3" "g4" "g5" "g6" "g7"
;; 		       "f0" "f1" "f2" "f3" "f4" "f5" "f6" "f7"
;; 		       "e0" "e1" "e2" "e3" "e4" "e5" "e6" "e7"
;; 		       "d0" "d1" "d2" "d3" "d4" "d5" "d6" "d7"
;; 		       "c0" "c1" "c2" "c3" "c4" "c5" "c6" "c7"])

;; (def p300-matrix ["c0" "c1" "c2" "c3" "c4" "c5" "c6" "c7"
;; 		  "b0" "b1" "b2" "b3" "b4" "b5" "b6" "b7"
;; 		  "a0" "a1" "a2" "a3" "a4" "a5" "a6" "a7"
;; 		  "g0" "g1" "g2" "g3" "g4" "g5" "g6" "g7"
;; 		  "f0" "f1" "f2" "f3" "f4" "f5" "f6" "f7"
;; 		  "e0" "e1" "e2" "e3" "e4" "e5" "e6" "e7"
;; 		  "d0" "d1" "d2" "d3" "d4" "d5" "d6" "d7"
;; 		  "c0" "c1" "c2" "c3" "c4" "c5" "c6" "c7"])


;; (def sample-paths

;;      {:c4 "/Users/ike/code/clj/museegk/samples/C4.wav"
;;       :b "/Users/ike/code/clj/museegk/samples/B.wav"
;;       :a "/Users/ike/code/clj/museegk/samples/A.wav"
;;       :g "/Users/ike/code/clj/museegk/samples/G.wav"
;;       :f "/Users/ike/code/clj/museegk/samples/F.wav"
;;       :e "/Users/ike/code/clj/museegk/samples/E.wav"
;;       :d "/Users/ike/code/clj/museegk/samples/Dnew.wav"
;;       :c3 "/Users/ike/code/clj/museegk/samples/C3.wav"} 

;;      ;; simple pattern I like
;;      ;; {:d "/Users/ike/code/clj/museegk/samples/D.wav"
;;      ;;  :e "/Users/ike/code/clj/museegk/samples/E.wav"
;;      ;;  :c3 "/Users/ike/code/clj/museegk/samples/C3.wav"
;;      ;;  :g "/Users/ike/code/clj/museegk/samples/G.wav"
;;      ;;  :f "/Users/ike/code/clj/museegk/samples/F.wav"
;;      ;;  :b "/Users/ike/code/clj/museegk/samples/B.wav"
;;      ;;  :a "/Users/ike/code/clj/museegk/samples/A.wav"
;;      ;;  :c4 "/Users/ike/code/clj/museegk/samples/C4.wav"}
;;      )

;; (defn load-samples []
;;   (doall (map tone/sample (vals sample-paths))))

;;Global Values
(def *framerate*        60)
(def *screen-width*    825)
(def *screen-height*   925);     825)
(def *bg-color*         51)
(def *hl-active-color* 255)
(def *hl-inactive-color* 220)
(def *active-color*    185)
(def *inactive-color*   80)
(def *intensified-color* 220)
(def *matrix-rows*      16) ;; 16)
(def *matrix-cols*      16)  ;; 16)
(def *button-height*    40)
(def *button-width*     40)
(def *border-width*     20)
(def *dx*               10)
(def *dy*               10)
(def *bpm*              180)


;;Atoms
(def *time* (atom 0))
(def *mouse-position* (atom [0 0]))
(def *mouse-button* (atom nil))
; nil  - not-pressed
; true - pressed



;; for the (clear-matrix) button hidden in the corner
(def *wipe-matrix-trigger* (atom nil))
(def *draw-wipe-matrix-button* (atom nil))
(def *wipe-w* 100)
(def *wipe-h* 100)
(def *wipe-x* (- *screen-width* *wipe-w*))
(def *wipe-y* (- *screen-height* *wipe-h*))


;;Structs
(defrecord rgb-color [r g b a]);;unused?
(defrecord hsb-color [h s b a]);;unused?
(defrecord button [idx x-pos y-pos edge-length color instr snd active?])
(defrecord ticker [x-pos y-pos radius color])


;;;;;; note sequences
(defn load-scales [root n & mode]
  "loads n octaves of the provided root scale"
  (let [mode (or (and mode (first mode)) :major)]
    (take (* n 8) (drop (* 7 4) (tone/scale (keyword root) mode)))))

(def *note-matrix*  (ref nil))

(defn map-inst
  "(map-inst [[tb303 (0) 64]])
   (map-inst [[tb303 :row [0 1 2]]
              [tb404 :row [3 4 5]]])
   (map-inst tb303)"
  [kvs]
  (let [inst (map first kvs)
	indices (map second kvs)]))

(defn map-midi [midi]
  (dosync
   (ref-set *note-matrix* midi)
   (ref-set *matrix* (gen-matrix (matrix-coords *matrix-rows* *matrix-cols*)
				 (map :instr @*matrix*)
				 midi
				 (map :active? @*matrix*)))))

(defn change-key
  ([root]
   (map-midi (reverse (flatten (map #(repeat *matrix-cols* %)
				    (load-scales root (/ *matrix-rows* 8) :major))))))
  ([root mode]
   (map-midi (reverse (flatten (map #(repeat *matrix-cols* %)
				    (load-scales root (/ *matrix-rows* 8) mode)))))))


(defn ticker-positions
  "Generates an infinite sequence of x-coords
   for a matrix that is n rows tall. Uses iterate, 
   returning [init, (f init), (f (f init)), (f (f (f init))) ...]"
  ([]
     (ticker-positions *matrix-rows*
		       (fn [pos] (+ pos (+ *button-width* *dx*)))
		       (+ *border-width* (/ *button-width* 2))))
  ([n f init]
     (cycle (take n (iterate f init)))))
								   
								   
;; trying to time the light flashes
(def *ticker-position* (ref nil))
							 
(def *ticker* (ref nil))

(defn init-ticker
  "instantiates an instance of the ticker record"
  ([]
     (let [bottom-border (- *screen-height*
			    (+ *button-height*
			       (* 2 *border-width*)
			       (* (dec *matrix-rows*)
				  (+ *button-height* *dy*))))]
       (ticker. (first @*ticker-position*)	        ;xpos
		(- *screen-height* (/ bottom-border 2))	;ypos
		(/ bottom-border 4)			;radius
		95)))                                  ;color
  ([x y radius color]
     (ticker. (first @*ticker-position*) x y radius)))

(defn draw-ticker [tic]
  (let [x (:x-pos tic)
	y (:y-pos tic)
	r (:radius tic)
	c (:color tic)]
    (ellipse-mode RADIUS) ;we specify radius, not diameter
    (fill c 128 180)
    (ellipse x y r r)))

(defn move-ticker [tic]
  (ticker. (first @*ticker-position*) (:y-pos tic) (:radius tic) (:color tic)))

(defn update-ticker []
  (dosync (alter *ticker-position* next)
	  (alter *ticker* move-ticker)))



;;;; Utils


;;(defrecord button [x-pos y-pos edge-length color snd active?])
					;
(defn create-button [idx x y len color instr note active?]
  (button. idx x y len color instr note active?))


(defn n-iterate
  "Returns a (length n) sequence of x, (f x), (f (f x)), etc"
  [n f x]
  (take n (iterate f x)))


(defn coords [n f x]
  (n-iterate n f x))
		    

(defn row-coords
  "returns a seq of the y-coordinates of each row in an m-row matrix,
   given an initial starting value y, and a function f, which is called iteratively on y.
   result:  [ y , (f y) , (f (f y)) , (f (f (f y))) , etc. ] 

   hint: y = *border-width*
         f = #(+ % *button-height* *row-offset*) "
  [m f y]
  (coords m f y))


(defn col-coords
  "returns a seq of the x-coordinates of each row in an n-column matrix,
   given an initial starting value x, and a function f, which is called iteratively on x.
   result:  [ x , (f x) , (f (f x)) , (f (f (f x))) , ..etc. ] 

   hint: x = *border-width*
         f = #(+ % *button-width* *col-offset*)"
  [m f x]
  (coords m f x))


(defn cartesian-product
  "rows and cols are seqs of y and x values (respectively)
   used to lay out the matrix on a cartesian plane."
  [rows cols]
  (combo/cartesian-product rows cols))

(defn gen-matrix [points instruments notes & pattern]
  (let [len *button-width*
	colr *inactive-color*]
    ;; (println "points: " points)
    (doall
     (map (fn
	    [idx xy instr note state]
	    (button. idx (second xy) (first xy) len
		     (or (and state *active-color*) *inactive-color*)
		     instr note state))
	  (iterate inc 0)
	  points
	  (or (and (seq? instruments) instruments) (repeat instruments))
	  notes
	  ;; (first pattern)
	  (or (and pattern (first pattern)) (repeat false))
	  ))))

(defn matrix-coords [m n]
  (let [row-offset-fun #(+ % *button-height* *dy*)
	col-offset-fun #(+ % *button-width*  *dx*)
	first-row-pos  *border-width*
	first-col-pos  *border-width*
	rows   (row-coords m row-offset-fun first-row-pos)
	cols   (col-coords n col-offset-fun first-col-pos)
	coords (cartesian-product rows cols)]
    coords))				       
				       
(defn create-matrix [m n instr notes pattern]
  (gen-matrix (matrix-coords m n) instr notes pattern))



 ;(one day we may want to pre-load patterns)

;; (defn create-matrix [rows cols]
;;   (let [init-point [*border-width* *border-width*]
;; 	length *button-height*
;; 	diff-x *dx*
;; 	diff-y *dy*
;; 	]
    
;;     (defn make-button
;; ;     ([b] (create-button (:x-pos b) (:y-pos b) (:edge-length b) (:color b) (:snd b) (:active? b)))
;;       [i x y s] (create-button i x y length *inactive-color* s false))

;;     (loop [res [] row 0 col 0 idx 0 samples (load-samples)]
;;       (let [button (make-button idx
;; 				(+ (first init-point)
;; 				   (+ (* col diff-x)
;; 				      (* col length)))
;; 				(+ (second init-point)
;; 				   (+ (* row diff-y)
;; 				      (* row length)))
;; 				(first samples))
;; 	    end-of-row? (= col (dec cols))
;; 	    end-of-col? (= row (dec rows))]
;; 	(if (and end-of-row? end-of-col?)
;; 	  (conj res button)
;; 	  (recur (conj res button)
;; 		 (if end-of-row? (inc row) row)
;; 		 (if end-of-row? 0 (inc col))
;; 		 (inc idx)
;; 		 (if end-of-row? (next samples) samples)))))))

(def *matrix* (ref nil))

(defn clear-matrix []
  (let [matrix @*matrix*
	instr (map :instr matrix)
	notes (map :snd matrix)
	pattern (repeat false)]
    (dosync
     (ref-set *matrix* (create-matrix *matrix-rows*
				      *matrix-cols*
				      instr notes pattern))))
  "matrix cleared")

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
  


;; (let [target nil]
;;   (map (fn [elt] (if (some #(== (:idx elt) %) target)
;; 		   (button. (:idx elt) (:x-pos elt) (:y-pos elt) (:edge-length elt)
;; 			     (if turn-on? active-b inactive-b); (:active? cur-b) inactive-b active-b)
;;; 			     (:snd cur-b) (if turn-on? true false) ))) vec))

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
			     (:instr cur-b)
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
			     (:instr cur-b)
			     (:snd cur-b) (:active? cur-b))
					;de-highlight
		    (button. idx  (:x-pos cur-b) (:y-pos cur-b) (:edge-length cur-b)
			     (if (:active? cur-b) active-b inactive-b)
			     (:instr cur-b)
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
			 (:instr cur-b)
			 (:snd cur-b) (:active? cur-b))
		;de-highlight
		(button. idx  (:x-pos cur-b) (:y-pos cur-b) (:edge-length cur-b)
			 (if (:active? cur-b) active-b inactive-b)
			 (:instr cur-b)
			 (:snd cur-b) (:active? cur-b)))]
	(if (= idx vec-len)
	  (conj res new-b)
	  (recur (conj res new-b) (inc idx)))))))

  ;; (let [vec-len (dec (.size vec))
  ;; 	row-len  *matrix-cols*
  ;; 	active-b  *active-color*
  ;; 	inactive-b *inactive-color*
  ;; 	active-hl   *hl-active-color*
  ;; 	inactive-hl  *hl-inactive-color*]
  ;;   (loop [res [] idx 0]
  ;;     (let [cur-b (nth vec idx)
  ;; 	    new-b (button. idx (:x-pos cur-b) (:y-pos cur-b) (:edge-length cur-b)
  ;; 			   (if (= n (int (Math/floor (/ idx row-len))))  
  ;; 			     (if (:active? cur-b) active-hl inactive-hl)
  ;; 			     (if (:active? cur-b) active-b inactive-b))
			     
  ;; 			     ;; (comment
  ;; 			     ;; 	 (cond (= (:color cur-b) active-b) active-b
  ;; 			     ;; 	       (= (:color cur-b) inactive-b) inactive-hl
  ;; 			     ;; 	       :else (:color cur-b)))

  ;; 			   (:snd cur-b) (:active? cur-b)) ;)
  ;; 	    ]
  ;; 	(if (= idx vec-len)
  ;; 	  (conj res new-b)
  ;; 	  (recur (conj res new-b) (inc idx))))))
;;;;)


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
			 (:instr cur-b)
			 (:snd cur-b) (:active? cur-b))
		;de-highlight
		(button. idx  (:x-pos cur-b) (:y-pos cur-b) (:edge-length cur-b)
			 (if (:active? cur-b) active-b inactive-b)
			 (:instr cur-b)
			 (:snd cur-b) (:active? cur-b)))]
	(if (= idx vec-len)
	  (conj res new-b)
	  (recur (conj res new-b) (inc idx)))))))




(defn destructive-pop [seq]
    (let [h (first @seq)]
      (swap! seq next)
      h))



;; hm.. could use some refactoring
(let [rows (atom (apply concat (iterate #(shuffle %) (shuffle (range *matrix-rows*)))))
      cols (atom (apply concat (iterate #(shuffle %) (shuffle (range *matrix-cols*)))))]

  (defn rand-row []
    (destructive-pop rows))
  
  (defn rand-col []
    (destructive-pop cols)))
			    
			    

(def *rand-cols* (ref (shuffle (range *matrix-rows*))))

(defn shffl [n] (shuffle (range n)))


(let [rc-toggle (atom 1)]
  (defn update []
    ;; (when (zero? (mod @*time* 4))
    ;;   (if (pos? @rc-toggle)
    ;;   	(dosync
    ;;   	 (let [r (rand-row)]
    ;;   	   ;(println "\n ROW: " r)
    ;;   	   (alter *matrix* (fn [vec n] (hl vec (nth-row n *matrix-rows* *matrix-cols*))) r ))) ;(rand-row)
    ;;   	(dosync
    ;;   	 (let [c (rand-col)]
    ;;   	   ;(println "\n col: " c)
    ;;   	   (alter *matrix* (fn [vec n] (hl vec (nth-col n *matrix-rows* *matrix-cols*))) c )))  ;	 (rand-col)
    ;;   	)
    ;;   (swap! rc-toggle #(* % -1)))
    
    ))

					;(if (zero? (int (Math/floor (rand 2)))) ;row/col coin toss
					;(dosync (alter *matrix* (fn [vec row] (hl-row vec row)) (int (rand *matrix-rows*))))
					; (dosync (alter *matrix* (fn [vec col] (hl-col vec col)) (int (rand *matrix-cols*)))))
;; (dosync (alter *matrix*
;; 		   (fn [vec n0 n1] (hl-col (hl-row vec n0) n1))
;; 		   (int (rand *matrix-rows*))
;; 		   (int (rand *matrix-cols*))))


;; (defrecord rgb-color [r g b a])
;; (defrecord hsb-color [h s b a])
;; (defrecord button [idx x-pos y-pos edge-length color instr snd active?])
(defn draw-button [button]
  (let [x (:x-pos button)
	y (:y-pos button)
	c (:color button)
	idx (:idx button)
	l (:edge-length button)]
    
    (rect-mode CORNER)
    (no-stroke)				;(stroke-weight 3)
					;(stroke 255)

    (fill c 128 180)
    ;; (if (= c  *hl-inactive-color*)
    ;;   (fill 60 128 180)
    ;;   (fill c 128 180))
					;pretty colors    (fill (mod x 256)  128 180)
    (rect x y l l)))

    ;; now draw the note name.
    ;; for now we use a shitty
    ;; hack, since the indices
    ;; of note-matrix are 1-1
    ;; with the actual step sequencer
    ;;
    ;; (fill 75)
    ;; (string->text
    ;;  (nth note-name-matrix idx)
    ;;  (+ x (/ l 2))
    ;;  (+ y 10 (/ l 2))) ;;;;  ));thus ended the draw-button function

(defn draw-matrix [m]
  (dorun (map draw-button m)))

(defn draw-wipe-button []
  (and
   (or (stroke-float 125) (fill 255) 1)
   @*wipe-matrix-trigger*
   (or (stroke-float 245 128 180) (fill 245 128 180) 1))

  ;; throw some text out there
  (string->text "(clear)"
		(+ 3 *wipe-x* (/ *wipe-w* 2))
		(+ 5 *wipe-y* (/ *wipe-h* 2)))
  
  ;; now draw the button
  (no-fill)
  (stroke-weight 4)
  (rect-mode CORNER)
  (rect *wipe-x* *wipe-y* *wipe-w* *wipe-h*))

(let [poses (take *matrix-cols*
		  (ticker-positions *matrix-rows*
				    (fn [pos] (+ pos (+ *button-width* *dx*)))
				    (+ *border-width* (/ *button-width* 2))))]
  (defn draw-small-tickers []
    (let [tic @*ticker*]
      (dorun (map (fn [pos]
		    (let [x pos
			  y (:y-pos tic)
			  r (:radius tic)
			  c (:color tic)]
		      (ellipse-mode RADIUS)
		      (fill 128 128 128)
		      (ellipse x y (/ r 4) (/ r 4))))
		  poses)))))

(defn draw []
  (color-mode RGB)
  (background 0)
  (draw-matrix @*matrix*)
  ;; (draw-small-tickers)
  ;; (draw-ticker @*ticker*)
  (swap! *time* inc)
  ;  (draw-wipe-button)
  (and @*draw-wipe-matrix-button* (draw-wipe-button))
  (update))


;;;;; collision detection
(defn in-quad
  "returns true when x and y are within the quad of 
   whose top-left corner is (qx, qy) and dimensions are qh qw"
  [x y q-x q-y q-w q-h]
  (and (> x q-x) (< x (+ q-x q-w))
       (> y q-y) (< y (+ q-y q-h))))

(defn over-wipe-matrix-button?
  "returns true when mouse is hovering over the wipe-matrix button"
  [x y]
  (in-quad x y *wipe-x* *wipe-y* *wipe-w* *wipe-h*))


(defn collision? 
  "returns a vector of [collision? idx]"
  [vec x y]
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



;;;; INPUT EVENTS

(def *toggle-on* (atom false))
(def *starting-block* (atom -1))
(def *visited* (atom nil))


(defn mouse-moved [evt]
  (let [x (.getX evt)
	y (.getY evt)
	over-button (over-wipe-matrix-button? x y)] ;rename as over-hidden button ... or refactor
    (reset! *mouse-position* [x y])
    (reset! *draw-wipe-matrix-button* over-button)
    (when (not over-button)
      (compare-and-set! *wipe-matrix-trigger* true false))))
							    

(defn mouse-pressed [evt]
  (let [x (.getX evt)
	y (.getY evt)
	b @*mouse-button*]
    (println "type of event: " (type evt))

    (if (over-wipe-matrix-button? x y)
      (swap! *wipe-matrix-trigger* not))
    
    (let [[selection? idx] (collision? @*matrix* x y)]
      (println selection? "\n" idx)
      (if selection?
	(dosync
	 (if (:active? (nth @*matrix* idx))
	   (reset! *toggle-on* false)
	   (reset! *toggle-on* true))
	 (reset! *starting-block* idx)
	 (swap!  *visited* conj idx)
	 (alter *matrix* (fn [vec ind] (activate vec [ind] @*toggle-on*)) idx))
	(reset! *toggle-on* true)))
    (swap! *mouse-button* not)))

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
    (swap! *mouse-button* not)
    (and @*wipe-matrix-trigger*
	 (over-wipe-matrix-button? x y)
	 (clear-matrix))
    (swap! *wipe-matrix-trigger* not)))

(defn mouse-dragged [evt]
  (let [x (.getX evt)
	y (.getY evt)]
    (reset! *draw-wipe-matrix-button* (over-wipe-matrix-button? x y))
    (let [[selection? idx] (collision? @*matrix* x y)]
      (println "mouse-dragged:  " selection? "\n" idx "\nvisited: " @*visited* "\ntoggle: " @*toggle-on*  "\n")
      (when selection? 
	(dosync
	 (when (not (some #(== idx %) @*visited*))
	   (alter *matrix* (fn [vec ind] (activate vec [ind] @*toggle-on*)) idx)))
	(swap! *visited* #(distinct (conj % idx)))))
    (reset! *mouse-position* [x y])))



(defn press-aux  
  "presses button at given idx"
  ([vec bidx]
     (let [vec-len (dec (* *matrix-rows* *matrix-cols*))]
       (loop [res [] idx 0]
	 (let [cur-b (nth vec idx)
	       new-b (if (= idx bidx)
		       (button. idx  (:x-pos cur-b) (:y-pos cur-b) (:edge-length cur-b)
				;; the following shouldn't happen here 
				;; why not dispatch on :active? in (draw-button) to determine color
				(if (:active? cur-b) *inactive-color* *active-color*) 
				(:instr cur-b)
				(:snd cur-b)  (not (:active? cur-b)))
		       cur-b)]
	   (if (= idx vec-len)
	     (conj res new-b)
	     (recur (conj res new-b) (inc idx))))))))

(defn press-aux-neu [vec bidx]
  (let [button (nth vec bidx)]
   (assoc vec bidx (not (:active? button)))))

(defn press
  "toggles the button at idx on/off"
  [idx]
  (dosync (alter *matrix* (fn [m id] (press-aux m id)) idx)))

(defn turn-on
  "turns on the button at idx"
  [ids]
  (let [indices (or (and (seq? ids) ids) (seq [ids]))]
   (dosync (alter *matrix* (fn [m ids] (activate m ids true)) indices))))

(defn load-song [notes]
  (clear-matrix)
  (dorun (map press notes)))

(defn load-song* [notes] (turn-on (seq notes)))

(def m-agent (agent nil))

(def metro (tone/metronome *bpm*))

;; (def tones
;;      (map #([(:idx %) (:snd %) (:active? %)])
;; 	  @*matrix*))

(defn get-button [idx]
 (nth @*matrix* idx)
  ;; (nth @tones idx)

  )

;;; "test" version
;; (def tb303
;;      (tone/synth [note 60 wave 1 
;; 		  cutoff 100 r 0.9 
;; 		  attack 0.301 decay 0.8 sustain 1.2 release 0.102 
;; 		  env 80 gate 0 vol 0.8]
;; 		 (let [freq (tone/midicps note) 
;; 		       freqs [freq (* 1.01 freq) (* 1.03 freq)] 
;; 		       vol-env (tone/env-gen (tone/adsr attack decay sustain release) 
;; 					     (tone/line:kr 1 0 (+ attack decay release)) 
;; 					     :action :free)
;; 		       fil-env (tone/env-gen (tone/perc)) 
;; 		       fil-cutoff (+ cutoff (* env fil-env)) 
;; 		       waves [(* vol-env (tone/saw freqs)) 
;; 			      (* vol-env [(tone/pulse (first freqs) 0.8)
;; 					  (tone/lf-tri (second 
;; 							freqs))])
;; 			      (* vol-env [(tone/pulse (first freqs) 0.8)
;; 					  (tone/lf-tri (second (rest freqs)))])]] 
;; 		   (tone/out 0 (* [vol vol] (tone/rlpf (tone/select wave (apply + waves)) fil-cutoff r))))))

;;; "real" version
(def tb303
     (tone/synth [note 60 wave 1 
		  cutoff 100 r 0.9 
		  attack 0.101 decay 0.05 ;; (cos @*time*)
		  sustain 1.16 release 0.9 
		  env  220 ;; (* 80 (sin @*time*))
		  gate 0 vol 0.75]
		 (let [freq (tone/midicps note) 
		       freqs [freq (* 1.01 freq) (* 1.03 freq)] 
		       vol-env (tone/env-gen (tone/adsr attack decay sustain release) 
					     (tone/line:kr 1 0 (+ attack decay release)) 
					     :action :free) 
		       fil-env (tone/env-gen (tone/perc)) 
		       fil-cutoff (+ cutoff (* env fil-env)) 
		       waves [(* vol-env (tone/saw freqs)) 
			      (* vol-env [(tone/pulse (first freqs) 0.5)
					  (tone/lf-tri (second 
							freqs))])
			      (* vol-env [(tone/pulse (first freqs) 0.5)
					  (tone/lf-tri (second (rest freqs)))])]] 
		   (tone/out 0 (* [vol vol] (tone/rlpf (tone/select wave (apply + waves)) fil-cutoff 
						       r))))))

;; 2011 version
(def tb303
     (tone/synth [note 60 wave 1 
		  cutoff 100 r 0.9 
		  attack 0.101 decay 0.03 ;; (cos @*time*) ;; 0.05
		  sustain 1.16 release 0.2 
		  env  (* 80 (sin @*time*))  ;; 220
		  gate 0 vol 0.75]
		 (let [freq (tone/midicps note) 
		       freqs [freq (* 1.01 freq) (* 1.03 freq)] 
		       vol-env (tone/env-gen (tone/adsr attack decay sustain release) 
					     (tone/line:kr 1 0 (+ attack decay release)) 
					     :action :free) 
		       fil-env (tone/env-gen (tone/perc)) 
		       fil-cutoff (+ cutoff (* env fil-env)) 
		       waves [(* vol-env (tone/saw freqs)) 
			      (* vol-env [(tone/pulse (first freqs) 0.5)
					  (tone/lf-tri (second 
							freqs))])
			      (* vol-env [(tone/pulse (first freqs) 0.5)
					  (tone/lf-tri (second (rest freqs)))])]] 
		   (tone/out 0 (* [vol vol] (tone/rlpf (tone/select wave (apply + waves)) fil-cutoff 
						       r))))))


;; butchered tb303
(def tb303
     (tone/synth [note 60 wave 1 
		  cutoff 100 r 0.9 
		  attack 0.101 decay 0.03 ;; (cos @*time*) ;; 0.05
		  sustain 1.16 release 0.2 
		  env  (* 80 (sin @*time*)) ;; 220
		  gate 0 vol 0.75]
		 (let [freq (tone/midicps note) 
		       freqs [freq (* 4.00 freq) (* 2.00 freq)] 
		       vol-env (tone/env-gen (tone/adsr attack decay sustain release) 
					     (tone/line:kr 1 0 (+ attack decay release)) 
					     :action :free) 
		       fil-env (tone/env-gen (tone/perc)) 
		       fil-cutoff (+ cutoff (* env fil-env)) 
		       waves [(* vol-env (tone/saw freqs)) 
			      (* vol-env [(tone/pulse (first freqs) 0.5)
					  (tone/lf-tri (second freqs))])
			      (* vol-env [(tone/pulse (first freqs) 0.5)
					  (tone/lf-tri (second (rest freqs)))])]] 
		   (tone/out 0 (* [vol vol] (tone/rlpf (tone/select wave (apply + waves)) fil-cutoff r))))))


(defn simple-player [beat buttons]
  (let [button (get-button (first buttons))
	active? (:active? button)
	note (:snd button)
	instr (:instr button)]

    (tone/at (metro beat)
	     (when active? (instr note))) 
    

    (tone/apply-at (metro (inc beat))
		   #'simple-player
		   [(inc beat)
		    (next buttons)])))


(defn simple-player2 [beat buttons]
  (let [button (get-button (first buttons))
	active? (:active? button)
	note (:snd button)
	instr (:instr button)]

    (tone/at (metro beat)
	     (update-ticker)
	     (when active? (instr note))) ;plays sample
    

    (tone/apply-at (metro (inc beat))
		   #'simple-player2
		   [(inc beat)
		    (next buttons)])))




(defn simple-clock [beat]
  (tone/periodic #(update-ticker)
  		 (tone/beat-ms 1 *bpm*)
		 (- (metro (inc beat)) (tone/now))))



(def *font* (atom nil))
					
(defn setup
  "executes once."
  []
  (smooth)
  (background-float 200 200 255)
  (framerate *framerate*)

  ;; (dosync (ref-set *note-matrix* (reverse (flatten (map #(repeat *matrix-cols* %)
  ;; 						 (load-scales :c (/ *matrix-rows* 8)))))))
  (dosync
    (ref-set
     *note-matrix* (reverse (flatten (map #(repeat *matrix-cols* %)
					  (load-scales :c (/ *matrix-rows* 8))))))
											 
    (ref-set
     *matrix* (create-matrix *matrix-rows* *matrix-cols* #'tb303
			     @*note-matrix* (repeat (* *matrix-rows* *matrix-cols*) false)))

   (ref-set
    *ticker-position* (cycle (take *matrix-rows*
				    (iterate #(+ % (+ *button-width* *dx*))
					     (+ *border-width* (/ *button-width* 2))))))
   (ref-set
    *ticker* (init-ticker)))

  (let [rows (map #(nth-row % *matrix-rows* *matrix-cols*)
		  (range *matrix-cols*))]
    (doall
     (map (fn [b]
	    (send (agent nil)
		  (fn [_] (simple-player *matrix-cols* (cycle  b)))))
	  rows)))
  ;; (simple-clock 16)
  (reset! *font* (load-font "Monaco-48.vlw"))
  (text-font @*font* 18)
  (text-align CENTER))


(defn key-pressed [evt]
  (let [char (.getKeyChar evt)]
    (when (= char \space)
      (clear-matrix))))

(defapplet museegk
  :title "Enter the MatrEEGx!"
  :setup setup
  :draw draw
  :size [*screen-width* *screen-height*]
  :mouse-moved mouse-moved
  :mouse-pressed mouse-pressed
  :mouse-released mouse-released
  :mouse-dragged mouse-dragged
  :key-pressed key-pressed)



;; TODO
;; change-scale
;; mute-region
;; snapshots [record song progression]

;; ticker / light-up note


;; (tone/stop)
;; (tone/reset)
;; (run museegk :interactive)
;; (stop museegk)