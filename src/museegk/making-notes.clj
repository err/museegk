(ns session.sixteen-days
  (:use overtone.live))

(def metro (metronome 90))

(definst kick [freq 120 dur 0.3 width 0.5]
  (let [freq-env (* freq (env-gen (perc 0 (* 0.99 dur))))
        env (env-gen (perc 0.01 dur) 1 1 0 1 :free)
        sqr (* (env-gen (perc 0 0.01)) (pulse (* 2 freq) width))
        src (sin-osc freq-env)
        drum (+ sqr (* env src))]
    (compander drum drum 0.2 1 0.1 0.01 0.01)))

;(kick)

(definst c-hat [amp 0.8 t 0.04]
  (let [env (env-gen (perc 0.001 t) 1 1 0 1 :free)
        noise (white-noise)
        sqr (* (env-gen (perc 0.01 0.04)) (pulse 880 0.2))
        filt (bpf (+ sqr noise) 9000 0.5)]
    (* amp env filt)))

(definst bass [freq 120 t 0.2 amp 0.4]
  (let [env (env-gen (perc 0.1 t) 1 1 0 1 :free)
        src (saw [freq (* 1.02 freq)])
        sub (sin-osc (/ freq 2))
        filt (rlpf src (* 0.8 freq) 0.5)]
    (* (+ src sub) env amp)))

; Sample and hold experiment using a latch
(definst robot [note 40 dur 0.5]
  (let [gate (line 1 0 dur)
        env (env-gen (adsr 0.05 0.8 0.01 0.3)
                     :gate gate
                     :action :free)
        noiz (white-noise)
        modulator (* 24 (latch:kr noiz (lf-pulse:kr 8)))
        src (sin-osc (midicps (+ modulator note)) 0 0.45)]
    src))

; Well, maybe flute is an overstatement :-)
(definst flute [note 94 vel 0.8 dur 0.5]
         (let [freq (midicps note)
               gate (line 1 0 dur)
               env (env-gen (adsr 0.05 0.8 0.01 0.3) gate 1 0 1 :free)
               src (lf-pulse freq 0 0.45)
               noiz (white-noise)
               modulator (slew:kr (latch:kr noiz (lf-pulse:kr 10))
                                  0.2)]
           (* 0.2
             vel env
              (+ (* 0.3 src)
                 (rlpf
                   (+ src
                      (* 0.2 noiz))
                   (+ (* freq modulator)
                      ;(* env (+  (* 6 freq)))
		      (* env (+  (* 3 freq))))
                   0.2)))))

(defn ex-1 [beat notes vels durs]
  (at (metro beat)
      (flute (+ 50 (first notes))
             (first vels)
             (* 0.10 (first durs))))

  (at (metro (+ 0.51 beat))
      (c-hat (+ 0.2 (* 0.2 (rem beat 3)))))

  (at (metro (+ 0.01 beat))
      (c-hat))
  
  (if (= 0 (mod beat 3))
    (at (metro (+ 0.5 beat))
        (flute (+ 62 (first notes))
               (first vels)
               (* 0.1 (first durs))))
    (at (metro (+ 0.75 beat))
        (flute (+ 62 (first notes))
               (first vels)
               (* 0.4 (first durs)))))
  
  (if (= 0 (mod beat 2))
    (at (metro (+ 0.51 beat))
	(kick)))
  
  (if (= 0 (mod beat 5))
    (at (metro beat)
	(kick)))
  
  (apply-at #'ex-1 (metro (inc beat))
            (inc beat)
            (next notes)
            (next vels)
            (next durs)))

(def notes-1 [:a :f :a :g :e :a])
(def vels-1 [0.3 0.4 0.3 0.7 0.4 0.4 0.1 0.5])
(def durs-1 [1 3 0.5 2])

;(ex-1 (metro) (cycle (map NOTE notes-1)) (cycle vels-1) (cycle durs-1))

;(reset)