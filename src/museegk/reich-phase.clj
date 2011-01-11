(definst beep [note 58] 
  (let [freq (midicps note) 
        env  (env-gen (perc 0.1 0.2) 1 1 0 1 :free) 
        src  (sin-osc freq)] 
    (* 0.5 env src))) 
		      
(def pitches [64 66 71 73 74 66 64 73 71 66 74 73]) 
						    
(defn play-scale [time notes sep] 
  (let [next-tick (+ time sep)] 
    (at time (tb303 (first notes))) 
    (apply-at #'play-scale next-tick next-tick (next notes) sep))) 
								   
(defn reich [tempo offset] 
   (let [time (+ 400 (now)) 
         notes (cycle pitches)] 
       (play-scale time 
         notes 
         tempo) 
       (play-scale time 
         notes 
         (+ tempo offset)))) 

(reich 300 3) 



;; Try replacing the beep with this synth, originally inspired by a tb303 
;; patch I found on the SC list.  Start reich playing 
 (reich 300 9) 

;; And then experiment with the parameters by changing the default values 
;; and re-evaluating the synth definition. 
;; wave => 0 or 1 to change the waveforms 
;; cutoff and env => 10 to 10000 to modify filter cutoffs 
;; adsr => envelope params....  attack, decay, and release are in 
;; seconds, while sustain is the amplitude from 0.0 to 1.0 
;; r      => filter resonance, try from 0.01 to 1 
;; Lots of different sounds here. 

(defsynth tb303 [note 60 wave 1 
                 cutoff 100 r 0.9 
                 attack 0.101 decay 1.8 sustain 0.2 release 2.0 
                 env 200 gate 0 vol 0.8] 
  (let [freq (midicps note) 
        freqs [freq (* 1.01 freq)] 
        vol-env (env-gen (adsr attack decay sustain release) 
                         (line:kr 1 0 (+ attack decay release)) 
                         :action :free) 
        fil-env (env-gen (perc)) 
        fil-cutoff (+ cutoff (* env fil-env)) 
        waves [(* vol-env (saw freqs)) 
               (* vol-env [(pulse (first freqs) 0.5) (lf-tri (second freqs))])]] 
    (out 0 (* [vol vol] (rlpf (select wave (apply + waves)) fil-cutoff r))))) 