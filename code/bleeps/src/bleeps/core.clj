;; pasuspender -- jackd -r -d alsa
(ns bleeps.core) ;; REPL, too
(use 'overtone.core)
(boot-external-server)

(definst beep [note 440 width 0.7 gate 1]
  (let [sig (+ (* 0.7 (lf-tri note))
               (* 0.3 (lf-tri (* 2 note))))
        env (env-gen (asr 0.001 1 0.01) gate 1 0 1 FREE)]
    (* sig env)))

(defn tone [n]
  (let [hz (clojure.core/int (midi->hz (note n)))
        t (now)]
    (at t (beep hz))
    (at (+ t 150) (ctl beep :gate 0))))

(defn coin-block []
  (let [t (now)]
    (at t (beep (clojure.core/int (midi->hz (note :b5)))))
    (at (+ t 110) (ctl beep :gate 0))
    (at (+ t 120) (beep (clojure.core/int (midi->hz (note :e6)))))
    (at (+ t 250) (ctl beep :gate 0))))

(tone :b5)
(tone :e6)
(stop)
(coin-block)

;; Taken from examples/getting_started/basic.clj
;; Need to make a call?
(def DTMF-TONES {1  [697, 1209]
                 2  [770, 1209]
                 3  [852, 1209]
                 4  [697, 1336]
                 5  [770, 1336]
                 6  [852, 1336]
                 7  [697, 1477]
                 8  [770, 1477]
                 9  [852, 1477]
                 \* [697, 1633]
                 0  [770, 1633]
                 \# [852, 1633]})

(definst dtmf [freq-a 770 freq-b 1633 gate 1]
  (let [sig (* 0.2 (+ (sin-osc freq-a) (sin-osc freq-b)))
        env (env-gen (asr 0.001 1 0.001) gate 1 0 1 FREE)]
    (* sig env)))

(defn dial-number [num-seq]
  (loop [t (now)
         nums num-seq]
    (when nums
      (let [t-on  (+ t 160 (rand-int 200))
            t-off (+ t-on 160 (rand-int 80))
            [a b] (get DTMF-TONES (first nums))]
        (at t-on (dtmf a b))
        (at t-off (ctl dtmf :gate 0))
        (recur t-off (next nums))))))

(defn dial-digit [num]
  (let [t (now) t-on (+ t 0) t-off (+ t 150)
        [a b] (get DTMF-TONES num)]
    (at t-on (dtmf a b))
    (at t-off (ctl dtmf :gate 0))))

(dial-number [\# 0 6 2 1 2 2 4 2 9 8])

(dial-digit 1)
