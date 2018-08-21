(ns overtone-guitar-utils.core
  (:require [overtone.music.pitch :refer [note ]]
            [overtone.synth.stringed :refer [guitar-string-notes guitar-chord-frets]]))

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))


(defn chord-to-midis
  "Translates a guitar chord into a chord of midi notes"
  [g-chord]
  (let [froms (into [] (map note guitar-string-notes))
        chordv (into [] g-chord)]
    (for [i (range 6) :let [fret (nth chordv i)
                            base-note (nth froms i)]]
      (if (not= -1 fret)
        (do
          (println "fret: " fret ", base: " base-note )
          (+ fret base-note))
        -1))))

(def guitar-midi-chords
  (zipmap (keys guitar-chord-frets)
          (map (comp
                #(filter (fn [n] (> n -1)) %)
                chord-to-midis
                guitar-chord-frets)
               (keys guitar-chord-frets))))

(defn print-chord-tab [chord]
  (let [ch (cond
             (keyword? chord) (chord guitar-chord-frets)
             (seq? chord) chord
             :else [-1 -1 -1 -1 -1 -1])
        fret-count (first (reverse (sort  ch)))
        string-printer (fn [fret]
                         (apply concat (for [f (range fret-count)]
                                         (if (= fret (inc f))
                                           "|--x--"
                                           "|-----"))))]
    (map string-printer ch)))
