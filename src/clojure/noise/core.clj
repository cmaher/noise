(ns noise.core
  (:require [clojure.math.numeric-tower :as math])
  (:import (org.kdotjpg.noise OpenSimplexNoise)))

(def ^:dynamic seed
  "The seed used to initialize the noise generator"
  0)

(defmacro simplex-noise-base
  ([& coords]
   (let [noise-gen (gensym)]
     `(let [~noise-gen (OpenSimplexNoise. seed)]
        (.eval ~noise-gen ~@coords)))))

(defn simplex-noise
  ([x y] (simplex-noise-base x y))
  ([x y z] (simplex-noise-base x y z))
  ([x y z w] (simplex-noise-base x y z w)))

(defn fbm-noise
  "noise subjected to fractal Brownian motion"
  [iterations persistence scale low high coords]
  (let [iters (range 0 iterations)
        amps (mapv #(math/expt persistence %) iters)
        max-amp (reduce + amps)
        freqs (mapv #(* scale (math/expt 2 %)) iters)
        make-noise (fn [i]
                     (let [freq (get freqs i)
                           amp (get amps i)
                           scaled-coords (map #(* freq %) coords)
                           iter-noise (apply simplex-noise scaled-coords)]
                       (* iter-noise amp)))
        noises (map make-noise iters)
        noise (reduce + noises)
        average-noise (/ noise max-amp)
        straddle-result (/ (* average-noise (- high low)) 2)
        adjustment (/ (+ high low) 2)]
    (+ straddle-result adjustment)))

;(defn- cartesian-reduce
;  [f coord-seqs]
;  (reduce (fn [memo coord] (assoc memo coord (f coord))) {}
;          (apply combos/cartesian-product coord-seqs)))
;
;(defn fractal-brownian-motion-volume
;  [iterations persistence scale low high & coord-seqs]
;  (cartesian-reduce
;    #(fractal-brownian-motion iterations persistence scale low high %) coord-seqs))
;
;(defn volume
;  "return a map of {'(x y z w) noise-value,
;  with the length of the seq dependent on the number of input seqs"
;  [& coord-seqs]
;  (cartesian-reduce #(apply simplex-noise %) coord-seqs))
;
;; intellectual curiosity
;(defmacro cartesian-reduce [f & sets]
;  (loop [[x & xs] sets
;         args []]
;    (let [a (gensym)
;          m (gensym)]
;        ((fn [fbody]
;           `(reduce (fn [~m ~a] ~fbody) [] ~x))
;          (if (seq xs)
;            (recur xs (conj args a))))
;            `(conj ~m (~f ~@(conj args a))))))
