(ns noise.core
  (:require [clojure.math.numeric-tower :as math])
  (:import (org.kdotjpg.noise OpenSimplexNoise)))

(def ^:dynamic noise-gen
  "The seeded instance of OpenSimplexNoise to use"
  (OpenSimplexNoise. 0))

(defn make-noise-gen [seed] (OpenSimplexNoise. seed))

(defmacro simplex-noise-base
  ([& coords]
    `(.eval noise-gen ~@coords)))

(defn simplex
  ([x y] (simplex-noise-base x y))
  ([x y z] (simplex-noise-base x y z))
  ([x y z w] (simplex-noise-base x y z w)))

(defn scale
  [noise low high]
  (let [straddle-result (/ (* noise (- high low)) 2)
        adjustment (/ (+ high low) 2)]
    (+ straddle-result adjustment)))

(defn fbm
  "noise subjected to fractal Brownian motion"
  [iterations persistence scale coords]
  (let [iters (range 0 iterations)
        amps (mapv #(math/expt persistence %) iters)
        max-amp (reduce + amps)
        freqs (mapv #(* scale (math/expt 2 %)) iters)
        make-noise (fn [i]
                     (let [freq (get freqs i)
                           amp (get amps i)
                           scaled-coords (map #(* freq %) coords)
                           iter-noise (apply simplex scaled-coords)]
                       (* iter-noise amp)))
        noises (map make-noise iters)
        noise (reduce + noises)]
    (/ noise max-amp)))
