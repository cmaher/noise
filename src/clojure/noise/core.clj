(ns noise.core
  (:require [clojure.math.numeric-tower :as math])
  (:import (org.kdotjpg.noise OpenSimplexNoise)
           (java.util Date)))

(set! *warn-on-reflection* true)

(defn noise-gen
  ([] (noise-gen (.getTime (Date.))))
  ([^long seed] (OpenSimplexNoise. seed)))

(defmacro simplex-noise-base
  ([noise-gen & coords]
    `(.eval ~noise-gen ~@coords)))

(defn simplex
  ([noise-gen x y] (simplex-noise-base noise-gen x y))
  ([noise-gen x y z] (simplex-noise-base noise-gen x y z))
  ([noise-gen x y z w] (simplex-noise-base noise-gen x y z w)))

(defn scale
  [noise low high]
  (let [straddle-result (/ (* noise (- high low)) 2)
        adjustment (/ (+ high low) 2)]
    (+ straddle-result adjustment)))

(defn fbm
  "noise subjected to fractal Brownian motion"
  [noise-gen iterations persistence scale coords]
  (let [iters (range 0 iterations)
        amps (mapv #(math/expt persistence %) iters)
        max-amp (reduce + amps)
        freqs (mapv #(* scale (math/expt 2 %)) iters)
        make-noise (fn [i]
                     (let [freq (get freqs i)
                           amp (get amps i)
                           scaled-coords (map #(* freq %) coords)
                           iter-noise (apply simplex noise-gen scaled-coords)]
                       (* iter-noise amp)))
        noises (map make-noise iters)
        noise (reduce + noises)]
    (/ noise max-amp)))
