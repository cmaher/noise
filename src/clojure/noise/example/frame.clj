(ns noise.example.frame
  (:require [noise.core :as noise])
  (:import (java.awt Color Dimension Frame)))

(defn- set-px [gfx x y color]
  (.setColor gfx color)
  (.fillRect gfx x y 1 1))

(defn make-frame [size]
  (let [frame (Frame.)
        _ (.setSize frame (Dimension. size size))
        _ (.setVisible frame true)]
    frame))

(defn standard-noise [x y]
  (noise/fbm-noise 8 0.5 0.01 0 255 [x y]))

(defn half-iter-noise [x y]
  (noise/fbm-noise 4 0.5 0.01 0 255 [x y]))

(defn earth [grey]
  (if (< 178 grey)
    (Color. 0 grey 0)
    (Color. 0 0 grey)))

(defn greyscale [grey] #(Color. grey grey grey))

(defn noise-frame
  ([frame colorize get-noise]
   (let [gfx (.getGraphics frame)
         size (.getWidth frame)
         size-range (range 0 size)]
     (doseq [x size-range
             y size-range]
       (let [px-noise (get-noise x y)
             int-noise (int px-noise)
             color (colorize int-noise)]
         (set-px gfx x y color)))))
  ([frame colorize]
   (noise-frame frame colorize standard-noise))
  ([frame]
   (noise-frame frame greyscale)))

