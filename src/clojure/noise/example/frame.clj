(ns noise.example.frame
  (:require [noise.core :as noise])
  (:import (java.awt Color Dimension Frame)))

(defn- set-px [gfx x y color]
  (.setColor gfx color)
  (.fillRect gfx x y 1 1))

(defn make-frame [size]
  "convenience method to make an AWT fram of dimensions size x size"
  (let [frame (Frame.)
        _ (.setSize frame (Dimension. size size))
        _ (.setVisible frame true)]
    frame))

(defn standard-noise [x y]
  "standard terrain-like noise"
  (noise/scale (noise/fbm 8 0.5 0.01 [x y]) 0 255))

(defn staged-noise [x y]
  "build noise in multiple stages.
  Chunky sets the general shape, increasing local grouping
  and refined applies the motion"
  (let [chunky (noise/scale (noise/fbm 1 0 0.003 [x y]) 0 255)
        refined (noise/scale (noise/fbm 6 0.52 0.01 [x y]) 0 255)]
    (+ (* 0.5 chunky) (* 0.5 refined))))

(defn colorize-earth [grey]
  (if (< 138 grey)
    (Color. 0 grey 0)
    (Color. 0 0 grey)))

(defn colorize-greyscale [grey]
  (Color. grey grey grey))

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
   (noise-frame frame colorize-greyscale)))

