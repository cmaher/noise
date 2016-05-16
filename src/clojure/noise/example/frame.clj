(ns noise.example.frame
  (:require [noise.core :as noise])
  (:import (java.awt.image BufferedImage)
           (java.awt Color Dimension Frame)))

(set! *warn-on-reflection* true)

(def default-noise-gen (noise/noise-gen 0))

(defn standard-noise
  "standard terrain-like noise"
  ([noise-gen x y]
   (noise/scale (noise/fbm noise-gen 5 0.5 0.01 [x y]) 0 255))
  ([x y] (standard-noise default-noise-gen x y)))

(defn staged-noise
  "build noise in multiple stages.
  Chunky sets the general shape, increasing local grouping
  and refined applies the motion"
  ([noise-gen x y]
   (let [chunky (noise/scale (noise/fbm noise-gen 1 0 0.003 [x y]) 0 255)
         refined (noise/scale (noise/fbm noise-gen 6 0.52 0.01 [x y]) 0 255)]
     (+ (* 0.5 chunky) (* 0.5 refined))))
  ([x y] (staged-noise default-noise-gen x y)))

(defn- color [r g b]
  (.getRGB (Color. (int r) (int g) (int b))))

(defn colorize-earth [grey]
  (if (< 138 grey)
    (color 0 grey 0)
    (color 0 0 grey)))

(defn colorize-greyscale [grey]
  (color grey grey grey))


(defn noise-pixels [width height get-noise colorize]
  (for [x (range width)
        y (range height)]
    (let [raw-noise (int (get-noise x y))]
      (colorize raw-noise))))

(defn make-image [width height get-noise colorize]
  "Make an image with the given noise characteristics"
  (let [
        bands 3
        buf-width (* bands width)
        buf-height (* bands height)
        ; the size of a BufferedImage is in terms of the number of bands
        img (BufferedImage. buf-width buf-height BufferedImage/TYPE_INT_RGB)
        raster (.getRaster img)
        pixels (int-array (noise-pixels width height get-noise colorize))]
    (.setDataElements raster 0 0 width height pixels)
    img))

(defn make-frame [width height]
  "convenience method to make an AWT fram of dimensions size x size"
  (let [frame (Frame.)]
    (.setSize frame (Dimension. width height))
    (.setVisible frame true)
    frame))

(defn noise-frame
  ([^Frame frame get-noise colorize]
   (let [gfx (.getGraphics frame)
         width (.getWidth frame)
         height (.getHeight frame)
         img (make-image width height get-noise colorize)]
     (.drawImage gfx img 0 0 nil)))
  ([frame]
   (noise-frame frame standard-noise colorize-greyscale)))

(defn run []
  (def frame (make-frame 300 300)))
