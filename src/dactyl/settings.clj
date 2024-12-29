(ns dactyl.settings
  (:require [scad-clj.model :as model]))

(def sa-length 18.25)
(def sa-double-length 37.5)

(def post-size 0.1)
(def post-adj (/ post-size 2))
(def web-thickness 3.5)

(def left-wall-x-offset 10)
(def left-wall-z-offset  3)

(def rj9-cube (model/cube 14.78 13 22.38))

(def usb-holder-thickness 4)
(def usb-holder-size [6.5 10.0 13.6])

(def teensy-width 20)
;; (def teensy-height 12)
;; (def teensy-length 33)
;; (def teensy2-length 53)
(def teensy-pcb-thickness 2)
(def teensy-holder-width  (+ 7 teensy-pcb-thickness))
;; (def teensy-holder-height (+ 6 teensy-width))
;; (def teensy-offset-height 5)
(def teensy-holder-top-length 18)

(def screw-insert-height 3.8)
(def screw-insert-bottom-radius (/ 5.31 2))
(def screw-insert-top-radius (/ 5.1 2))

(def wire-post-height 7)
(def wire-post-overhang 3.5)
(def wire-post-diameter 2.6)
