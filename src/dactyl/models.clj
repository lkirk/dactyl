(ns dactyl.models
  (:require
   [clojure.java.io :as io]
   [clojure.pprint :refer [pprint]]
   [dactyl.case :refer [case-walls rj9-space rj9-holder screw-insert-outers
                        screw-insert-holes teensy-holder wire-posts
                        usb-holder usb-holder-hole]]
   [dactyl.connectors :refer [connectors]]
   [dactyl.keycaps :refer [caps thumbcaps]]
   [dactyl.placement :refer [key-holes]]
   [dactyl.thumb :refer [thumb thumb-connectors]]
   [scad-clj.model :as model]
   [scad-clj.scad :as scad]))

(defn model-right [config & {:keys [keycaps]}]
  (model/difference
   (model/union
    (key-holes config)
    (connectors config)
    (thumb config)
    (thumb-connectors config)
    (model/difference
     (model/union
      (case-walls config)
      (screw-insert-outers config)
      (when (config :chip-holder) (teensy-holder config))
      (usb-holder config))
     (rj9-space config)
     (usb-holder-hole config)
     (screw-insert-holes config))
    (rj9-holder config)
    (wire-posts config)
    (when keycaps
      (model/union (thumbcaps config) (caps config))))
   (model/translate [0 0 -20] (model/cube 350 350 40))))

(defn model-left [config & {:keys [keycaps]}]
  (model/mirror [-1 0 0] (model-right config {:keycaps keycaps})))

(defn write-model [config model file & {:keys [keycaps]}]
  (spit file (scad/write-scad (model config {:keycaps keycaps}))))

(defn write-models [config out-path & {:keys [dry-run keycaps]}]
  (if dry-run
    (pprint (into (sorted-map) config))
    (do
      (.mkdirs out-path)
      (let [lfile (io/file out-path "right.scad")
            rfile (io/file out-path "left.scad")]
        @(future (write-model config model-right lfile {:keycaps keycaps}))
        @(future (write-model config model-left rfile {:keycaps keycaps}))))))
