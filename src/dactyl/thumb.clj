(ns dactyl.thumb
  (:require
   [dactyl.connectors :refer
    [triangle-hulls web-post web-post-tr web-post-tl web-post-bl web-post-br]]
   [dactyl.placement :refer
    [cornerrow key-place key-position lastrow mount-height mount-width
     single-plate larger-plate]]
   [dactyl.settings :refer [post-adj]]
   [scad-clj.model :as model]))

(defn deg2rad [degrees]
  (* (/ degrees 180) Math/PI))

(defn thumb-post-tr [config]
  (let [web-post (web-post config)
        mount-height (mount-height config)
        mount-width (mount-width config)]
    (model/translate
     [(- (/ mount-width 2) post-adj)
      (- (/ mount-height 1.15) post-adj) 0] web-post)))

(defn thumb-post-tl [config]
  (let [web-post (web-post config)
        mount-height (mount-height config)
        mount-width (mount-width config)]
    (model/translate
     [(+ (/ mount-width -2) post-adj)
      (- (/ mount-height 1.15) post-adj) 0] web-post)))

(defn thumb-post-bl [config]
  (let [web-post (web-post config)
        mount-height (mount-height config)
        mount-width (mount-width config)]
    (model/translate
     [(+ (/ mount-width -2) post-adj)
      (+ (/ mount-height -1.15) post-adj) 0] web-post)))

(defn thumb-post-br [config]
  (let [web-post (web-post config)
        mount-height (mount-height config)
        mount-width (mount-width config)]
    (model/translate
     [(- (/ mount-width 2) post-adj)
      (+ (/ mount-height -1.15) post-adj) 0] web-post)))

(defn thumborigin [config]
  (let [cornerrow (cornerrow config)
        mount-height (mount-height config)
        mount-width (mount-width config)
        thumb-offsets (config :thumb-offsets)]
    (map + (key-position config 1 cornerrow
                         [(/ mount-width 2) (- (/ mount-height 2)) 0])
         thumb-offsets)))

(defn thumb-tr-place [config shape]
  (->> shape
       (model/rotate (deg2rad  10) [1 0 0])
       (model/rotate (deg2rad -23) [0 1 0])
       (model/rotate (deg2rad  10) [0 0 1])
       (model/translate (thumborigin config))
       (model/translate [-12 -16 3])))

(defn thumb-tl-place [config shape]
  (->> shape
       (model/rotate (deg2rad  10) [1 0 0])
       (model/rotate (deg2rad -23) [0 1 0])
       (model/rotate (deg2rad  10) [0 0 1])
       (model/translate (thumborigin config))
       (model/translate [-32 -15 -2])))

(defn thumb-mr-place [config shape]
  (->> shape
       (model/rotate (deg2rad  -6) [1 0 0])
       (model/rotate (deg2rad -34) [0 1 0])
       (model/rotate (deg2rad  48) [0 0 1])
       (model/translate (thumborigin config))
       (model/translate [-29 -40 -13])))

(defn thumb-ml-place [config shape]
  (->> shape
       (model/rotate (deg2rad   6) [1 0 0])
       (model/rotate (deg2rad -34) [0 1 0])
       (model/rotate (deg2rad  40) [0 0 1])
       (model/translate (thumborigin config))
       (model/translate [-51 -25 -12])))

(defn thumb-br-place [config shape]
  (->> shape
       (model/rotate (deg2rad -16) [1 0 0])
       (model/rotate (deg2rad -33) [0 1 0])
       (model/rotate (deg2rad  54) [0 0 1])
       (model/translate (thumborigin config))
       (model/translate [-37.8 -55.3 -25.3])))

(defn thumb-bl-place [config shape]
  (->> shape
       (model/rotate (deg2rad  -4) [1 0 0])
       (model/rotate (deg2rad -35) [0 1 0])
       (model/rotate (deg2rad  52) [0 0 1])
       (model/translate (thumborigin config))
       (model/translate [-56.3 -43.3 -23.5])))

(defn thumb-1x-layout [config shape]
  (model/union
   (thumb-mr-place config shape)
   (thumb-ml-place config shape)
   (thumb-br-place config shape)
   (thumb-bl-place config shape)))

(defn thumb-15x-layout [config shape]
  (model/union
   (thumb-tr-place config shape)
   (thumb-tl-place config shape)))

(defn thumb [config]
  (model/union
   (thumb-1x-layout config (single-plate config))
   (thumb-15x-layout config (single-plate config))
   (thumb-15x-layout config (larger-plate config))))

(defn thumb-connectors [config]
  (let [cornerrow (cornerrow config)
        lastrow (lastrow config)
        thumb-post-tr (thumb-post-tr config)
        thumb-post-br (thumb-post-br config)
        thumb-post-tl (thumb-post-tl config)
        thumb-post-bl (thumb-post-bl config)
        web-post-tr (web-post-tr config)
        web-post-br (web-post-br config)
        web-post-tl (web-post-tl config)
        web-post-bl (web-post-bl config)]
    (model/union
     (triangle-hulls    ; top two
      (thumb-tl-place config thumb-post-tr)
      (thumb-tl-place config thumb-post-br)
      (thumb-tr-place config thumb-post-tl)
      (thumb-tr-place config thumb-post-bl))
     (triangle-hulls    ; bottom two on the right
      (thumb-br-place config web-post-tr)
      (thumb-br-place config web-post-br)
      (thumb-mr-place config web-post-tl)
      (thumb-mr-place config web-post-bl))
     (triangle-hulls    ; bottom two on the left
      (thumb-bl-place config web-post-tr)
      (thumb-bl-place config web-post-br)
      (thumb-ml-place config web-post-tl)
      (thumb-ml-place config web-post-bl))
     (triangle-hulls    ; centers of the bottom four
      (thumb-br-place config web-post-tl)
      (thumb-bl-place config web-post-bl)
      (thumb-br-place config web-post-tr)
      (thumb-bl-place config web-post-br)
      (thumb-mr-place config web-post-tl)
      (thumb-ml-place config web-post-bl)
      (thumb-mr-place config web-post-tr)
      (thumb-ml-place config web-post-br))
     (triangle-hulls    ; top two to the middle two, starting on the left
      (thumb-tl-place config thumb-post-tl)
      (thumb-ml-place config web-post-tr)
      (thumb-tl-place config thumb-post-bl)
      (thumb-ml-place config web-post-br)
      (thumb-tl-place config thumb-post-br)
      (thumb-mr-place config web-post-tr)
      (thumb-tr-place config thumb-post-bl)
      (thumb-mr-place config web-post-br)
      (thumb-tr-place config thumb-post-br))
     (triangle-hulls    ; top two to the main keyboard, starting on the left
      (thumb-tl-place config thumb-post-tl)
      (key-place config 0 cornerrow web-post-bl)
      (thumb-tl-place config thumb-post-tr)
      (key-place config 0 cornerrow web-post-br)
      (thumb-tr-place config thumb-post-tl)
      (key-place config 1 cornerrow web-post-bl)
      (thumb-tr-place config thumb-post-tr)
      (key-place config 1 cornerrow web-post-br)
      (key-place config 2 lastrow web-post-tl)
      (key-place config 2 lastrow web-post-bl)
      (thumb-tr-place config thumb-post-tr)
      (key-place config 2 lastrow web-post-bl)
      (thumb-tr-place config thumb-post-br)
      (key-place config 2 lastrow web-post-br)
      (key-place config 3 lastrow web-post-bl)
      (key-place config 2 lastrow web-post-tr)
      (key-place config 3 lastrow web-post-tl)
      (key-place config 3 cornerrow web-post-bl)
      (key-place config 3 lastrow web-post-tr)
      (key-place config 3 cornerrow web-post-br)
      (key-place config 4 cornerrow web-post-bl))
     (triangle-hulls
      (key-place config 1 cornerrow web-post-br)
      (key-place config 2 lastrow web-post-tl)
      (key-place config 2 cornerrow web-post-bl)
      (key-place config 2 lastrow web-post-tr)
      (key-place config 2 cornerrow web-post-br)
      (key-place config 3 cornerrow web-post-bl))
     (triangle-hulls
      (key-place config 3 lastrow web-post-tr)
      (key-place config 3 lastrow web-post-br)
      (key-place config 3 lastrow web-post-tr)
      (key-place config 4 cornerrow web-post-bl)))))
