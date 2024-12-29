(ns dactyl.connectors
  (:require
   [dactyl.placement :refer [columns cornerrow key-place lastrow]]
   [dactyl.settings :refer [post-adj post-size web-thickness]]
   [scad-clj.model :as model]))

(defn mount-width [config] (+ (config :keyswitch-width) 3))
(defn mount-height [config] (+ (config :keyswitch-height) 3))
(defn web-post [config]
  (->> (model/cube post-size post-size web-thickness)
       (model/translate [0 0 (+ (/ web-thickness -2)
                                (config :plate-thickness))])))

(defn web-post-tr [config]
  (model/translate [(- (/ (mount-width config) 2) post-adj)
                    (- (/ (mount-height config) 2) post-adj) 0]
                   (web-post config)))
(defn web-post-tl [config]
  (model/translate [(+ (/ (mount-width config) -2) post-adj)
                    (- (/ (mount-height config) 2) post-adj) 0]
                   (web-post config)))
(defn web-post-bl [config]
  (model/translate [(+ (/ (mount-width config) -2) post-adj)
                    (+ (/ (mount-height config) -2) post-adj) 0]
                   (web-post config)))
(defn web-post-br [config]
  (model/translate [(- (/ (mount-width config) 2) post-adj)
                    (+ (/ (mount-height config) -2) post-adj) 0]
                   (web-post config)))

(defn triangle-hulls [& shapes]
  (apply model/union
         (map (partial apply model/hull)
              (partition 3 1 shapes))))

(defn connectors [config]
  (let [lastrow (lastrow config)
        ncols (config :ncols)
        columns (columns config)
        cornerrow (cornerrow config)
        web-post-tl (web-post-tl config)
        web-post-tr (web-post-tr config)
        web-post-bl (web-post-bl config)
        web-post-br (web-post-br config)]
    (apply model/union
           (concat
          ;; Row connections
            (for [column (range 0 (dec ncols))
                  row (range 0 lastrow)]
              (triangle-hulls
               (key-place config (inc column) row web-post-tl)
               (key-place config column row web-post-tr)
               (key-place config (inc column) row web-post-bl)
               (key-place config column row web-post-br)))

          ;; Column connections
            (for [column columns
                  row (range 0 cornerrow)]
              (triangle-hulls
               (key-place config column row web-post-bl)
               (key-place config column row web-post-br)
               (key-place config column (inc row) web-post-tl)
               (key-place config column (inc row) web-post-tr)))

          ;; Diagonal connections
            (for [column (range 0 (dec ncols))
                  row (range 0 cornerrow)]
              (triangle-hulls
               (key-place config column row web-post-br)
               (key-place config column (inc row) web-post-tr)
               (key-place config (inc column) row web-post-bl)
               (key-place config (inc column) (inc row) web-post-tl)))))))
