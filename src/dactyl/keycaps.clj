(ns dactyl.keycaps
  (:require
   [dactyl.placement :refer [rows columns key-place lastrow]]
   [dactyl.settings :refer [sa-double-length]]
   [dactyl.thumb :refer [thumb-1x-layout thumb-15x-layout]]
   [scad-clj.model :as model]))

(defn sa-cap [config]
  {1 (let [bl2 (/ 18.5 2)
           m (/ 17 2)
           key-cap
           (model/hull
            (->> (model/polygon [[bl2 bl2] [bl2 (- bl2)] [(- bl2) (- bl2)] [(- bl2) bl2]])
                 (model/extrude-linear {:height 0.1 :twist 0 :convexity 0})
                 (model/translate [0 0 0.05]))
            (->> (model/polygon [[m m] [m (- m)] [(- m) (- m)] [(- m) m]])
                 (model/extrude-linear {:height 0.1 :twist 0 :convexity 0})
                 (model/translate [0 0 6]))
            (->> (model/polygon [[6 6] [6 -6] [-6 -6] [-6 6]])
                 (model/extrude-linear {:height 0.1 :twist 0 :convexity 0})
                 (model/translate [0 0 12])))]
       (->> key-cap
            (model/translate [0 0 (+ 5 (config :plate-thickness))])
            (model/color [220/255 163/255 163/255 1])))
   2 (let [bl2 (/ sa-double-length 2)
           bw2 (/ 18.25 2)
           key-cap
           (model/hull
            (->> (model/polygon [[bw2 bl2] [bw2 (- bl2)] [(- bw2) (- bl2)] [(- bw2) bl2]])
                 (model/extrude-linear {:height 0.1 :twist 0 :convexity 0})
                 (model/translate [0 0 0.05]))
            (->> (model/polygon [[6 16] [6 -16] [-6 -16] [-6 16]])
                 (model/extrude-linear {:height 0.1 :twist 0 :convexity 0})
                 (model/translate [0 0 12])))]
       (->> key-cap
            (model/translate [0 0 (+ 5 (config :plate-thickness))])
            (model/color [127/255 159/255 127/255 1])))
   1.5 (let [bl2 (/ 18.25 2)
             bw2 (/ 28 2)
             key-cap
             (model/hull
              (->> (model/polygon [[bw2 bl2] [bw2 (- bl2)] [(- bw2) (- bl2)] [(- bw2) bl2]])
                   (model/extrude-linear {:height 0.1 :twist 0 :convexity 0})
                   (model/translate [0 0 0.05]))
              (->> (model/polygon [[11 6] [-11 6] [-11 -6] [11 -6]])
                   (model/extrude-linear {:height 0.1 :twist 0 :convexity 0})
                   (model/translate [0 0 12])))]
         (->> key-cap
              (model/translate [0 0 (+ 5 (config :plate-thickness))])
              (model/color [240/255 223/255 175/255 1])))})

(defn thumbcaps [config]
  (let [thumb-1x-layout (partial thumb-1x-layout config)
        thumb-15x-layout (partial thumb-15x-layout config)]
    (model/union
     (thumb-1x-layout ((sa-cap config) 1))
     (thumb-15x-layout (model/rotate (/ Math/PI 2) [0 0 1] ((sa-cap config) 1.5))))))

(defn caps [config]
  (apply model/union
         (for [column (columns config)
               row (rows config)
               :when (or (.contains [2 3] column)
                         (not= row (lastrow config)))]
           (->> ((sa-cap config) (if (= column 5) 1 1))
                (key-place config column row)))))
