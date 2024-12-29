(ns dactyl.placement
  (:require
   [clojure.core.matrix :refer [mmul]]
   [dactyl.settings :refer [web-thickness]]
   [scad-clj.model :as model]))

(defn columns [config] (range 0 (config :ncols)))
(defn rows [config] (range 0 (config :nrows)))
(defn lastrow [config] (dec (config :nrows)))
(defn cornerrow [config] (dec (lastrow config)))
(defn lastcol [config] (dec (config :ncols)))

(defn mount-width  [config] (+ (config :keyswitch-width) 3))
(defn mount-height [config] (+ (config :keyswitch-height) 3))
(defn cap-top-height [config] (+ (config :plate-thickness) (config :sa-profile-key-height)))
(defn row-radius [config]
  (+ (/ (/ (+ (mount-height config) (config :extra-height)) 2)
        (Math/sin (/ (config :α) 2)))
     (cap-top-height config)))
(defn column-radius [config] (+ (/ (/ (+ (mount-width config) (config :extra-width)) 2)
                                   (Math/sin (/ (config :β) 2)))
                                (cap-top-height config)))
(defn column-x-delta [config] (+ -1 (- (* (column-radius config) (Math/sin (config :β))))))
;; (defn column-base-angle [config] (* (config :β) (- (config :centercol) 2)))
(defn column-offset [column]
  (cond
    (= column 2) [0 2.82 -4.5]
    (>= column 4) [0 -12 5.64]
    :else [0 0 0]))

(defn apply-key-geometry [config translate-fn rotate-x-fn rotate-y-fn column row shape]
  (let [β (config :β)
        α (config :α)
        centercol (config :centercol)
        centerrow (config :centerrow)
        column-style (config :column-style)
        tenting-angle (config :tenting-angle)
        keyboard-z-offset (config :keyboard-z-offset)
        row-radius (row-radius config)
        column-radius (column-radius config)
        column-x-delta (column-x-delta config)
        column-angle (* β (- centercol column))
        placed-shape
        (->> shape
             (translate-fn [0 0 (- row-radius)])
             (rotate-x-fn  (* α (- centerrow row)))
             (translate-fn [0 0 row-radius])
             (translate-fn [0 0 (- column-radius)])
             (rotate-y-fn  column-angle)
             (translate-fn [0 0 column-radius])
             (translate-fn (column-offset column)))
        column-z-delta (* column-radius (- 1 (Math/cos column-angle)))
        placed-shape-ortho
        (->> shape
             (translate-fn [0 0 (- row-radius)])
             (rotate-x-fn  (* α (- centerrow row)))
             (translate-fn [0 0 row-radius])
             (rotate-y-fn  column-angle)
             (translate-fn [(- (* (- column centercol) column-x-delta)) 0 column-z-delta])
             (translate-fn (column-offset column)))]
    (->> (case column-style
           :orthographic placed-shape-ortho
           placed-shape)
         (rotate-y-fn  tenting-angle)
         (translate-fn [0 0 keyboard-z-offset]))))

(defn key-place [config column row shape]
  (apply-key-geometry
   config
   model/translate
   (fn [angle obj] (model/rotate angle [1 0 0] obj))
   (fn [angle obj] (model/rotate angle [0 1 0] obj))
   column row shape))

(defn single-plate [config]
  (let [keyswitch-width (config :keyswitch-width)
        keyswitch-height (config :keyswitch-height)
        plate-thickness (config :plate-thickness)
        top-wall (->> (model/cube (+ keyswitch-width 3) 1.5 plate-thickness)
                      (model/translate [0
                                        (+ (/ 1.5 2) (/ keyswitch-height 2))
                                        (/ plate-thickness 2)]))
        left-wall (->> (model/cube 1.5 (+ keyswitch-height 3) plate-thickness)
                       (model/translate [(+ (/ 1.5 2) (/ keyswitch-width 2))
                                         0
                                         (/ plate-thickness 2)]))
        side-nub (->> (binding [model/*fn* 30] (model/cylinder 1 2.75))
                      (model/rotate (/ Math/PI 2) [1 0 0])
                      (model/translate [(+ (/ keyswitch-width 2)) 0 1])
                      (model/hull (->> (model/cube 1.5 2.75 plate-thickness)
                                       (model/translate [(+ (/ 1.5 2) (/ keyswitch-width 2))
                                                         0
                                                         (/ plate-thickness 2)]))))
        plate-half (model/union top-wall left-wall (model/with-fn 100 side-nub))]
    (model/union plate-half
                 (->> plate-half
                      (model/mirror [1 0 0])
                      (model/mirror [0 1 0])))))

(defn larger-plate [config]
  (let [sa-double-length (config :sa-double-length)
        plate-thickness (config :plate-thickness)
        mount-height (mount-height config)
        mount-width (mount-width config)
        plate-height (/ (- sa-double-length mount-height) 3)
        top-plate
        (->>
         (model/cube mount-width plate-height web-thickness)
         (model/translate [0 (/ (+ plate-height mount-height) 2)
                           (- plate-thickness (/ web-thickness 2))]))]
    (model/union top-plate (model/mirror [0 1 0] top-plate))))

(defn key-holes [config]
  (apply model/union
         (for [column (columns config)
               row (rows config)
               :when (or (.contains [2 3] column)
                         (not= row (lastrow config)))]
           (->> (single-plate config)
                (key-place config column row)))))

(defn rotate-around-x [angle position]
  (mmul
   [[1 0 0]
    [0 (Math/cos angle) (- (Math/sin angle))]
    [0 (Math/sin angle)    (Math/cos angle)]]
   position))

(defn rotate-around-y [angle position]
  (mmul
   [[(Math/cos angle)     0 (Math/sin angle)]
    [0                    1 0]
    [(- (Math/sin angle)) 0 (Math/cos angle)]]
   position))

(defn key-position [config column row position]
  (apply-key-geometry
   config
   (partial map +)
   rotate-around-x
   rotate-around-y
   column
   row
   position))
