(ns dactyl.config
  (:require
   [clojure.edn :as edn]
   [clojure.spec.alpha :as spec]))

;; Config spec
(spec/def :dactyl/nrows pos-int?)
(spec/def :dactyl/ncols pos-int?)
(spec/def :dactyl/centercol pos-int?)
(spec/def :dactyl/α number?)
(spec/def :dactyl/β number?)
(spec/def :dactyl/centerrow number?)
(spec/def :dactyl/tenting-angle number?)
(spec/def :dactyl/column-style #{:orthographic :standard})
(spec/def :dactyl/thumb-offsets (spec/coll-of number? :kind vector? :count 3))
(spec/def :dactyl/keyboard-z-offset number?)
(spec/def :dactyl/extra-width number?)
(spec/def :dactyl/extra-height number?)
(spec/def :dactyl/wall-z-offset number?)
(spec/def :dactyl/wall-xy-offset number?)
(spec/def :dactyl/wall-thickness number?)
(spec/def :dactyl/keyswitch-height number?)
(spec/def :dactyl/keyswitch-width number?)
(spec/def :dactyl/sa-profile-key-height number?)
(spec/def :dactyl/sa-double-length number?)
(spec/def :dactyl/plate-thickness number?)
(spec/def :dactyl/chip-holder boolean?)
(spec/def :dactyl/config
  (spec/keys
   :req
   [:dactyl/nrows :dactyl/ncols :dactyl/centercol :dactyl/α :dactyl/β
    :dactyl/centerrow :dactyl/tenting-angle :dactyl/column-style
    :dactyl/thumb-offsets :dactyl/keyboard-z-offset :dactyl/extra-width
    :dactyl/extra-height :dactyl/wall-z-offset :dactyl/wall-xy-offset
    :dactyl/wall-thickness :dactyl/keyswitch-height :dactyl/keyswitch-width
    :dactyl/sa-profile-key-height :dactyl/sa-double-length
    :dactyl/plate-thickness :dactyl/chip-holder]))
(defn defaults [nrows]
  {:dactyl/α (/ Math/PI 12) ; curvature of the columns
   :dactyl/β (/ Math/PI 36) ; curvature of the rows
   :dactyl/centerrow (- nrows 3)  ; controls front-back tilt
   :dactyl/tenting-angle (/ Math/PI 12) ; or, change this for more precise tenting control
   :dactyl/column-style (if (> nrows 5) :orthographic :standard)
   :dactyl/thumb-offsets [6 -3 7]
   :dactyl/keyboard-z-offset 9 ; controls overall height; original=9 with centercol=3; use 16 for centercol=2
   :dactyl/extra-width 2.5     ; extra space between the base of keys; original= 2
   :dactyl/extra-height 1.0    ; ??
   :dactyl/wall-z-offset -15   ; length of the first downward-sloping part of the wall (negative)
   :dactyl/wall-xy-offset 5    ; offset in x and/or y for first downsloping part of wall (neg)
   :dactyl/wall-thickness 2    ; wall thickness parameter
   :dactyl/keyswitch-height 14.4
   :dactyl/keyswitch-width 14.4
   :dactyl/sa-profile-key-height 12.7
   :dactyl/sa-double-length 37.5
   :dactyl/plate-thickness 4
   :dactyl/chip-holder true})

(defn validate-and-apply-defaults [config]
  (let [merged (merge (defaults (config :dactyl/nrows)) config)]
    (if (not (spec/valid? :dactyl/config merged))
      (do (println
           "Invalid configuration" (spec/explain-str :dactyl/config merged))
          nil)
      (update-keys merged (fn [k] (-> k name keyword)))))) ;; strip namespace

(defn config-from-file [path]
  (let [config (-> path slurp edn/read-string)]
    (validate-and-apply-defaults config)))

(defn config-from-string [s]
  (let [config (edn/read-string s)]
    (validate-and-apply-defaults config)))
