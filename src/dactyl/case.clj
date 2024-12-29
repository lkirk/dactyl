(ns dactyl.case
  (:require
   [dactyl.connectors :refer
    [web-post web-post-tr web-post-tl web-post-bl web-post-br]]
   [dactyl.placement :refer
    [cornerrow lastcol lastrow key-place key-position mount-height mount-width]]
   [dactyl.thumb :refer
    [thumb-post-br thumb-post-tl thumb-mr-place thumb-ml-place thumb-tr-place
     thumb-tl-place thumb-br-place thumb-bl-place]]
   [dactyl.settings :refer
    [left-wall-x-offset left-wall-z-offset rj9-cube usb-holder-thickness
     usb-holder-size teensy-width teensy-pcb-thickness teensy-holder-width
     teensy-holder-top-length screw-insert-height screw-insert-bottom-radius
     screw-insert-top-radius wire-post-height wire-post-overhang
     wire-post-diameter]]
   [scad-clj.model :as model]))

(defn wall-locate1 [config dx dy]
  (let [wall-thickness (config :wall-thickness)]
    [(* dx wall-thickness) (* dy wall-thickness) -1]))
(defn wall-locate2 [config dx dy]
  (let [wall-xy-offset (config :wall-xy-offset)
        wall-z-offset (config :wall-z-offset)]
    [(* dx wall-xy-offset) (* dy wall-xy-offset) wall-z-offset]))
(defn wall-locate3 [config dx dy]
  (let [wall-thickness (config :wall-thickness)
        wall-xy-offset (config :wall-xy-offset)
        wall-z-offset (config :wall-z-offset)]
    [(* dx (+ wall-xy-offset wall-thickness))
     (* dy (+ wall-xy-offset wall-thickness))
     wall-z-offset]))

(defn bottom [height p]
  (->> (model/project p)
       (model/extrude-linear {:height height :twist 0 :convexity 0})
       (model/translate [0 0 (- (/ height 2) 10)])))

(defn left-key-position [config row direction]
  (map - (key-position config 0 row
                       [(* (mount-width config) -0.5)
                        (* direction (mount-height config) 0.5) 0])
       [left-wall-x-offset 0 left-wall-z-offset]))

(defn left-key-place [config row direction shape]
  (model/translate (left-key-position config row direction) shape))

(defn bottom-hull [& p]
  (model/hull p (bottom 0.001 p)))

(defn wall-brace [config place1 dx1 dy1 post1 place2 dx2 dy2 post2]
  (model/union
   (model/hull
    (place1 post1)
    (place1 (model/translate (wall-locate1 config dx1 dy1) post1))
    (place1 (model/translate (wall-locate2 config dx1 dy1) post1))
    (place1 (model/translate (wall-locate3 config dx1 dy1) post1))
    (place2 post2)
    (place2 (model/translate (wall-locate1 config dx2 dy2) post2))
    (place2 (model/translate (wall-locate2 config dx2 dy2) post2))
    (place2 (model/translate (wall-locate3 config dx2 dy2) post2)))
   (bottom-hull
    (place1 (model/translate (wall-locate2 config dx1 dy1) post1))
    (place1 (model/translate (wall-locate3 config dx1 dy1) post1))
    (place2 (model/translate (wall-locate2 config dx2 dy2) post2))
    (place2 (model/translate (wall-locate3 config dx2 dy2) post2)))))

(defn key-wall-brace [config x1 y1 dx1 dy1 post1 x2 y2 dx2 dy2 post2]
  (wall-brace config
              (partial key-place config x1 y1) dx1 dy1 post1
              (partial key-place config x2 y2) dx2 dy2 post2))

(defn case-walls [config]
  (let [ncols (config :ncols)
        cornerrow (cornerrow config)
        lastrow (lastrow config)
        lastcol (lastcol config)
        thumb-post-br (thumb-post-br config)
        thumb-post-tl (thumb-post-tl config)
        web-post (web-post config)
        web-post-tr (web-post-tr config)
        web-post-br (web-post-br config)
        web-post-tl (web-post-tl config)
        web-post-bl (web-post-bl config)
        kp (partial key-place config)
        wb (partial wall-brace config)
        kwb (partial key-wall-brace config)
        lkp (partial left-key-place config)
        t-mr-p (partial thumb-mr-place config)
        t-ml-p (partial thumb-ml-place config)
        t-tr-p (partial thumb-tr-place config)
        t-tl-p (partial thumb-tl-place config)
        t-br-p (partial thumb-br-place config)
        t-bl-p (partial thumb-bl-place config)]
    (model/union
   ; back wall
     (for [x (range 0 ncols)]
       (kwb x 0 0 1 web-post-tl x       0 0 1 web-post-tr))
     (for [x (range 1 ncols)]
       (kwb x 0 0 1 web-post-tl (dec x) 0 0 1 web-post-tr))
     (kwb lastcol 0 0 1 web-post-tr lastcol 0 1 0 web-post-tr)
   ; right wall
     (for [y (range 0 lastrow)]
       (kwb lastcol y 1 0 web-post-tr lastcol y 1 0 web-post-br))
     (for [y (range 1 lastrow)]
       (kwb lastcol (dec y) 1 0 web-post-br lastcol y 1 0 web-post-tr))
     (key-wall-brace
      config lastcol cornerrow 0 -1 web-post-br lastcol cornerrow 1 0 web-post-br)
   ; left wall
     (for [y (range 0 lastrow)]
       (model/union
        (wb (partial lkp y  1) -1 0 web-post
            (partial lkp y -1) -1 0 web-post)
        (model/hull (kp  0  y web-post-tl)
                    (kp  0  y web-post-bl)
                    (lkp y  1 web-post)
                    (lkp y -1 web-post))))
     (for [y (range 1 lastrow)]
       (model/union
        (wb (partial lkp (dec y)      -1) -1 0 web-post
            (partial lkp      y        1) -1 0 web-post)
        (model/hull (kp       0        y       web-post-tl)
                    (kp       0   (dec y)      web-post-bl)
                    (lkp      y        1       web-post)
                    (lkp (dec y)      -1       web-post))))
     (wb
      (partial kp  0 0)  0 1 web-post-tl
      (partial lkp 0 1)  0 1 web-post)
     (wb
      (partial lkp 0 1)  0 1 web-post
      (partial lkp 0 1) -1 0 web-post)
   ; front wall
     (kwb lastcol 0 0 1 web-post-tr lastcol 0 1 0 web-post-tr)
     (kwb 3 lastrow   0 -1 web-post-bl 3 lastrow 0.5 -1 web-post-br)
     (kwb 3 lastrow 0.5 -1 web-post-br 4 cornerrow 1 -1 web-post-bl)
     (for [x (range 4 ncols)]
       (kwb x cornerrow 0 -1 web-post-bl x       cornerrow 0 -1 web-post-br))
     (for [x (range 5 ncols)]
       (kwb x cornerrow 0 -1 web-post-bl (dec x) cornerrow 0 -1 web-post-br))
   ; thumb walls
     (wb t-mr-p  0 -1 web-post-br t-tr-p  0 -1 thumb-post-br)
     (wb t-mr-p  0 -1 web-post-br t-mr-p  0 -1 web-post-bl)
     (wb t-br-p  0 -1 web-post-br t-br-p  0 -1 web-post-bl)
     (wb t-ml-p -0.3  1 web-post-tr t-ml-p  0  1 web-post-tl)
     (wb t-bl-p  0  1 web-post-tr t-bl-p  0  1 web-post-tl)
     (wb t-br-p -1  0 web-post-tl t-br-p -1  0 web-post-bl)
     (wb t-bl-p -1  0 web-post-tl t-bl-p -1  0 web-post-bl)
   ; thumb corners
     (wb t-br-p -1  0 web-post-bl t-br-p  0 -1 web-post-bl)
     (wb t-bl-p -1  0 web-post-tl t-bl-p  0  1 web-post-tl)
   ; thumb tweeners
     (wb t-mr-p  0 -1 web-post-bl t-br-p  0 -1 web-post-br)
     (wb t-ml-p  0  1 web-post-tl t-bl-p  0  1 web-post-tr)
     (wb t-bl-p -1  0 web-post-bl t-br-p -1  0 web-post-tl)
     (wb t-tr-p  0 -1 thumb-post-br (partial kp 3 lastrow)  0 -1 web-post-bl)
   ; clunky bit on the top left thumb connection  (normal connectors don't work well)
     (bottom-hull
      (lkp cornerrow -1 (model/translate (wall-locate2 config -1 0) web-post))
      (lkp cornerrow -1 (model/translate (wall-locate3 config -1 0) web-post))
      (t-ml-p (model/translate (wall-locate2 config -0.3 1) web-post-tr))
      (t-ml-p (model/translate (wall-locate3 config -0.3 1) web-post-tr)))
     (model/hull
      (lkp cornerrow -1 (model/translate (wall-locate2 config -1 0) web-post))
      (lkp cornerrow -1 (model/translate (wall-locate3 config -1 0) web-post))
      (t-ml-p (model/translate (wall-locate2 config -0.3 1) web-post-tr))
      (t-ml-p (model/translate (wall-locate3 config -0.3 1) web-post-tr))
      (t-tl-p thumb-post-tl))
     (model/hull
      (lkp cornerrow -1 web-post)
      (lkp cornerrow -1 (model/translate (wall-locate1 config -1 0) web-post))
      (lkp cornerrow -1 (model/translate (wall-locate2 config -1 0) web-post))
      (lkp cornerrow -1 (model/translate (wall-locate3 config -1 0) web-post))
      (t-tl-p thumb-post-tl))
     (model/hull
      (lkp cornerrow -1 web-post)
      (lkp cornerrow -1 (model/translate (wall-locate1 config -1 0) web-post))
      (kp 0 cornerrow web-post-bl)
      (kp 0 cornerrow (model/translate (wall-locate1 config -1 0) web-post-bl))
      (t-tl-p thumb-post-tl))
     (model/hull
      (t-ml-p web-post-tr)
      (t-ml-p (model/translate (wall-locate1 config -0.3 1) web-post-tr))
      (t-ml-p (model/translate (wall-locate2 config -0.3 1) web-post-tr))
      (t-ml-p (model/translate (wall-locate3 config -0.3 1) web-post-tr))
      (t-tl-p thumb-post-tl)))))

(defn rj9-start [config]
  (map + [0 -3  0]
       (key-position config 0 0
                     (map + (wall-locate3 config 0 1)
                          [0 (/ (mount-height config) 2) 0]))))
(defn rj9-position [config]
  [(first (rj9-start config)) (second (rj9-start config)) 11])
(defn rj9-space [config]
  (model/translate (rj9-position config) rj9-cube))
(defn rj9-holder [config]
  (model/translate
   (rj9-position config)
   (model/difference
    rj9-cube
    (model/union (model/translate [0 2 0] (model/cube 10.78  9 18.38))
                 (model/translate [0 0 5] (model/cube 10.78 13  5))))))

(defn usb-holder-position [config]
  (key-position config 1 0 (map + (wall-locate2 config 0 1)
                                [0 (/ (mount-height config) 2) 0])))
(defn usb-holder [config]
  (let [usb-holder-position (usb-holder-position config)]
    (->>
     (model/cube
      (+ (first usb-holder-size) usb-holder-thickness)
      (second usb-holder-size)
      (+ (last usb-holder-size) usb-holder-thickness))
     (model/translate
      [(first usb-holder-position)
       (second usb-holder-position)
       (/ (+ (last usb-holder-size) usb-holder-thickness) 2)]))))

(defn usb-holder-hole [config]
  (let [usb-holder-position (usb-holder-position config)]
    (->> (apply model/cube usb-holder-size)
         (model/translate
          [(first usb-holder-position)
           (second usb-holder-position)
           (/ (+ (last usb-holder-size) usb-holder-thickness) 2)]))))

(defn teensy-top-xy [config]
  (key-position config 0 (- (config :centerrow) 1) (wall-locate3 config -1 0)))
(defn teensy-bot-xy [config]
  (key-position config 0 (+ (config :centerrow) 1) (wall-locate3 config -1 0)))
(defn teensy-holder-length [config]
  (- (second (teensy-top-xy config)) (second (teensy-bot-xy config))))
(defn teensy-holder-top-offset [config]
  (- (/ teensy-holder-top-length 2) (teensy-holder-length config)))
(defn teensy-holder-offset [config]
  (/ (teensy-holder-length config) -2))

(defn teensy-holder [config]
  (let
   [teensy-top-xy (teensy-top-xy config)
    teensy-holder-length (teensy-holder-length config)
    teensy-holder-top-offset (teensy-holder-top-offset config)
    teensy-holder-offset (teensy-holder-offset config)]
    (->>
     (model/union
      (->> (model/cube 3 teensy-holder-length (+ 6 teensy-width))
           (model/translate [1.5 teensy-holder-offset 0]))
      (->> (model/cube teensy-pcb-thickness teensy-holder-length 3)
           (model/translate [(+ (/ teensy-pcb-thickness 2) 3)
                             teensy-holder-offset
                             (- -1.5 (/ teensy-width 2))]))
      (->> (model/cube 4 teensy-holder-length 4)
           (model/translate [(+ teensy-pcb-thickness 5)
                             teensy-holder-offset
                             (-  -1 (/ teensy-width 2))]))
      (->> (model/cube teensy-pcb-thickness teensy-holder-top-length 3)
           (model/translate [(+ (/ teensy-pcb-thickness 2) 3)
                             teensy-holder-top-offset
                             (+ 1.5 (/ teensy-width 2))]))
      (->> (model/cube 4 teensy-holder-top-length 4)
           (model/translate [(+ teensy-pcb-thickness 5)
                             teensy-holder-top-offset
                             (+ 1 (/ teensy-width 2))])))
     (model/translate [(- teensy-holder-width) 0 0])
     (model/translate [-1.4 0 0])
     (model/translate [(first teensy-top-xy)
                       (- (second teensy-top-xy) 1)
                       (/ (+ 6 teensy-width) 2)]))))

(defn screw-insert-shape [bottom-radius top-radius height]
  (model/union (model/cylinder [bottom-radius top-radius] height)
               (model/translate [0 0 (/ height 2)] (model/sphere top-radius))))

(defn screw-insert [config column row bottom-radius top-radius height]
  (let [lastcol (lastcol config)
        lastrow (lastrow config)
        wall-locate2 (partial wall-locate2 config)
        wall-locate3 (partial wall-locate3 config)
        mount-height (mount-height config)
        mount-width (mount-width config)
        key-position (partial key-position config)
        left-key-position (partial left-key-position config)
        shift-right (= column lastcol)
        shift-left (= column 0)
        shift-up (and (not (or shift-right shift-left)) (= row 0))
        shift-down (and (not (or shift-right shift-left)) (>= row lastrow))
        position
        (if shift-up
          (key-position column row (map + (wall-locate2  0  1)
                                        [0 (/ mount-height 2) 0]))
          (if shift-down
            (key-position column row (map - (wall-locate2  0 -1)
                                          [0 (/ mount-height 2) 0]))
            (if shift-left
              (map + (left-key-position row 0) (wall-locate3 -1 0))
              (key-position column row (map + (wall-locate2  1  0)
                                            [(/ mount-width 2) 0 0])))))]
    (->> (screw-insert-shape bottom-radius top-radius height)
         (model/translate [(first position) (second position) (/ height 2)]))))

(defn screw-insert-all-shapes [config bottom-radius top-radius height]
  (let [screw-insert (partial screw-insert config)
        lastcol (lastcol config)
        lastrow (lastrow config)]
    (model/union (screw-insert 0 0 bottom-radius top-radius height)
                 (screw-insert 0 lastrow bottom-radius top-radius height)
                 (screw-insert 2 (+ lastrow 0.3)  bottom-radius top-radius height)
                 (screw-insert 3 0 bottom-radius top-radius height)
                 (screw-insert lastcol 1 bottom-radius top-radius height))))

(defn screw-insert-holes [config]
  (screw-insert-all-shapes
   config
   screw-insert-bottom-radius
   screw-insert-top-radius
   screw-insert-height))

(defn screw-insert-outers [config]
  (screw-insert-all-shapes
   config
   (+ screw-insert-bottom-radius 1.6)
   (+ screw-insert-top-radius 1.6)
   (+ screw-insert-height 1.5)))

;; (defn screw-insert-screw-holes [config]
;;   (screw-insert-all-shapes config 1.7 1.7 350))

(defn wire-post [config direction offset]
  (let [α (config :α)
        mount-height (mount-height config)]
    (->>
     (model/union
      (model/translate [0 (* wire-post-diameter -0.5 direction) 0]
                       (model/cube
                        wire-post-diameter wire-post-diameter wire-post-height))
      (model/translate [0 (* wire-post-overhang -0.5 direction)
                        (/ wire-post-height -2)]
                       (model/cube
                        wire-post-diameter wire-post-overhang wire-post-diameter)))
     (model/translate [0 (- offset) (+ (/ wire-post-height -2) 3)])
     (model/rotate (/ α -2) [1 0 0])
     (model/translate [3 (/ mount-height -2) 0]))))

(defn wire-posts [config]
  (let [wire-post (partial wire-post config)
        key-place (partial key-place config)
        thumb-ml-place (partial thumb-ml-place config)]
    (model/union
     (thumb-ml-place (model/translate [-5 0 -2] (wire-post  1 0)))
     (thumb-ml-place (model/translate [0 0 -2.5] (wire-post -1 6)))
     (thumb-ml-place (model/translate [5 0 -2] (wire-post  1 0)))
     (for [column (range 0 (lastcol config))
           row (range 0 (cornerrow config))]
       (model/union
        (key-place column row (model/translate [-5 0 0] (wire-post 1 0)))
        (key-place column row (model/translate [0 0 0] (wire-post -1 6)))
        (key-place column row (model/translate [5 0 0] (wire-post  1 0))))))))
