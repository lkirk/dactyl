(ns dactyl.convert
  (:require
   [clojure.java.shell :refer [sh]]
   [clojure.string :as string]))

(def formats
  (hash-set :stl :off :amf :3mf :csg :dxf :svg
            :pdf :png :echo :ast :term :nef3 :nefdbge))

(defn change-ext [f fmt]
  (string/replace (str f) #".scad$" (str "." (name fmt))))

(defn convert-scad [in format]
  (when (not (contains? formats format))
    (throw (ex-info "Incorrect file format" {:format format})))
  (sh "openscad" "-o" (change-ext in format) (str in)))

(defn convert-scads [files format]
  (let [futs (map #(future (convert-scad % format)) files)
        result (pmap deref futs)]
    (doseq [r result]
      (when (not (= 0 (:exit r)))
        (throw (ex-info "Conversion failed"
                        {:msg (last (string/split (:err r) #"\n"))
                         :exit (:exit r)}))) result)))
